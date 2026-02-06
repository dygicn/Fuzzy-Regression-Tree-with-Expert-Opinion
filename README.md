library(data.table)
library(rpart)
library(party)
library(Cubist)
library(Metrics)
library(data.table)

# Read the file (adjust the file path as needed)
library(readxl)
data <- as.data.table(read_excel("data.xlsx"))

# Create expert_TFN character column
data[, expert_TFN := paste0("(",
                            as.integer(Expert_Lower), ", ",
                            as.integer(Expert_Center), ", ",
                            as.integer(Expert_Upper), ")")]

data$Expert_Lower <- NULL
data$Expert_Center <- NULL
data$Expert_Upper <- NULL

fuzzify_crisp <- function(x, delta_percent = 0.05) {
  # x: crisp (real) value
  # delta_percent: percentage uncertainty, e.g., 0.05 = 5%
  delta <- x * delta_percent
  return(c(x - delta, x, x + delta))
}

# Create TFN vector for each Y_crisp
fuzzy_list <- lapply(data$Y_crisp, fuzzify_crisp, delta_percent = 0.05)

# Convert the list into a dataframe format
fuzzy_df <- do.call(rbind, fuzzy_list)

# Assign new column names
colnames(fuzzy_df) <- c("Y_TFN_a", "Y_TFN_b", "Y_TFN_c")

# Add to the original dataframe
data <- cbind(data, fuzzy_df)

# Check head(data)

library(stringr)

parse_tfn <- function(str) {
  nums <- str_extract_all(str, "[0-9.]+")[[1]]
  return(as.numeric(nums))
}

data$expert_TFN_parsed <- lapply(data$expert_TFN, parse_tfn)

# TFN Addition
tfn_add <- function(A, B) {
  return(c(A[1] + B[1], A[2] + B[2], A[3] + B[3]))
}

# TFN Subtraction
tfn_subtract <- function(A, B) {
  # A - B = (a1 - b3, a2 - b2, a3 - b1)
  return(c(A[1] - B[3], A[2] - B[2], A[3] - B[1]))
}

# TFN Multiplication
tfn_multiply <- function(A, B) {
  # Min and max are determined from the products of the extreme values
  products <- c(
    A[1] * B[1],
    A[1] * B[3],
    A[3] * B[1],
    A[3] * B[3]
  )
  a <- min(products)
  b <- A[2] * B[2]
  c <- max(products)
  return(c(a, b, c))
}

# TFN Division
tfn_divide <- function(A, B) {
  # Components of B must not be zero (check required)
  if(any(B == 0)) stop("Denominator TFN components must not be zero!")
  
  quotients <- c(
    A[1] / B[1],
    A[1] / B[3],
    A[3] / B[1],
    A[3] / B[3]
  )
  a <- min(quotients)
  b <- A[2] / B[2]
  c <- max(quotients)
  return(c(a, b, c))
}

# TFN scalar multiplication (scalar * TFN)
tfn_scalar_multiply <- function(scalar, A) {
  products <- scalar * A
  # If the scalar is negative, the TFN order may reverse
  if(scalar >= 0) {
    return(products)
  } else {
    # For negative scalar, min and max swap
    return(c(products[3], products[2], products[1]))
  }
}

# Sum of a list of TFNs
tfn_sum <- function(TFNs) {
  a_sum <- sum(sapply(TFNs, function(x) x[1]))
  b_sum <- sum(sapply(TFNs, function(x) x[2]))
  c_sum <- sum(sapply(TFNs, function(x) x[3]))
  return(c(a_sum, b_sum, c_sum))
}

# Mean of a list of TFNs
tfn_mean <- function(TFNs) {
  n <- length(TFNs)
  s <- tfn_sum(TFNs)
  return(c(s[1]/n, s[2]/n, s[3]/n))
}

tfn_square <- function(A) {
  return(c(A[1]^2, A[2]^2, A[3]^2))
}

calculate_sse_tfn <- function(TFNs) {
  n <- length(TFNs)
  mean_tfn <- tfn_mean(TFNs)
  
  squared_diffs <- lapply(TFNs, function(y) {
    diff <- tfn_subtract(y, mean_tfn)
    sq_diff <- tfn_square(diff)   # Use tfn_square here
    return(sq_diff)
  })
  
  sum_sq_diffs <- tfn_sum(squared_diffs)
  sse_tfn <- tfn_scalar_multiply(1 / (n - 1), sum_sq_diffs)
  return(sse_tfn)
}

gmir_distance <- function(A, B) {
  P_A <- (A[1] + 4 * A[2] + A[3]) / 6
  P_B <- (B[1] + 4 * B[2] + B[3]) / 6
  return(abs(P_A - P_B))
}

calculate_lambda_t <- function(Y_prev, U_prev) {
  diff <- abs(tfn_subtract(Y_prev, U_prev))
  one_plus_diff <- tfn_add(c(1,1,1), diff)
  inv_one_plus_diff <- tfn_divide(c(1,1,1), one_plus_diff)
  lambda_t <- tfn_subtract(c(1,1,1), inv_one_plus_diff)
  return(lambda_t)
}

# Split score calculation function
calculate_split_score <- function(SSE_TFN, lambda_TFN, d_s) {
  weighted_lambda <- tfn_scalar_multiply(d_s, lambda_TFN)
  split_score_tfn <- tfn_add(SSE_TFN, weighted_lambda)
  defuzzified_score <- (split_score_tfn[1] + 4 * split_score_tfn[2] + split_score_tfn[3]) / 6
  
  if (is.na(defuzzified_score) || is.nan(defuzzified_score)) {
    return(Inf)  # Return infinite score for invalid values
  }
  
  return(defuzzified_score)
}

# find_best_split function
find_best_split <- function(data, features, expert_TFN, prev_year_real_TFN, prev_year_expert_TFN) {
  
  best_score <- Inf
  best_feature <- NULL
  best_threshold <- NULL
  
  # Calculate expert weighting
  lambda_t <- calculate_lambda_t(prev_year_real_TFN, prev_year_expert_TFN)
  # print(paste("Lambda TFN:", paste(round(lambda_t, 2), collapse = ", ")))
  
  for (feature in features) {
    values <- sort(unique(data[[feature]]))
    thresholds <- (values[-1] + values[-length(values)]) / 2
    
    for (threshold in thresholds) {
      left_idx <- which(data[[feature]] <= threshold)
      right_idx <- which(data[[feature]] > threshold)
      
      if(length(left_idx) == 0 || length(right_idx) == 0) next
      
      left_TFNs <- lapply(left_idx, function(i) data$Y_TFN[[i]])
      right_TFNs <- lapply(right_idx, function(i) data$Y_TFN[[i]])
      
      sse_left <- calculate_sse_tfn(left_TFNs)
      sse_right <- calculate_sse_tfn(right_TFNs)
      
      n_left <- length(left_idx)
      n_right <- length(right_idx)
      n_total <- n_left + n_right
      
      weighted_sse <- tfn_add(
        tfn_scalar_multiply(n_left / n_total, sse_left),
        tfn_scalar_multiply(n_right / n_total, sse_right)
      )
      
      mean_left <- tfn_mean(left_TFNs)
      mean_right <- tfn_mean(right_TFNs)
      mean_group <- tfn_add(
        tfn_scalar_multiply(n_left / n_total, mean_left),
        tfn_scalar_multiply(n_right / n_total, mean_right)
      )
      
      d_s <- gmir_distance(mean_group, expert_TFN)
      
      score <- calculate_split_score(weighted_sse, lambda_t, d_s)
      
      if (is.na(score) || is.nan(score)) {
        next
      }
      
      if(score < best_score) {
        best_score <- score
        best_feature <- feature
        best_threshold <- threshold
      }
    }
  }
  
  return(list(feature = best_feature, threshold = best_threshold, score = best_score))
}

data$Y_TFN <- lapply(1:nrow(data), function(i) {
  c(data$Y_TFN_a[i], data$Y_TFN_b[i], data$Y_TFN_c[i])
})

build_fuzzy_regression_tree <- function(data, features, expert_TFN, prev_year_real_TFN, prev_year_expert_TFN, max_depth = 3, min_samples_split = 2, current_depth = 0) {
  
  # Stopping conditions
  if (current_depth >= max_depth || nrow(data) < min_samples_split) {
    # Leaf node: return mean TFN of target values
    leaf_value <- tfn_mean(data$Y_TFN)
    return(list(
      is_leaf = TRUE,
      prediction = leaf_value,
      samples = nrow(data),
      depth = current_depth
    ))
  }
  
  # Find the best split
  best_split <- find_best_split(data, features, expert_TFN, prev_year_real_TFN, prev_year_expert_TFN)
  
  if (is.null(best_split$feature)) {
    # If no split is found, create a leaf node
    leaf_value <- tfn_mean(data$Y_TFN)
    return(list(
      is_leaf = TRUE,
      prediction = leaf_value,
      samples = nrow(data),
      depth = current_depth
    ))
  }
  
  # Split information
  split_feature <- best_split$feature
  split_threshold <- best_split$threshold
  
  # Split the data into two parts
  left_data <- data[data[[split_feature]] <= split_threshold, ]
  right_data <- data[data[[split_feature]] > split_threshold, ]
  
  # Recursive call — previous year TFNs can be updated here
  # For now, the same values are passed
  
  left_subtree <- build_fuzzy_regression_tree(left_data, features, expert_TFN, prev_year_real_TFN, prev_year_expert_TFN,
                                              max_depth, min_samples_split, current_depth + 1)
  right_subtree <- build_fuzzy_regression_tree(right_data, features, expert_TFN, prev_year_real_TFN, prev_year_expert_TFN,
                                               max_depth, min_samples_split, current_depth + 1)
  
  # Node structure
  return(list(
    is_leaf = FALSE,
    feature = split_feature,
    threshold = split_threshold,
    left = left_subtree,
    right = right_subtree,
    samples = nrow(data),
    depth = current_depth
  ))
}

# Previous year real TFN (example: use the first row)
prev_year_real_TFN <- c(data$Y_TFN_a[1], data$Y_TFN_b[1], data$Y_TFN_c[1])

# Previous year expert TFN (example: parsed TFN from the first row)
prev_year_expert_TFN <- data$expert_TFN_parsed[[1]]

# Expert TFN for the current year (example: from the first row)
expert_TFN <- data$expert_TFN_parsed[[1]]

# Specify features (example)
features <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")

# Maximum depth and minimum number of samples
max_depth <- 5
min_samples_split <- 10

# Build the tree
fuzzy_tree <- build_fuzzy_regression_tree(data, features, expert_TFN, prev_year_real_TFN, prev_year_expert_TFN,
                                          max_depth, min_samples_split)

# Print the tree
# print(fuzzy_tree)

print_fuzzy_tree <- function(tree, indent = "") {
  if (tree$is_leaf) {
    cat(indent, "Leaf: Prediction TFN = (", paste(round(tree$prediction, 2), collapse = ", "), "), Samples =", tree$samples, "\n")
  } else {
    cat(indent, "Node: Feature =", tree$feature, ", Threshold =", tree$threshold, ", Samples =", tree$samples, "\n")
    cat(indent, " Left:\n")
    print_fuzzy_tree(tree$left, paste0(indent, "  "))
    cat(indent, " Right:\n")
    print_fuzzy_tree(tree$right, paste0(indent, "  "))
  }
}

print_fuzzy_tree(fuzzy_tree)

library(DiagrammeR)

tree_to_graph <- function(tree, graph = NULL, parent = NULL, edge_label = NULL) {
  if (is.null(graph)) {
    graph <- create_graph()
  }
  
  # Node label
  if (tree$is_leaf) {
    label <- paste0("Leaf\nTFN:\n(", paste(round(tree$prediction, 1), collapse = ", "), ")\nSamples: ", tree$samples)
  } else {
    label <- paste0("Node\n", tree$feature, " <= ", round(tree$threshold, 3), "\nSamples: ", tree$samples)
  }
  
  # Add node
  graph <- add_node(graph)
  new_node_id <- tail(get_node_ids(graph), 1)
  
  # Set label and font size
  graph <- set_node_attrs(graph, node_attr = "label", values = label, nodes = new_node_id)
  graph <- set_node_attrs(graph, node_attr = "fontsize", values = 5, nodes = new_node_id)
  
  # Add edge (penwidth is provided via edge_aes)
  if (!is.null(parent)) {
    graph <- add_edge(graph, from = parent, to = new_node_id, rel = edge_label, edge_aes = edge_aes(penwidth = 1))
  }
  
  # Children
  if (!tree$is_leaf) {
    graph <- tree_to_graph(tree$left, graph, new_node_id, "Yes")
    graph <- tree_to_graph(tree$right, graph, new_node_id, "No")
  }
  
  return(graph)
}

# Usage
gr <- tree_to_graph(fuzzy_tree)
render_graph(gr, layout = "tree")

head(data)

# Prediction function for a single observation (internal use)
predict_fuzzy_tree <- function(tree, observation) {
  if (tree$is_leaf) {
    return(tree$prediction)
  }
  
  feature_value <- observation[[tree$feature]]
  
  if (feature_value <= tree$threshold) {
    return(predict_fuzzy_tree(tree$left, observation))
  } else {
    return(predict_fuzzy_tree(tree$right, observation))
  }
}

# Prediction function for multiple observations
predict_fuzzy_tree_dataset <- function(tree, data) {
  preds <- lapply(1:nrow(data), function(i) {
    obs <- data[i, ]   # Take a single row
    predict_fuzzy_tree(tree, obs)
  })
  return(preds)  # Returns TFN predictions as a list
}

# Get predictions for multiple observations
# 1. Obtain predictions
tahminler <- predict_fuzzy_tree_dataset(fuzzy_tree, data)

# 2. Convert list of predictions to matrix
tahmin_mat <- do.call(rbind, tahminler)
colnames(tahmin_mat) <- c("Pred_TFN_a", "Pred_TFN_b", "Pred_TFN_c")

# 3. Add here (remove old columns if they exist and add new ones)
if ("data.table" %in% class(data)) {
  cols_to_remove <- c("Pred_TFN_a", "Pred_TFN_b", "Pred_TFN_c")
  existing_cols <- cols_to_remove[cols_to_remove %in% colnames(data)]
  if (length(existing_cols) > 0) {
    data[, (existing_cols) := NULL]
  }
  data <- cbind(data, as.data.table(tahmin_mat))
} else {
  cols_to_remove <- c("Pred_TFN_a", "Pred_TFN_b", "Pred_TFN_c")
  existing_cols <- cols_to_remove[cols_to_remove %in% colnames(data)]
  if (length(existing_cols) > 0) {
    data[, existing_cols] <- NULL
  }
  data <- cbind(data, tahmin_mat)
}

# 4. Check head(data)

# Create crisp predictions using Center of Gravity
data$Pred_Crisp <- (data$Pred_TFN_a + 4 * data$Pred_TFN_b + data$Pred_TFN_c) / 6

# Check head(data)

# Mean Squared Error (MSE)
mse <- mean((data$Y_crisp - data$Pred_Crisp)^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((data$Y_crisp - data$Pred_Crisp)^2))

# Mean Absolute Error (MAE)
mae <- mean(abs(data$Y_crisp - data$Pred_Crisp))

# Print results
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

### Comparison with other methods

# Required libraries
library(rpart)
library(party)
library(Cubist)
library(Metrics)
library(data.table)

# Train-test split (70% / 30%)
set.seed(123)
n <- nrow(data)
train_idx <- sample(1:n, size = 0.7 * n)
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# ---------------------
# 1. CART (rpart)
cart_model <- rpart(Y_crisp ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data = train_data)
cart_pred_train <- predict(cart_model, newdata = train_data)
cart_pred_test <- predict(cart_model, newdata = test_data)

# ---------------------
# 2. GUIDE-like model (ctree)
guide_model <- ctree(Y_crisp ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data = train_data)
guide_pred_train <- predict(guide_model, newdata = train_data)
guide_pred_test <- predict(guide_model, newdata = test_data)

# ---------------------
# 3. Cubist
cubist_model <- cubist(x = train_data[, .(X1, X2, X3, X4 , X5 , X6 , X7 , X8 , X9)], y = train_data$Y_crisp)
cubist_pred_train <- predict(cubist_model, newdata = train_data)
cubist_pred_test <- predict(cubist_model, newdata = test_data)

# ---------------------
# 4. Fuzzy Tree – Crisp predictions
# Fuzzy predictions for the full dataset are assumed to be already available
fuzzy_pred_crisp_train <- data$Pred_Crisp[train_idx]
fuzzy_pred_crisp_test <- data$Pred_Crisp[-train_idx]

true_y_train <- train_data$Y_crisp
true_y_test <- test_data$Y_crisp

# ---------------------
# 5. Compute MAE – RMSE – MSE (Train & Test)
results <- data.table(
  Model = rep(c("Fuzzy Tree", "CART", "GUIDE", "Cubist"), each = 2),
  Dataset = rep(c("Train", "Test"), times = 4),
  MAE = c(
    mae(true_y_train, fuzzy_pred_crisp_train),
    mae(true_y_test, fuzzy_pred_crisp_test),
    
    mae(true_y_train, cart_pred_train),
    mae(true_y_test, cart_pred_test),
    
    mae(true_y_train, guide_pred_train),
    mae(true_y_test, guide_pred_test),
    
    mae(true_y_train, cubist_pred_train),
    mae(true_y_test, cubist_pred_test)
  ),
  RMSE = c(
    rmse(true_y_train, fuzzy_pred_crisp_train),
    rmse(true_y_test, fuzzy_pred_crisp_test),
    
    rmse(true_y_train, cart_pred_train),
    rmse(true_y_test, cart_pred_test),
    
    rmse(true_y_train, guide_pred_train),
    rmse(true_y_test, guide_pred_test),
    
    rmse(true_y_train, cubist_pred_train),
    rmse(true_y_test, cubist_pred_test)
  ),
  MSE = c(
    mse(true_y_train, fuzzy_pred_crisp_train),
    mse(true_y_test, fuzzy_pred_crisp_test),
    
    mse(true_y_train, cart_pred_train),
    mse(true_y_test, cart_pred_test),
    
    mse(true_y_train, guide_pred_train),
    mse(true_y_test, guide_pred_test),
    
    mse(true_y_train, cubist_pred_train),
    mse(true_y_test, cubist_pred_test)
  )
)

# ---------------------
# 6. Print results
print(results)

# Required libraries
library(ggplot2)
library(reshape2)

# Convert data to long format
results_long <- melt(results,
                     id.vars = c("Model", "Dataset"),
                     measure.vars = c("MAE", "RMSE", "MSE"),
                     variable.name = "Metric",
                     value.name = "Error")

# Show all metrics in a single plot using facets
ggplot(results_long, aes(x = Model, y = Error, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "Model Comparison - Train and Test Errors",
       y = "Error Value", x = "") +
  theme(legend.position = "top",
        strip.text = element_text(size = 12, face = "bold"))

# Putting all graphs in one vertical plot
library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming your original 'results' data frame looks like this:
# Model, Dataset, MAE, RMSE, MSE

results_long <- results %>%
  pivot_longer(cols = c(MAE, RMSE, MSE),
               names_to = "Metric",
               values_to = "Value")

print(results_long, n = 24)

# Save as Excel file
library(writexl)
write_xlsx(results_long, path = "results_long_paper_3_realdata.xlsx")

ggplot(results_long, aes(x = Model, y = Value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(Value, 1)),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3.5) +  # Push labels outside the bars
  facet_wrap(~ Metric, scales = "free_x") +
  coord_flip() +
  labs(x = "Model", y = "Error Value") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom")

ggplot(results_long, aes(x = Model, y = Value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(Value, 1)),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3.5) +  # Place labels outside the bars
  facet_wrap(~ Metric, scales = "free_x") +
  coord_flip() +
  labs(x = "Model", y = "Error") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) + # Add space on the right
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 10),
    axis.text.x = element_blank(),      # Hide x-axis values
    axis.ticks.x = element_blank(),     # Hide x-axis ticks
    legend.position = "bottom",
    legend.title = element_blank()
  )

library(ggplot2)

# Plot using your pivoted data
p <- ggplot(results_long, aes(x = Model, y = Value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Value, 1)),
            position = position_dodge(width = 0.8),
            hjust = -0.15, size = 3.5) +
  facet_wrap(~ Metric, scales = "free_x") +
  coord_flip() +
  labs(title = NULL,
       x = "Model", y = "Error") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +  # Leave room for text
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Print to RStudio Viewer
print(p)

# Save as high-resolution image for publication
ggsave("model_comparison_errors.png", p, width = 12, height = 6, dpi = 400)
ggsave("model_comparison_errors.pdf", p, width = 12, height = 6)

