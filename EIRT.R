# Expert-Informed Regression Tree (EIRT)
# Duygu Icen -- implementation accompanying
# "Expert-Informed Regression Tree Using Triangular Fuzzy Numbers".
# Base R only. Sourcing this file does not fit models or write files.

.eirt_matrix <- function(x, label, tfn = FALSE, allow_empty = FALSE) {
  if (!(is.matrix(x) || is.data.frame(x)))
    stop(label, " must be a numeric matrix or data.frame.")
  if (is.data.frame(x) && !all(vapply(x, is.numeric, logical(1))))
    stop(label, " must contain numeric columns only.")
  x <- as.matrix(x)
  if (!is.numeric(x) || ncol(x) < 1L ||
      (!allow_empty && nrow(x) < 1L) || any(!is.finite(x)))
    stop(label, " must contain finite numeric values (no NA/Inf).")
  if (tfn && (ncol(x) != 3L || any(x[, 1] > x[, 2]) ||
              any(x[, 2] > x[, 3])))
    stop(label, " must have three ordered columns: lower <= vertex <= upper.")
  x
}

.eirt_scalar <- function(x, label, minimum, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < minimum || (integer && x != floor(x)))
    stop(label, " must be a finite ", if (integer) "integer " else "number ",
         ">= ", minimum, ".")
}

# Mean of the sums of three squared component errors; no division by three.
tfn_loss <- function(observed, predicted) {
  observed <- .eirt_matrix(observed, "observed", tfn = TRUE)
  predicted <- .eirt_matrix(predicted, "predicted", tfn = TRUE)
  if (!identical(dim(observed), dim(predicted)))
    stop("observed and predicted must have the same dimensions.")
  mean(rowSums((observed - predicted)^2))
}

# Numeric predictors only. Expert assessments are observation-specific and
# aligned with the rows of X and Y. Set use_expert=FALSE for TFN-RT.
fit_eirt <- function(X, Y, E = NULL, use_expert = TRUE,
                     max_depth = 3L, min_samples_split = 6L,
                     min_samples_leaf = 2L, epsilon = 1e-12, eta = 1e-8) {
  X <- .eirt_matrix(X, "X")
  Y <- .eirt_matrix(Y, "Y", tfn = TRUE)
  if (nrow(X) != nrow(Y)) stop("X and Y must have the same number of rows.")
  if (!is.logical(use_expert) || length(use_expert) != 1L || is.na(use_expert))
    stop("use_expert must be TRUE or FALSE.")
  if (use_expert && is.null(E)) stop("E is required when use_expert=TRUE.")
  if (!is.null(E)) {
    E <- .eirt_matrix(E, "E", tfn = TRUE)
    if (nrow(E) != nrow(Y)) stop("E and Y must have the same number of rows.")
  }
  if (!use_expert) E <- NULL
  .eirt_scalar(max_depth, "max_depth", 0, TRUE)
  .eirt_scalar(min_samples_split, "min_samples_split", 2, TRUE)
  .eirt_scalar(min_samples_leaf, "min_samples_leaf", 1, TRUE)
  .eirt_scalar(epsilon, "epsilon", 0)
  .eirt_scalar(eta, "eta", 0)
  feature_names <- colnames(X)
  if (!is.null(feature_names) &&
      (anyNA(feature_names) || any(feature_names == "") || anyDuplicated(feature_names)))
    stop("X column names must be unique and nonempty.")

  loss <- function(a, b) mean(rowSums((a - b)^2))
  build <- function(index, depth) {
    y <- Y[index, , drop = FALSE]
    e <- if (is.null(E)) NULL else E[index, , drop = FALSE]
    n <- length(index)
    prediction <- colSums(y) / n
    node <- list(type = "leaf", n = n, depth = depth, prediction = prediction)
    # Stop before LOO: a singleton never requires an n-1 denominator.
    if (depth >= max_depth || n < min_samples_split) return(node)
    totals <- colSums(y)
    loo <- t(vapply(seq_len(n), function(i) (totals - y[i, ]) / (n - 1), numeric(3)))
    d_reference <- loss(y, loo)
    d_expert <- if (is.null(e)) NA_real_ else loss(y, e)
    w <- if (is.null(e) || d_reference <= epsilon) 0 else
      max(0, 1 - d_expert / d_reference)
    if (!is.finite(d_reference) || (!is.null(e) && !is.finite(d_expert)))
      stop("Non-finite loss: rescale the response/expert values.")
    score <- function(pred) {
      lt <- loss(y, pred)
      le <- if (is.null(e)) 0 else loss(e, pred)
      result <- (1 - w) * lt + w * le
      if (!is.finite(result)) stop("Non-finite split score: rescale the inputs.")
      result
    }
    parent_score <- score(matrix(prediction, n, 3L, byrow = TRUE))
    node$weight <- w
    node$reference_loss <- d_reference
    node$expert_loss <- d_expert
    node$unsplit_score <- parent_score
    best_score <- Inf
    best <- NULL
    for (j in seq_len(ncol(X))) {
      values <- sort(unique(X[index, j]))
      if (length(values) < 2L) next
      # Halving before adding avoids overflow for very large predictors.
      thresholds <- head(values, -1L) / 2 + tail(values, -1L) / 2
      for (theta in thresholds) {
        left <- X[index, j] <= theta
        if (sum(left) < min_samples_leaf || sum(!left) < min_samples_leaf) next
        pred <- matrix(NA_real_, n, 3L)
        pred[left, ] <- matrix(colSums(y[left, , drop = FALSE]) / sum(left),
                              sum(left), 3L, byrow = TRUE)
        pred[!left, ] <- matrix(colSums(y[!left, , drop = FALSE]) / sum(!left),
                               sum(!left), 3L, byrow = TRUE)
        q <- score(pred)
        # Exact ties retain the first predictor/midpoint encountered.
        if (q < best_score) {
          best_score <- q
          best <- list(feature = j, threshold = theta, left = left)
        }
      }
    }
    if (is.null(best) || parent_score - best_score <= eta) return(node)
    node$type <- "node"
    node$feature <- best$feature
    node$threshold <- best$threshold
    node$score <- best_score
    node$left <- build(index[best$left], depth + 1L)
    node$right <- build(index[!best$left], depth + 1L)
    node
  }
  structure(list(tree = build(seq_len(nrow(X)), 0L),
                 feature_names = feature_names, n_features = ncol(X),
                 use_expert = use_expert,
                 control = list(max_depth = max_depth,
                                min_samples_split = min_samples_split,
                                min_samples_leaf = min_samples_leaf,
                                epsilon = epsilon, eta = eta)), class = "eirt")
}

# No expert assessments at prediction time. A named training matrix requires
# named prediction columns; reorder them explicitly to the training order.
predict.eirt <- function(object, newdata, type = c("tfn", "vertex"), ...) {
  type <- match.arg(type)
  x <- .eirt_matrix(newdata, "newdata", allow_empty = TRUE)
  if (ncol(x) != object$n_features) stop("newdata has the wrong number of predictors.")
  if (!is.null(object$feature_names)) {
    if (is.null(colnames(x)) || anyDuplicated(colnames(x)) ||
        !setequal(colnames(x), object$feature_names))
      stop("newdata column names must match the training predictors.")
    x <- x[, object$feature_names, drop = FALSE]
  }
  result <- matrix(NA_real_, nrow(x), 3L,
                   dimnames = list(NULL, c("lower", "vertex", "upper")))
  for (i in seq_len(nrow(x))) {
    node <- object$tree
    while (node$type == "node") {
      node <- if (x[i, node$feature] <= node$threshold) node$left else node$right
    }
    result[i, ] <- node$prediction
  }
  if (type == "vertex") result[, 2L] else result
}
