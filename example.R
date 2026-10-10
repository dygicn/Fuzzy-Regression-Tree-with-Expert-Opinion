# Run from this directory: Rscript example.R
source("EIRT.R")
set.seed(20261010)
make_data <- function(n, expert = FALSE) {
  X <- matrix(runif(n * 4), ncol = 4, dimnames = list(NULL, paste0("X", 1:4)))
  mu <- 10 + 6 * (X[, 1] > .35) + 8 * (X[, 1] > .70) +
    3 * (X[, 2] > .55) + 4 * (X[, 3] > .45 & X[, 4] > .60)
  vertex <- mu + rnorm(n, sd = 2)
  Y <- cbind(vertex - runif(n, 0, 2), vertex, vertex + runif(n, 0, 2))
  E <- NULL
  if (expert) {
    # Reliable-expert setting: error sd=1, independent of observation noise.
    ev <- mu + rnorm(n, sd = 1)
    E <- cbind(ev - runif(n, 0, 2), ev, ev + runif(n, 0, 2))
  }
  list(X = X, Y = Y, E = E)
}
train <- make_data(50, expert = TRUE)
test <- make_data(1000)
fit <- fit_eirt(train$X, train$Y, train$E,
                max_depth = 3, min_samples_split = 6, min_samples_leaf = 3)
reference <- fit_eirt(train$X, train$Y, use_expert = FALSE,
                      max_depth = 3, min_samples_split = 6, min_samples_leaf = 3)
pred <- predict(fit, test$X)
print(head(pred))
errors <- function(model) {
  residual <- test$Y[, 2] - predict(model, test$X, type = "vertex")
  c(RMSE = sqrt(mean(residual^2)), MAE = mean(abs(residual)))
}
print(rbind(EIRT = errors(fit), `TFN-RT` = errors(reference)))
cat("Illustrative single replicate with fixed settings, not the paper's benchmark.\n")
