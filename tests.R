# Run from this directory: Rscript tests.R
source("EIRT.R")
same <- function(a, b) stopifnot(isTRUE(all.equal(a, b, tolerance = 1e-12,
                                               check.attributes = FALSE)))
fails <- function(expr) stopifnot(inherits(tryCatch({force(expr); NULL},
                                                  error = identity), "error"))
X <- cbind(x = 1:8, duplicate = 1:8)
v <- c(0, 0, 0, 0, 10, 10, 10, 10)
Y <- cbind(v - 1, v, v + 2)
E <- Y
same(tfn_loss(Y, Y), 0)
same(tfn_loss(Y, Y + 1), 3)
fit <- fit_eirt(X, Y, E, min_samples_leaf = 2)
stopifnot(fit$tree$feature == 1, fit$tree$threshold == 4.5,
          fit$tree$weight == 1)
same(predict(fit, X), Y)
same(predict(fit, X[, 2:1]), Y)
same(predict(fit, X, type = "vertex"), v)
same(predict(fit, X[1, , drop = FALSE]), Y[1, , drop = FALSE])
stopifnot(identical(dim(predict(fit, X[FALSE, , drop = FALSE])), c(0L, 3L)))
# Equal-score cut points: first midpoint retained.
z <- c(0, 1, 0)
tied <- fit_eirt(matrix(1:3, ncol = 1), cbind(z,z,z), use_expert = FALSE,
                 max_depth = 1, min_samples_split = 2, min_samples_leaf = 1)
same(tied$tree$threshold, 1.5)
# Mean leaf prediction is independent of expert values when growth is disabled.
root <- fit_eirt(X, Y, E + 100, max_depth = 0)
same(root$tree$prediction, colMeans(Y))
stopifnot(root$tree$type == "leaf", is.null(root$tree$weight))
single <- fit_eirt(X[1, , drop = FALSE], Y[1, , drop = FALSE], E[1, , drop = FALSE])
same(single$tree$prediction, Y[1, ])
constant <- matrix(5, 8, 3)
flat <- fit_eirt(X, constant, constant)
stopifnot(flat$tree$type == "leaf", flat$tree$weight == 0)
blocked <- fit_eirt(X, Y, E, min_samples_leaf = 5)
stopifnot(blocked$tree$type == "leaf")
rejected <- fit_eirt(X, Y, E, eta = 1e6)
stopifnot(rejected$tree$type == "leaf")
no_cuts <- fit_eirt(matrix(1, 8, 2), Y, E)
stopifnot(no_cuts$tree$type == "leaf")
ref <- fit_eirt(X, Y, use_expert = FALSE)
bad <- fit_eirt(X, Y, E + 1e4)
same(predict(ref, X), predict(bad, X))
stopifnot(bad$tree$weight == 0)
same(predict(fit_eirt(X, Y, E + 99, use_expert = FALSE), X), predict(ref, X))
fails(fit_eirt(X, Y))
fails(fit_eirt(X, Y, E[-1, ]))
fails(fit_eirt(X, Y[, 3:1], E))
fails(fit_eirt(X, Y, E, min_samples_split = 1))
fails(fit_eirt(X, Y, E, max_depth = 1.5))
fails(fit_eirt(X, Y, E, eta = -1))
fails(fit_eirt(X, Y, E, epsilon = NA_real_))
fails(fit_eirt(X, Y, E, use_expert = NA))
fails(fit_eirt(data.frame(x = letters[1:8]), Y, E))
missing <- X; missing[1, 1] <- NA
fails(fit_eirt(missing, Y, E))
fails(predict(fit, unname(X)))
fails(predict(fit, X[, 1, drop = FALSE]))
fails(tfn_loss(Y, Y[-1, ]))
# Independently recalculate local weights, scores, stopping rules and leaf means.
audit <- function(node, ids, X, Y, E, control) {
  y <- Y[ids, , drop = FALSE]; e <- E[ids, , drop = FALSE]
  same(node$prediction, colMeans(y))
  if (!is.null(node$weight)) {
    loo <- t(vapply(seq_along(ids), function(i) colMeans(y[-i, , drop = FALSE]), numeric(3)))
    dr <- tfn_loss(y, loo); de <- tfn_loss(y, e)
    same(node$reference_loss, dr)
    same(node$weight, if (dr <= control$epsilon) 0 else max(0, 1 - de / dr))
  }
  if (node$type == "node") {
    left <- X[ids, node$feature] <= node$threshold
    stopifnot(sum(left) >= control$min_samples_leaf,
              sum(!left) >= control$min_samples_leaf,
              node$depth < control$max_depth, node$n >= control$min_samples_split)
    p <- matrix(NA_real_, length(ids), 3)
    p[left, ] <- matrix(colMeans(y[left, , drop = FALSE]), sum(left), 3, byrow = TRUE)
    p[!left, ] <- matrix(colMeans(y[!left, , drop = FALSE]), sum(!left), 3, byrow = TRUE)
    same(node$score, (1-node$weight)*tfn_loss(y,p)+node$weight*tfn_loss(e,p))
    stopifnot(node$unsplit_score - node$score > control$eta)
    audit(node$left, ids[left], X, Y, E, control)
    audit(node$right, ids[!left], X, Y, E, control)
  }
}
for (seed in 1:12) {
  set.seed(seed)
  x <- matrix(runif(120), 40, 3)
  mu <- 4*(x[,1]>.4)+3*(x[,2]>.6)
  v <- mu+rnorm(40); ev <- mu+rnorm(40,sd=.5)
  y <- cbind(v-runif(40),v,v+runif(40))
  e <- cbind(ev-runif(40),ev,ev+runif(40))
  m <- fit_eirt(x,y,e,max_depth=4,min_samples_split=4,min_samples_leaf=2)
  audit(m$tree,seq_len(40),x,y,e,m$control)
}
cat("All EIRT implementation tests passed.\n")
