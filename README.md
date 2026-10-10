# Expert-Informed Regression Tree (EIRT)

Implementation accompanying **Expert-Informed Regression Tree Using Triangular
Fuzzy Numbers**, Duygu Icen.

This repository provides the EIRT algorithm, a small synthetic usage example,
and implementation tests. It is **not** the complete replication package for
the paper's benchmark tables, figures, cross-validation or sensitivity analyses.
The example does not reproduce the paper's numerical results.

## Requirements and quick start

Base R only; tested with R 4.3.2. No package installation is needed.
Download these files into one directory, set that directory as R's working
directory, and run:

```r
source("example.R")
```

Alternatively, from a terminal in that directory:

```sh
Rscript example.R
Rscript tests.R
```

## Use with your own data

Prepare a training data frame with numeric predictor columns and six columns
for the response and expert TFNs. For example:

| Columns | Meaning |
| --- | --- |
| `x1`, `x2` | Numeric predictors (replace with your own predictor names) |
| `y_lower`, `y_vertex`, `y_upper` | Observed response TFN |
| `e_lower`, `e_vertex`, `e_upper` | Expert assessment TFN for the same observation |

The following example assumes `training_data` and `new_data` are already loaded
as R data frames. New observations need only the predictor columns. Do not
include response values, expert assessments or observation IDs as predictors.

```r
source("EIRT.R")

predictor_columns <- c("x1", "x2")  # Change to your predictor names.
X_train <- training_data[, predictor_columns, drop = FALSE]
Y_train <- training_data[, c("y_lower", "y_vertex", "y_upper"), drop = FALSE]
E_train <- training_data[, c("e_lower", "e_vertex", "e_upper"), drop = FALSE]
X_test <- new_data[, predictor_columns, drop = FALSE]

model <- fit_eirt(X_train, Y_train, E_train,
                  max_depth = 3, min_samples_split = 6, min_samples_leaf = 3)
prediction_tfn <- predict(model, X_test)
prediction_vertex <- predict(model, X_test, type = "vertex")

# Expert-free counterpart; no expert inputs are used.
reference <- fit_eirt(X_train, Y_train, use_expert = FALSE,
                      max_depth = 3, min_samples_split = 6, min_samples_leaf = 3)
```

`prediction_tfn` has one row per new observation and three columns named
`lower`, `vertex`, and `upper`. `prediction_vertex` is a numeric vector of
scalar predictions. These TFN endpoints represent the fitted fuzzy response;
they are not calibrated confidence or prediction interval limits.

- `X_train`: finite numeric matrix/data frame, one row per observation.
- `Y_train`, `E_train`: numeric matrices/data frames with exactly three columns
  **in the order lower, vertex, upper**; rows correspond to `X_train`.
- Each row must satisfy `lower <= vertex <= upper`; zero-spread TFNs are allowed.
- Assessments refer to the same quantity and units as the observed responses.
  In the intended training setup, experts do not see the observed responses
  when providing their assessments. The algorithm does not enforce elicitation.
- Missing, infinite, and categorical inputs are rejected; imputation and
  categorical encoding are not implemented.
- If training predictors have names, prediction inputs must have the same names;
  reordered columns are automatically aligned. For unnamed matrices, column
  order must match training.
- Expert assessments are required only for training, never for prediction.

The algorithm does not generate expert assessments or choose TFN spreads for
you. Supply bounds justified by your application. If an observed response is
treated as precise, it can be represented as `(y, y, y)`; this explicitly assumes
zero spread. Without expert assessments, use the expert-free TFN-RT option
shown above rather than constructing assessments from the observed responses.

### Choosing settings and evaluating predictions

The depth and sample-size settings in the example are illustrative. Select
settings using validation data or cross-validation within the training data;
keep the test data separate. In each validation fold, fit using only that fold's
training rows (including their expert assessments), and predict on the held-out
predictors without passing held-out expert assessments. For the paper's tuning
criterion, calculate `tfn_loss(Y_validation, predict(model, X_validation))`.
Use the same preprocessing learned from the training data for new observations.

If observed test responses are available, scalar errors can be calculated from
their vertex values:

```r
# new_data must additionally contain the observed test response vertex.
rmse <- sqrt(mean((new_data$y_vertex - prediction_vertex)^2))
mae <- mean(abs(new_data$y_vertex - prediction_vertex))
```

Optional model persistence (source `EIRT.R` again in a new R session before
calling `predict` on a reloaded model):

```r
saveRDS(model, "eirt_model.rds")
model <- readRDS("eirt_model.rds")
```

## Algorithm

The TFN loss is the **sum** of the squared lower, vertex and upper component
errors. Mean loss averages over observations, not over the three components.
Node and leaf predictions are componentwise means of observed response TFNs.

At each eligible node:

1. Calculate the mean TFN loss `D_E` between responses and expert assessments.
2. Calculate the leave-one-out node-mean reference loss `D_T_LOO`.
3. Set `w_E = max(0, 1 - D_E / D_T_LOO)` when `D_T_LOO > epsilon`;
   otherwise set `w_E = 0`. Keep this weight fixed for every candidate at this node.
4. Search predictor columns in input order and midpoints between sorted distinct
   predictor values in ascending order. Route `X <= threshold` left.
5. Calculate each child's prediction from observed responses only. Evaluate
   `Q = (1-w_E) * L_T + w_E * L_E`, where `L_E` compares those predictions
   with the observation-specific expert TFNs (not with an expert mean).
6. Retain the first exactly tied candidate. Accept the best candidate only when
   `Q_unsplit - Q_best > eta`, using the same node weight in both scores.
7. Repeat within each child, recalculating its weight locally.

The root has depth zero. Maximum depth, minimum split size and minimum child
size control growth. There is no cost-complexity pruning. Defaults are
`epsilon=1e-12`, `eta=1e-8`. Structural defaults are illustrative, not selected
hyperparameters or universally recommended values. The manuscript separately
tunes EIRT and TFN-RT using training-only five-fold cross-validation and full
TFN validation loss; that tuning workflow is not included here.

The fitted object stores node predictions and, at evaluated nodes, reference
losses, expert weights and unsplit scores. Nodes stopped before split evaluation
do not have a calculated weight. The weight is not a probability of expert
correctness and does not guarantee improved prediction.

## Validation

`tests.R` checks component losses, local weights, child scores, response-only
leaf means, stopping rules, exact ties, prediction column alignment, zero-spread
responses and invalid inputs. Before release, an additional local parity audit
compared this implementation against the archived research core in 120 fits
(10 seeds, three training sizes, and four expert/no-expert settings). Tree
structure, predictions, evaluated weights and scores agreed within numerical
tolerance. This is implementation validation, not a rerun of the paper's full
experiments.
