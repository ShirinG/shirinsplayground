# plumber.R

# load model
load("../../data/model_rf.RData")

# predict test case using model
#' @get /predict
calculate_prediction <- function(test) {
  predict(model_rf, test, type = "prob")
}
