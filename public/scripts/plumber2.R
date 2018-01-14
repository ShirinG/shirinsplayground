# plumber2.R

# load model
load("../../data/model_rf.RData")

# predict test case using model
#' @get /predict
calculate_prediction <- function(test) {
  pred <- predict(model_rf, test)
  cat("----------------\nTest case predicted to be", unlist(pred), "\n----------------")
}
