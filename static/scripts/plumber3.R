# plumber3.R

# load model
load("../../data/model_rf.RData")

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-", 
      req$REQUEST_METHOD, req$PATH_INFO, "-", 
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# predict test case using model
#' @get /predict
calculate_prediction <- function(test) {
  pred <- predict(model_rf, test)
  cat("----------------\nTest case predicted to be", unlist(pred), "\n----------------")
}
