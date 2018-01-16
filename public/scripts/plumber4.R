# plumber3.R

#' @apiTitle Title text
#' @apiDescription Description text

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
#' @post /predict
calculate_prediction <- function(test) {
  pred <- predict(model_rf, test)
  cat("----------------\nTest case predicted to be", unlist(pred), "\n----------------")
}

#' predict test case using model
#' @param age The age of the patient, numeric between x and y 
#' 
#' @get /predict2
#' @response 200 A probability between 0 and 1 indicating chronic kidney disease
calculate_prediction <- function(age, bp, sg_1.005, sg_1.010, sg_1.015, sg_1.020, sg_1.025, al_0, al_1, al_2, 
                                al_3, al_4, al_5, su_0, su_1, su_2, su_3, su_4, su_5, rbc_normal, rbc_abnormal, pc_normal, pc_abnormal,
                                pcc_present, pcc_notpresent, ba_present, ba_notpresent, bgr, bu, sc, sod, pot, hemo, pcv, 
                                wbcc, rbcc, htn_yes, htn_no, dm_yes, dm_no, cad_yes, cad_no, appet_good, appet_poor, pe_yes, pe_no, 
                                ane_yes, ane_no) {
  
  # write validation for parameter
  
  test <- data.frame(age, bp, sg_1.005, sg_1.010, sg_1.015, sg_1.020, sg_1.025, al_0, al_1, al_2, 
                     al_3, al_4, al_5, su_0, su_1, su_2, su_3, su_4, su_5, rbc_normal, rbc_abnormal, pc_normal, pc_abnormal,
                     pcc_present, pcc_notpresent, ba_present, ba_notpresent, bgr, bu, sc, sod, pot, hemo, pcv, 
                     wbcc, rbcc, htn_yes, htn_no, dm_yes, dm_no, cad_yes, cad_no, appet_good, appet_poor, pe_yes, pe_no, 
                     ane_yes, ane_no)
  
  pred <- predict(model_rf, test)
  cat("----------------\nTest case predicted to be", unlist(pred), "\n----------------")
}

