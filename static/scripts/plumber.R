# script name:
# plumber.R

# set API title and description to show up in http://localhost:8000/__swagger__/

#' @apiTitle Run predictions for Chronic Kidney Disease with Random Forest Model
#' @apiDescription This API takes as patient data on Chronic Kidney Disease and returns a prediction whether the lab values
#' indicate Chronic Kidney Disease (ckd) or not (notckd).
#' For details on how the model is built, see https://shirinsplayground.netlify.com/2017/12/lime_sketchnotes/
#' For further explanations of this plumber function, see https://shirinsplayground.netlify.com/2018/01/plumber/

# load model
# this path would have to be adapted if you would deploy this
load("/Users/shiringlander/Documents/Github/shirinsplayground/data/model_rf.RData")

#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# core function follows below:
# define parameters with type and description
# name endpoint
# return output as html/text
# specify 200 (okay) return

#' predict Chronic Kidney Disease of test case with Random Forest model
#' @param age:numeric The age of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param bp:numeric The blood pressure of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param sg_1.005:int The urinary specific gravity of the patient, integer (1: sg = 1.005, otherwise 0)
#' @param sg_1.010:int The urinary specific gravity of the patient, integer (1: sg = 1.010, otherwise 0)
#' @param sg_1.015:int The urinary specific gravity of the patient, integer (1: sg = 1.015, otherwise 0)
#' @param sg_1.020:int The urinary specific gravity of the patient, integer (1: sg = 1.020, otherwise 0)
#' @param sg_1.025:int The urinary specific gravity of the patient, integer (1: sg = 1.025, otherwise 0)
#' @param al_0:int The urine albumin level of the patient, integer (1: al = 0, otherwise 0)
#' @param al_1:int The urine albumin level of the patient, integer (1: al = 1, otherwise 0)
#' @param al_2:int The urine albumin level of the patient, integer (1: al = 2, otherwise 0)
#' @param al_3:int The urine albumin level of the patient, integer (1: al = 3, otherwise 0)
#' @param al_4:int The urine albumin level of the patient, integer (1: al = 4, otherwise 0)
#' @param al_5:int The urine albumin level of the patient, integer (1: al = 5, otherwise 0)
#' @param su_0:int The sugar level of the patient, integer (1: su = 0, otherwise 0)
#' @param su_1:int The sugar level of the patient, integer (1: su = 1, otherwise 0)
#' @param su_2:int The sugar level of the patient, integer (1: su = 2, otherwise 0)
#' @param su_3:int The sugar level of the patient, integer (1: su = 3, otherwise 0)
#' @param su_4:int The sugar level of the patient, integer (1: su = 4, otherwise 0)
#' @param su_5:int The sugar level of the patient, integer (1: su = 5, otherwise 0)
#' @param rbc_normal:int The red blood cell count of the patient, integer (1: rbc = normal, otherwise 0)
#' @param rbc_abnormal:int The red blood cell count of the patient, integer (1: rbc = abnormal, otherwise 0)
#' @param pc_normal:int The pus cell level of the patient, integer (1: pc = normal, otherwise 0)
#' @param pc_abnormal:int The pus cell level of the patient, integer (1: pc = abnormal, otherwise 0)
#' @param pcc_present:int The puc cell clumps status of the patient, integer (1: pcc = present, otherwise 0)
#' @param pcc_notpresent:int The puc cell clumps status of the patient, integer (1: pcc = notpresent, otherwise 0)
#' @param ba_present:int The bacteria status of the patient, integer (1: ba = present, otherwise 0)
#' @param ba_notpresent:int The bacteria status of the patient, integer (1: ba = notpresent, otherwise 0)
#' @param bgr:numeric The blood glucose random level of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param bu:numeric The blood urea level of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param sc:numeric The serum creatinine level of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param sod:numeric The sodium level of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param pot:numeric The potassium level of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param hemo:numeric The hemoglobin level of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param pcv:numeric The packed cell volume of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param wbcc:numeric The white blood cell count of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param rbcc:numeric The red blood cell count of the patient, numeric (scaled and centered to be btw 0 and 1)
#' @param htn_yes:int The hypertension status of the patient, integer (1: htn = yes, otherwise 0)
#' @param htn_no:int The hypertension status of the patient, integer (1: htn = no, otherwise 0)
#' @param dm_yes:int The diabetes mellitus status of the patient, integer (1: dm = yes, otherwise 0)
#' @param dm_no:int The diabetes mellitus status of the patient, integer (1: dm = no, otherwise 0)
#' @param cad_yes:int The coronary artery disease status of the patient, integer (1: cad = yes, otherwise 0)
#' @param cad_no:int The coronary artery disease status of the patient, integer (1: cad = no, otherwise 0)
#' @param appet_good:int The appetite of the patient, integer (1: appet = good, otherwise 0)
#' @param appet_poor:int The appetite of the patient, integer (1: appet = poor, otherwise 0)
#' @param pe_yes:int The pedal edema status of the patient, integer (1: pe = yes, otherwise 0)
#' @param pe_no:int The pedal edema status of the patient, integer (1: pe = no, otherwise 0)
#' @param ane_yes:int The anemia status of the patient, integer (1: ane = yes, otherwise 0)
#' @param ane_no:int The anemia status of the patient, integer (1: ane = no, otherwise 0)
#' @get /predict
#' @html
#' @response 200 Returns the class (ckd or notckd) prediction from the Random Forest model; ckd = Chronic Kidney Disease
calculate_prediction <- function(age, bp, sg_1.005, sg_1.010, sg_1.015, sg_1.020, sg_1.025, al_0, al_1, al_2, 
                                al_3, al_4, al_5, su_0, su_1, su_2, su_3, su_4, su_5, rbc_normal, rbc_abnormal, pc_normal, pc_abnormal,
                                pcc_present, pcc_notpresent, ba_present, ba_notpresent, bgr, bu, sc, sod, pot, hemo, pcv, 
                                wbcc, rbcc, htn_yes, htn_no, dm_yes, dm_no, cad_yes, cad_no, appet_good, appet_poor, pe_yes, pe_no, 
                                ane_yes, ane_no) {
  
  # make data frame from numeric parameters
  input_data_num <<- data.frame(age, bp, bgr, bu, sc, sod, pot, hemo, pcv, wbcc, rbcc,
                     stringsAsFactors = FALSE)
  # and make sure they really are numeric
  input_data_num <<- as.data.frame(t(sapply(input_data_num, as.numeric)))
  
  # make data frame from (binary) integer parameters
  input_data_int <<- data.frame(sg_1.005, sg_1.010, sg_1.015, sg_1.020, sg_1.025, al_0, al_1, al_2, 
                                al_3, al_4, al_5, su_0, su_1, su_2, su_3, su_4, su_5, rbc_normal, rbc_abnormal, pc_normal, pc_abnormal,
                                pcc_present, pcc_notpresent, ba_present, ba_notpresent, htn_yes, htn_no, dm_yes, dm_no, 
                                cad_yes, cad_no, appet_good, appet_poor, pe_yes, pe_no, ane_yes, ane_no,
                                stringsAsFactors = FALSE)
  # and make sure they really are numeric
  input_data_int <<- as.data.frame(t(sapply(input_data_int, as.integer)))
  # combine into one data frame
  input_data <<- as.data.frame(cbind(input_data_num, input_data_int))
  
  # validation for parameter
  if (any(is.na(input_data))) {
    res$status <- 400
    res$body <- "Parameters have to be numeric or integers"
  }
  
  if (any(input_data < 0) || any(input_data > 1)) {
    res$status <- 400
    res$body <- "Parameters have to be between 0 and 1"
  }

  # predict and return result
  pred_rf <<- predict(model_rf, input_data)
  paste("----------------\nTest case predicted to be", as.character(pred_rf), "\n----------------\n")
}

