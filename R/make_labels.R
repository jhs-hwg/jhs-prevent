#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author bcjaeger
#' @export
make_labels <- function(...) {

  variables <- c(
    age = "Age",
    sex = "Sex",
    bmi = "Body mass index",
    bp_meds = "Antihypertensive medication use",
    statin_meds = "Statin use",
    sbp = "Systolic blood pressure",
    dbp = "Diastolic blood pressure",
    htn_v1 = "Hypertension at visit 1",
    htn_v2 = "Hypertension at visit 2",
    htn_v3 = "Hypertension at visit 3",
    hba1c = "Glycated hemoglobin",
    diabetes = "Diabetes",
    chol_total = "Total cholesterol",
    chol_hdl = "High density lipoprotein cholesterol",
    chol_ldl = "Low density lipoprotein cholesterol",
    smoke_current = "Currently smoking",
    egfr_2021 = "Estimated glomerular filtration rate",
    lvmi_height = "Left ventricular mass at visit 3",
    lvh_height = "Left ventricular hypertrophy at visit 3",
    cvd_prevent_10 = "PREVENT 10-year CVD risk",
    cvd_prevent_30 = "PREVENT 30-year CVD risk",
    cvd_prevent_bnry_10 = "PREVENT 10-year CVD risk \u2265 10%",
    cvd_prevent_bnry_30 = "PREVENT 30-year CVD risk \u2265 30%",
    cvd_prevent_cat_30 = "PREVENT 30-year CVD risk categories",
    bp_cat = "Blood pressure category",
    cvd_group = "CVD risk category"
  )

  units <- c(
    age = "years",
    bmi = "kg/m2",
    sbp = "mm Hg",
    dbp = "mm Hg",
    hba1c = "%",
    chol_total = "mg/dL",
    chol_hdl = "mg/dL",
    chol_ldl = "mg/dL",
    egfr_2021 = "mL/min/1.73m2",
    lvmi_height = 'g/m2.7',
    cvd_prevent_10 = "%",
    cvd_prevent_30 = "%"
  )

  levels <- list(
    sex = c("Male" = "Men", "Female" = "Women"),
    bp_cat = c(
      overall  = "Overall",
      norm     = "Normal blood pressure",
      elevated = "Elevated blood pressure",
      stage_1  = "Stage 1 hypertension"
    )
  )

  abbreviations <- c(
    CI   = 'confidence interval',
    SD   = 'standard deviation',
    IQR  = 'interquartile range',
    EHR  = 'electronic health records',
    CVD  = 'cardiovascular disease',
    CKD  = 'chronic kidney disease',
    LV   = 'left ventricular',
    LVM  = 'left ventricular mass',
    LVH  = 'left ventricular hypertrophy',
    PREVENT = "predicting risk of cardiovascular disease events"
  )

  definitions <- list(
    ckd = paste(
      "estimated glomerular filtration rate <60 ml/min/1.73",
      "meters squared based on the 2021 CKD-EPI creatinine equation."
    )
  )

  labels <- list(...)

  labels$variables     <- variables
  labels$units         <- units
  labels$levels        <- levels
  labels$abbreviations <- abbreviations
  labels$definitions   <- definitions

  labels

}
