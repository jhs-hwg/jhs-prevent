#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param jhs_init
jhs_exclude <- function(jhs_init) {

  # Age for risk prediction
  jhs_exclude_1 <- jhs_init %>%
    filter(age >= 30, age < 60)

  # Consented to follow-up
  jhs_exclude_2 <- jhs_exclude_1 %>%
    filter(last_contact_chd != "Refused",
           last_contact_stroke != "Refused")

  # Without history of CVD:
  jhs_exclude_3 <- jhs_exclude_2 %>%
    filter(!(last_contact_chd %in% c("Previous CHD")),
           !(last_contact_stroke %in% c("Previous Stroke")))

  # Have information on SBP and self-reported antihypertensive medication use
  jhs_exclude_4 <- jhs_exclude_3 %>%
    drop_na(sbp, dbp, bp_meds)

  # Have data for the variables in the PCEs and PREVENT equations
  # - PREVENT variables (age, sex, total cholesterol, HDL-cholesterol,
  #   cigarette smoking, diabetes, estimated glomerular filtration rate,
  #   statin use)
  # - Pooled cohort risk equation variables (race/ethnicity)
  jhs_exclude_5 <- jhs_exclude_4 %>%
    drop_na(age,
            sex,
            # bmi,
            # hba1c,
            chol_total,
            chol_hdl,
            statin_meds,
            smoke_current,
            diabetes,
            egfr_2021)

  # All PREVENT variables should be within range

  require(data.table)

  jhs_exclude_6 <- jhs_exclude_5 %>%
    filter(chol_total %between% c(130, 320),
           chol_hdl %between% c(20, 100),
           sbp %between% c(90, 200),
           # bmi %between% c(18.5, 40),
           egfr_2021 %between% c(15, 140))

  jhs_exclude_7 <- jhs_exclude_6 %>%
    filter(htn_v1 == "No")

  jhs_exclude_8 <- jhs_exclude_7 %>%
    filter(!is.na(htn_v2) | !is.na(htn_v3))

  tidy_counts <- function(x, label){

    tibble(n = nrow(x), label = label)

  }

  counts <- bind_rows(
    tidy_counts(jhs_init, "JHS participants"),
    tidy_counts(jhs_exclude_1, "Age 30 to < 60 years at visit 1"),
    tidy_counts(jhs_exclude_2, "Consented to CVD follow-up at visit 1"),
    tidy_counts(jhs_exclude_3, "No history of CVD at visit 1"),
    tidy_counts(jhs_exclude_4, paste("Have information on self-reported",
                                     "antihypertensive medication use",
                                     "and blood pressure at visit 1")),
    tidy_counts(jhs_exclude_5, paste("Have information on other variables",
                                        "in the PCEs and PREVENT equations at visit 1")),
    tidy_counts(jhs_exclude_6, paste("All variables in range for PREVENT",
                                        "equations at visit 1")),
    tidy_counts(jhs_exclude_7, "Without hypertension at visit 1"),
    tidy_counts(jhs_exclude_8, "With hypertension status known at visit 2 or visit 3")
  )

  list(data = jhs_exclude_8,
       counts = counts)

}
