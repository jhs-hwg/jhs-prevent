#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param jhs_excluded
jhs_derive <- function(jhs_excluded) {

  sex_levels <- list(male = 'Male', female = 'Female')
  smoke_current_levels <- list(no = "No", yes = "Yes")
  diabetes_levels <- list(no = "No", yes = "Yes")
  bp_meds_levels <- list(no = "No", yes = "Yes")
  statin_meds_levels <- list(no = "No", yes = "Yes")

  jhs_excluded$data %>%
    mutate(

      lvmi_bsa = lvmass_v3 / bsa_v3,
      lvmi_height = lvmass_v3 / (height_m ^ 2.7),

      lvh_bsa = if_else(sex == "Male",
                        lvmi_bsa > 106.2,
                        lvmi_bsa > 84.6),

      lvh_height = if_else(sex == "Male",
                           lvmi_height > 45.1,
                           lvmi_height > 38),

      time_htn_left = if_else(htn_v2 == 'Yes' | is.na(htn_v2),
                              true = 0,
                              false = time_v2),


      time_htn_right = case_when(htn_v2 == "Yes" ~ time_v2,
                                 htn_v3 == "Yes" ~ time_v3,
                                 TRUE ~ Inf),

      status_htn = case_when(
        htn_v2 == "Yes" | htn_v3 == "Yes" ~ 1,
        TRUE ~ 0
      ),

      ascvd_pce = predict_10yr_ascvd_risk(
        age_years = age,
        race = rep("black", n()),
        sex = sex,
        smoke_current = smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = sbp,
        bp_meds = bp_meds,
        diabetes = diabetes,
        equation_version = "Goff_2013",
        override_boundary_errors = TRUE,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels
      ),
      ascvd_prevent_10 = predict_10yr_ascvd_risk(
        age_years = age,
        sex = sex,
        smoke_current = smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = sbp,
        bp_meds = bp_meds,
        diabetes = diabetes,
        statin_meds = statin_meds,
        egfr_mlminm2 = egfr_2021,
        bmi = bmi,
        equation_version = "Khan_2023",
        prevent_type = 'base',
        override_boundary_errors = TRUE,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),
      ascvd_prevent_30 = predict_30yr_ascvd_risk(
        age_years = age,
        sex = sex,
        smoke_current = smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = sbp,
        bp_meds = bp_meds,
        diabetes = diabetes,
        statin_meds = statin_meds,
        egfr_mlminm2 = egfr_2021,
        bmi = bmi,
        equation_version = "Khan_2023",
        prevent_type = 'base',
        override_boundary_errors = TRUE,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),
      cvd_prevent_10 = predict_10yr_cvd_risk(
        age_years = age,
        sex = sex,
        smoke_current = smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = sbp,
        bp_meds = bp_meds,
        diabetes = diabetes,
        statin_meds = statin_meds,
        egfr_mlminm2 = egfr_2021,
        bmi = bmi,
        equation_version = "Khan_2023",
        prevent_type = 'base',
        override_boundary_errors = TRUE,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),
      cvd_prevent_30 = predict_30yr_cvd_risk(
        age_years = age,
        sex = sex,
        smoke_current = smoke_current,
        chol_total_mgdl = chol_total,
        chol_hdl_mgdl = chol_hdl,
        bp_sys_mmhg = sbp,
        bp_meds = bp_meds,
        diabetes = diabetes,
        statin_meds = statin_meds,
        egfr_mlminm2 = egfr_2021,
        bmi = bmi,
        equation_version = "Khan_2023",
        prevent_type = 'base',
        override_boundary_errors = TRUE,
        sex_levels = sex_levels,
        smoke_current_levels = smoke_current_levels,
        diabetes_levels = diabetes_levels,
        bp_meds_levels = bp_meds_levels,
        statin_meds_levels = statin_meds_levels
      ),


      ascvd_pce_bnry_10 = factor(ascvd_pce >= 0.10,
                                 levels = c(FALSE, TRUE),
                                 labels = c("< 10%", "≥ 10%")),

      ascvd_prevent_bnry_10 = factor(ascvd_prevent_10 >= 0.10,
                                     levels = c(FALSE, TRUE),
                                     labels = c("< 10%", "≥ 10%")),

      ascvd_prevent_bnry_30 = factor(ascvd_prevent_30 >= 0.30,
                                     levels = c(FALSE, TRUE),
                                     labels = c("< 30%", "≥ 30%")),

      cvd_prevent_bnry_10 = factor(cvd_prevent_10 >= 0.10,
                                   levels = c(FALSE, TRUE),
                                   labels = c("< 10%", "≥ 10%")),

      cvd_prevent_bnry_30 = factor(cvd_prevent_30 >= 0.30,
                                   levels = c(FALSE, TRUE),
                                   labels = c("< 30%", "≥ 30%")),


      bp_cat = case_when(
        bp_meds == "Yes" ~ "hypertension",
        sbp < 120 & dbp < 80 ~ "norm",
        sbp < 130 & dbp < 80 ~ "elevated",
        sbp < 140 & dbp < 90 ~ "stage_1",
        sbp >= 140 | dbp >= 90 ~ "hypertension"
      ),

      bp_cat = factor(bp_cat, levels = c("norm",
                                         "elevated",
                                         "stage_1",
                                         "hypertension")),

      cvd_prevent_cat_30 = case_when(
        cvd_prevent_30 < 0.10 ~ "<10%",
        cvd_prevent_30 < 0.15 ~ "10% to <15%",
        cvd_prevent_30 < 0.20 ~ "15% to <20%",
        cvd_prevent_30 < 0.25 ~ "20% to <25%",
        cvd_prevent_30 < 0.30 ~ "25% to <30%",
        cvd_prevent_30 >= .30 ~ "\u226530%",
        TRUE ~ NA_character_
      ),

      cvd_prevent_cat_30 = factor(cvd_prevent_cat_30,
                                  levels = c("<10%",
                                             "10% to <15%",
                                             "15% to <20%",
                                             "20% to <25%",
                                             "25% to <30%",
                                             "\u226530%")),

      cvd_group = case_when(
        cvd_prevent_10 >= 0.10 ~ "high_10",
        cvd_prevent_30 <  0.30 ~ "low_10_low_30",
        cvd_prevent_30 >= 0.30 ~ "low_10_high_30",
        TRUE ~ NA_character_
      )

      # cvd_group = recode(
      #   cvd_group,
      #   high_10 = "10-year risk \u2265 10%",
      #   low_10_low_30 = "10 year risk < 10% and 30-year risk < 30%",
      #   low_10_high_30 = "10 year risk < 10% and 30-year risk \u2265 30%"
      # )

    )

}
