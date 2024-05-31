#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param jhs_cleaned
#' @return
#' @author bcjaeger
#' @export
jhs_impute <- function(jhs_cleaned) {

  library(miceRanger)

  impute_pred_vars <-
    c("time_chd",
      "time_stroke",
      "status_chd",
      "status_stroke",
      "time_ascvd",
      "status_ascvd",
      "height_m",
      "age",
      "sex",
      "bmi",
      "bp_meds",
      "statin_meds",
      "sbp",
      "dbp",
      "htn_v2",
      "htn_v3",
      "hba1c",
      "diabetes",
      "chol_total",
      "chol_hdl",
      "chol_ldl",
      "smoke_current",
      "egfr_2021",
      "bsa_v3",
      "lvmi_bsa",
      "lvmi_height",
      "lvh_bsa",
      "lvh_height",
      "ascvd_pce",
      "ascvd_prevent_10",
      "ascvd_prevent_30",
      "cvd_prevent_10",
      "cvd_prevent_30")

  vars_with_missing <- jhs_cleaned %>%
    map_lgl(~any(is.na(.x))) %>%
    enframe() %>%
    filter(value == TRUE) %>%
    pull(name)

  vars_to_impute <- vector('list', length = length(vars_with_missing)) %>%
    set_names(vars_with_missing)

  for(i in seq_along(vars_to_impute)){
    vars_to_impute[[i]] <- setdiff(impute_pred_vars, vars_with_missing[i])
  }

  imps <- miceRanger(data = jhs_cleaned, vars = vars_to_impute)

  imps_post <- completeData(imps) %>%
    map(
      ~ mutate(
        .x,
        lvh_bsa = if_else(sex == "Male",
                          lvmi_bsa > 106.2,
                          lvmi_bsa > 84.6),
        lvh_height = if_else(sex == "Male",
                             lvmi_height > 45.1,
                             lvmi_height > 38),
        across(
          .cols = c(lvh_bsa, lvh_height),
          .fns = ~ factor(.x, levels = c(FALSE, TRUE),
                        labels = c("No", "Yes"))
        )
      )
    )

  c(list(Dataset_0 = jhs_cleaned), imps_post)

}
