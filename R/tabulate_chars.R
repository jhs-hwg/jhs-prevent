#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param jhs_cleaned
#' @return
#' @author bcjaeger
#' @export
tabulate_chars <- function(jhs_cleaned, labels) {

  tbl_vars <-
    c(
      "age",
      "sex",
      "bmi",
      "smoke_current",
      "statin_meds",
      "sbp",
      "dbp",
      # "hba1c",
      "diabetes",
      "chol_total",
      "chol_hdl",
      "chol_ldl",
      "cvd_prevent_10",
      "cvd_prevent_30",
      # "htn_v2",
      "htn_v3",
      "lvmi_height",
      "lvh_height"
    )

  tbl_labs <- labels$variables[tbl_vars] %>%
    enframe() %>%
    left_join(enframe(labels$units, value = 'unit')) %>%
    transmute(
      name,
      value = if_else(
        is.na(unit),
        value,
        paste(value, unit, sep = ', ')
      )
    ) %>%
    deframe()

  select(jhs_cleaned, all_of(tbl_vars), bp_cat) %>%
    droplevels() %>%
    mutate(
      bp_cat = recode(bp_cat, !!!labels$levels$bp_cat),
      sex = recode(sex, !!!labels$levels$sex),
      across(.cols = c(cvd_prevent_10, cvd_prevent_30),~ .x * 100)
    ) %>%
    tbl_summary(
      by = 'bp_cat',
      label = tbl_labs,
      # type = list(
      #   cvd_prevent_10 ~ "continuous2",
      #   cvd_prevent_30 ~ "continuous2"
      # ),
      statistic = list(
        all_continuous() ~ c("{mean} ({sd})"),
        # all_continuous2() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})"),
        all_categorical() ~ "{p}"
      ),
      digits = list("age" ~ 0,
                    "bmi" ~ 1,
                    "sbp" ~ 0,
                    "dbp" ~ 0,
                    # "hba1c" ~ 1,
                    "chol_total" ~ 0,
                    "chol_hdl" ~ 0,
                    "chol_ldl" ~ 0,
                    "cvd_prevent_10" ~ 1,
                    "cvd_prevent_30" ~ 1,
                    "lvmi_height" ~ 0),
      missing = 'no'
    ) %>%
    add_overall() %>%
    # remove footnotes
    modify_footnote(c(all_stat_cols()) ~ NA) %>%
    modify_header(
      all_stat_cols() ~ "**{level}**,\nN = {n} ({style_percent(p)}%)"
    ) %>%
    # add_stat_label(
    #   # update default statistic label for continuous variables
    #   label = list(all_continuous() ~ "",
    #                all_continuous2() ~ c("Mean (SD)", "Median (25th, 75th %)"))
    # ) %>%
    as_flex_table() %>%
    theme_box() %>%
    align(align = 'center', part = 'all') %>%
    align(j = 1, align = 'left', part = 'all') %>%
    width(j = c(2:5), width = 1.28) %>%
    width(j = 1, width = 2)

}
