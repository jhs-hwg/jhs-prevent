#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_htn_cmbn
#' @return
#' @author bcjaeger
#' @export
tabulate_fit_htn <- function(fit_htn_cmbn, labels) {

  rspec <- round_spec() %>%
    round_using_decimal(2)

  fit_htn_cmbn %>%
    transmute(
      term,
      term_group = if_else(
        str_detect(term, "^Per 10%"),
        term,
        "Risk categories"
      ),
      group = recode(group, !!!labels$levels$bp_cat),
      tbl_value = if_else(
        is.na(lwr) & is.na(upr),
        "1 (ref)",
        table_glue("{est}\n({lwr}, {upr})", rspec = rspec)
      )
    ) %>%
    pivot_wider(names_from = group,
                values_from = tbl_value)


}
