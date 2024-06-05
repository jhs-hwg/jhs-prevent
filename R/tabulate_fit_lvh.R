#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_lvh_cmbn
#' @param labels
#' @return
#' @author bcjaeger
#' @export
tabulate_fit_lvh <- function(fit_lvh_cmbn, labels) {

  rspec <- round_spec() %>%
    round_using_decimal(2) %>%
    round_half_up()

  tbl_data <- fit_lvh_cmbn %>%
    filter(estimate > 10^-5, estimate < 10^5) %>%
    mutate(
      tbl_value = if_else(
        is.na(conf.low) & is.na(conf.high),
        "1 (ref)",
        table_glue("{estimate}\n({conf.low}, {conf.high})", rspec = rspec),
      ),
      group = recode(group, !!!labels$levels$bp_cat),
      term_group = recode(term_group,
                          ctns = "Per 10% predicted risk",
                          catg = "By risk categories"),
      term = if_else(str_detect(term, "^I\\("),
                     "Per 10% predicted risk",
                     term)
    ) %>%
    select(-c(estimate, conf.low, conf.high, p.value)) %>%
    pivot_wider(names_from = group, values_from = tbl_value) %>%
    split(list(.$outcome, .$missing_data)) %>%
    map(select, -outcome, -missing_data)

}
