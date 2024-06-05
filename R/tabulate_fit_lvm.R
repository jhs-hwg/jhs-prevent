#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param fit_lvm_cmbn
#' @param labels
#' @return
#' @author bcjaeger
#' @export
tabulate_fit_lvm <- function(fit_lvm_cmbn, labels) {

  rspec <- round_spec() %>%
    round_using_decimal(2)

  tbl_data <- fit_lvm_cmbn %>%
    mutate(
      tbl_value = if_else(
        is.na(conf.low) & is.na(conf.high),
        "0 (ref)",
        table_glue("{estimate}\n({conf.low}, {conf.high})", rspec = rspec),
      ),
      group = recode(group, !!!labels$levels$bp_cat),
      term_group = recode(term_group,
                          ctns = "Change per 10% predicted risk",
                          catg = "Differences by risk categories"),
      term = if_else(str_detect(term, "scale_by"),
                     "per 10% predicted risk",
                     term)
    ) %>%
    select(-c(estimate, conf.low, conf.high, p.value)) %>%
    pivot_wider(names_from = group, values_from = tbl_value) %>%
    split(list(.$outcome, .$missing_data)) %>%
    map(select, -outcome, -missing_data)

}
