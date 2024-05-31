#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param analysis_set
#' @return
#' @author bcjaeger
#' @export
lm_fit <- function(analysis_set, .outcome, scale_by = 10) {

  .analysis_set <- analysis_set %>%
    map(
      ~ {
        .x$outcome <- .x[[.outcome]]
        .x
      }
    )

  fits <- map(.analysis_set,
              ~ lm(outcome ~ I(cvd_prevent_30*scale_by), data = .x))

  list(
    complete_case = tidy(fits[[1]], conf.int = TRUE),
    imputed = tidy(pool(fits[-1]), conf.int = TRUE)
  ) %>%
    bind_rows(.id = 'missing_data') %>%
    select(missing_data, term,
           estimate, conf.low, conf.high, p.value) %>%
    filter(term != "(Intercept)") %>%
    mutate(outcome = .outcome, .before = 1)


}

