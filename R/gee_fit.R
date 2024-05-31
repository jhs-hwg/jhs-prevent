
# analysis_set <- tar_read(analysis_set_overall)

gee_fit <- function(analysis_set, .outcome) {

  .analysis_set <- analysis_set %>%
    map(
      ~ {
        .x$outcome <- as.numeric(.x[[.outcome]]=="Yes")
        .x
      }
    )

  fits <- map(.analysis_set,
              .f = ~ geeglm(outcome ~ I(cvd_prevent_30*10),
                            data = .x,
                            family = 'poisson',
                            id = seq(nrow(.x))))

  list(
    complete_case = tidy(fits[[1]], conf.int = TRUE, exponentiate=TRUE),
    imputed = tidy(pool(fits[-1]), conf.int = TRUE, exponentiate=TRUE)
  ) %>%
    bind_rows(.id = 'missing_data') %>%
    select(missing_data, term,
           estimate, conf.low, conf.high, p.value) %>%
    filter(term != "(Intercept)") %>%
    mutate(outcome = .outcome, .before = 1)


}
