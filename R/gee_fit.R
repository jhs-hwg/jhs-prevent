
# analysis_set <- tar_read(analysis_set_elevated)
# .outcome = 'lvh_height'

gee_fit <- function(analysis_set, .outcome) {

  .analysis_set <- analysis_set %>%
    map(
      ~ {
        .x$outcome <- as.numeric(.x[[.outcome]]=="Yes")
        filter(.x,
               !(time_chd < time_v2 & status_chd == 1),
               !(time_stroke < time_v2 & status_stroke == 1),
               cvd_prevent_10 < 0.10)
      }
    )

  fits_ctns <- map(.analysis_set,
                   .f = ~ geeglm(outcome ~ I(cvd_prevent_30*10),
                                 data = .x,
                                 family = 'poisson',
                                 id = seq(nrow(.x))))

  fits_catg <- map(.analysis_set,
                   .f = ~ geeglm(outcome ~ cvd_prevent_cat_30,
                                 data = .x,
                                 family = 'poisson',
                                 id = seq(nrow(.x))))

  map_dfr(.x = list(ctns = fits_ctns,
                    catg = fits_catg),
          .f = tidy_gee_fits,
          .id = 'term_group') %>%
    mutate(outcome = .outcome, .before = 1) %>%
    filter(!(term_group == 'ctns' & term == '<10%'))

}

tidy_gee_fits <- function(fits){

  list(
    complete_case = tidy(fits[[1]], conf.int = TRUE, exponentiate=TRUE),
    imputed = tidy(pool(fits[-1]), conf.int = TRUE, exponentiate=TRUE)
  ) %>%
    map(add_row, term = "<10%", estimate = 1, .before=1) %>%
    bind_rows(.id = 'missing_data') %>%
    select(missing_data, term,
           estimate, conf.low, conf.high, p.value) %>%
    filter(term != "(Intercept)") %>%
    mutate(term = str_remove(term, '^cvd_prevent_cat_30'))

}

