#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param analysis_set
#' @return
#' @author bcjaeger
#' @export
#'

# analysis_set <- tar_read(analysis_set_overall)
# .outcome <- 'lvmi_height'
# scale_by <- 10


lm_fit <- function(analysis_set, .outcome, scale_by = 10) {

  .analysis_set <- analysis_set %>%
    map(
      ~ {
        .x$outcome <- .x[[.outcome]]
        filter(.x,
               !(time_chd < time_v2 & status_chd == 1),
               !(time_stroke < time_v2 & status_stroke == 1),
               cvd_prevent_10 < 0.10)
      }
    )

  fits_ctns <- map(.analysis_set,
                   ~ lm(outcome ~ I(cvd_prevent_30*scale_by), data = .x))

  fits_catg <- map(.analysis_set,
                   ~ lm(outcome ~ cvd_prevent_cat_30, data = .x))

  map_dfr(.x = list(ctns = fits_ctns,
                    catg = fits_catg),
          .f = tidy_lm_fits,
          .id = 'term_group') %>%
    mutate(outcome = .outcome, .before = 1) %>%
    filter(!(term_group == 'ctns' & term == '<10%'))

}

tidy_lm_fits <- function(fits){
  list(
    complete_case = tidy(fits[[1]], conf.int = TRUE),
    imputed = tidy(pool(fits[-1]), conf.int = TRUE)
  ) %>%
    map(add_row, .before = 1, term = "<10%", estimate = 0) %>%
    bind_rows(.id = 'missing_data') %>%
    select(missing_data, term,
           estimate, conf.low, conf.high, p.value) %>%
    filter(term != "(Intercept)") %>%
    mutate(term = str_remove(term, '^cvd_prevent_cat_30'))
}
