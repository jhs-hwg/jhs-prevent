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

# analysis_set <- tar_read(analysis_set_norm)

icen_fit <- function(analysis_set) {

  fit_ctns <-
    ic_sp(formula = cbind(time_htn_left,
                          time_htn_right) ~ I(cvd_prevent_30*10),
          data = analysis_set$Dataset_0,
          bs_samples = 100)

  smry_ctns <- summary(fit_ctns)$summaryParameters %>%
    as_tibble(rownames = 'term') %>%
    mutate(term = "Per 10% predicted risk")

  fit_catg <- ic_sp(formula = cbind(time_htn_left,
                                    time_htn_right) ~ cvd_prevent_cat_30,
                    data = analysis_set$Dataset_0,
                    bs_samples = 100)

  smry_catg <- summary(fit_catg)$summaryParameters %>%
    as_tibble(rownames = 'term') %>%
    mutate(term = str_remove(term, '^cvd_prevent_cat_30'))

  ref <- levels(analysis_set$Dataset_0$cvd_prevent_cat_30)[1]

  bind_rows(smry_ctns,
            tibble(term = ref, Estimate = 0, `Exp(Est)` = 1),
            smry_catg) %>%
    rename(est = `Exp(Est)`) %>%
    mutate(lwr = exp(Estimate - 1.96 * Std.Error),
           upr = exp(Estimate + 1.96 * Std.Error)) %>%
    select(term, est, lwr, upr, p)

}
