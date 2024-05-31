#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param analysis_set
#' @return
#' @author bcjaeger
#' @export
icen_fit <- function(analysis_set) {

  fit <-
    ic_sp(formula = cbind(time_htn_left,
                          time_htn_right) ~ I(cvd_prevent_30*10),
          data = analysis_set$Dataset_0,
          bs_samples = 100)

  summary(fit)$summaryParameters %>%
    as_tibble(rownames = 'term')

}
