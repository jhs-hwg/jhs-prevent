#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param jhs_imputed
#' @param subset
#' @return
#' @author bcjaeger
#' @export
jhs_subset <- function(jhs_imputed, subset) {

  switch(subset,
         'overall' = jhs_imputed,
         'norm' = map(jhs_imputed, ~filter(.x, bp_cat == 'norm')),
         'elevated' = map(jhs_imputed, ~filter(.x, bp_cat == 'elevated')),
         'stage_1' = map(jhs_imputed, ~filter(.x, bp_cat == 'stage_1')))

}
