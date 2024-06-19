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

tabulate_prevent_dist <- function(analysis_set) {

  counts <- analysis_set$Dataset_0 %>%
    count(cvd_prevent_bnry_10, cvd_prevent_cat_30) %>%
    group_by(cvd_prevent_bnry_10) %>%
    mutate(p = n / sum(n))

  smry <- analysis_set$Dataset_0 %>%
    # group_by(bp_cat) %>%
    group_by(cvd_prevent_bnry_10, cvd_prevent_cat_30) %>%
    summarize(lvmi_height = mean(lvmi_height, na.rm = TRUE),
              lvh_height = mean(lvh_height == "Yes", na.rm = TRUE),
              htn = mean(status_htn, na.rm = TRUE))

  left_join(counts, smry)

}
