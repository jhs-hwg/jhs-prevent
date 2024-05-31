#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param jhs_derived
jhs_clean <- function(jhs_derived) {

  jhs_derived %>%
    select(-starts_with("last_contact")) %>%
    relocate(matches("^time|^status"), .before = age) %>%
    mutate(across(where(is.logical), ~factor(.x,
                                             levels = c(FALSE, TRUE),
                                             labels = c("No", "Yes")))) %>%
    mutate(across(where(is.character), as.factor))

}
