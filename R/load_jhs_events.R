#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

load_jhs_events <- function() {

 fpath <- "U:/data/Events/1-data/CSV/"

 inc_events <-
  list.files(fpath,
   pattern = "incev",
   include.dirs = FALSE,
   full.names = TRUE
  ) %>%
  set_names(c('chd', 'hf', 'stroke')) %>%
  map(read_csv, show_col_types = FALSE)

 inc_events$hf %<>%
  select(subjid = `Cohort ID`,
         status_hf = `Incidence Heart Failure`,
         date_hf_start = `Incidence Assessment Start Point`,
         date_hf = `Event or Censoring Date`,
         last_contact_hf = `Last Contact Type`)

 inc_events$chd %<>%
  select(subjid = `Participant ID`,
         status_chd = `Incidence CHD`,
         date_chd = `CHD Event or Censoring Date`,
         last_contact_chd = `CHD Last Contact Type`)

 inc_events$stroke %<>%
  select(subjid = `Participant ID`,
         status_stroke = `Incidence Stroke`,
         date_stroke = `Event or Censoring Date`,
         last_contact_stroke = `Last Contact Type`)


 events <- inc_events[c('chd', 'hf', 'stroke')] %>%
  reduce(.f = left_join, by = 'subjid')

 events


}
