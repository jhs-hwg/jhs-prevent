source("packages.R")
source("conflicts.R")

# Drop LVH
# Make table with only continuous CVD risk
# consider pooling normal/elevated
# consider pooling 20-30 and >= 30

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

library(future)
library(future.callr)
plan(callr)


proposal_version <- 2

if(!dir.exists(glue("doc/proposal-v{proposal_version}"))){
  dir.create(glue("doc/proposal-v{proposal_version}"))
}

manuscript_version <- 3

if(!dir.exists(glue("doc/manuscript-v{manuscript_version}"))){
  dir.create(glue("doc/manuscript-v{manuscript_version}"))
}


labels_tar <- tar_target(labels, make_labels())

# data management ---------------------------------------------------------

jhs_init_tar    <- tar_target(jhs_init,     jhs_load())
jhs_exclude_tar <- tar_target(jhs_excluded, jhs_exclude(jhs_init))
jhs_derive_tar  <- tar_target(jhs_derived,  jhs_derive(jhs_excluded))
jhs_clean_tar   <- tar_target(jhs_cleaned,  jhs_clean(jhs_derived))
jhs_imputed_tar <- tar_target(jhs_imputed,  jhs_impute(jhs_cleaned))

tbl_sample_tar <- tar_target(tbl_sample, jhs_count(jhs_cleaned))

tbl_chars_tar <- tar_target(tbl_chars, tabulate_chars(jhs_cleaned, labels))

outcomes_lvh_tar <- tar_target(outcomes_lvh, c("lvh_height", "lvh_bsa"))
outcomes_lvm_tar <- tar_target(outcomes_lvm, c("lvmi_height", "lvmi_bsa"))

analysis_ctns_tar <- tar_map(
  values = tibble(subset = c('overall', 'norm', 'elevated', 'stage_1')),
  tar_target(analysis_set, jhs_subset(jhs_imputed, subset = subset)),
  tar_target(tbl_prevent_dist, tabulate_prevent_dist(analysis_set)),
  tar_target(fit_htn, icen_fit(analysis_set)),

  tar_target(fit_lvm_htn,
             pattern = map(outcomes_lvm),
             lm_fit(analysis_set, .outcome = outcomes_lvm, htn = TRUE)),

  tar_target(fit_lvm_norm,
             pattern = map(outcomes_lvm),
             lm_fit(analysis_set, .outcome = outcomes_lvm, htn = FALSE)),

  tar_target(fit_lvh,
             pattern = map(outcomes_lvh),
             gee_fit(analysis_set, .outcome = outcomes_lvh))
)

tbl_prevent_dist_cmbn_tar <- tar_combine(
  tbl_prevent_dist_cmbn,
  analysis_ctns_tar$tbl_prevent_dist,
  command = bind_rows(!!!.x, .id = 'group') %>%
    mutate(group = str_remove(group, "^tbl_prevent_dist_"))
)

fit_htn_cmbn_tar <- tar_combine(
  fit_htn_cmbn,
  analysis_ctns_tar$fit_htn,
  command = bind_rows(!!!.x, .id = 'group') %>%
    mutate(group = str_remove(group, "^fit_htn_"))
)

tbl_fit_htn_tar <- tar_target(
  tbl_fit_htn,
  tabulate_fit_htn(fit_htn_cmbn, labels = labels)
)

fit_lvm_htn_cmbn_tar <- tar_combine(
  fit_lvm_htn_cmbn,
  analysis_ctns_tar$fit_lvm_htn,
  command = bind_rows(!!!.x, .id = 'group') %>%
    mutate(group = str_remove(group, "^fit_lvm_"))
)

fit_lvm_norm_cmbn_tar <- tar_combine(
  fit_lvm_norm_cmbn,
  analysis_ctns_tar$fit_lvm_norm,
  command = bind_rows(!!!.x, .id = 'group') %>%
    mutate(group = str_remove(group, "^fit_lvm_"))
)

tbl_fit_lvm_htn_tar <- tar_target(
  tbl_fit_lvm_htn,
  tabulate_fit_lvm(fit_lvm_htn_cmbn, labels = labels)
)

tbl_fit_lvm_norm_tar <- tar_target(
  tbl_fit_lvm_norm,
  tabulate_fit_lvm(fit_lvm_norm_cmbn, labels = labels)
)

fit_lvh_cmbn_tar <- tar_combine(
  fit_lvh_cmbn,
  analysis_ctns_tar$fit_lvh,
  command = bind_rows(!!!.x, .id = 'group') %>%
    mutate(group = str_remove(group, "^fit_lvh_"))
)

tbl_fit_lvh_tar <- tar_target(
  tbl_fit_lvh,
  tabulate_fit_lvh(fit_lvh_cmbn, labels = labels)
)

proposal_tar <- tar_render(
  proposal,
  path = here("doc/proposal.Rmd"),
  output_file = paste0("proposal", "-v", proposal_version, "/",
                       "proposal-", "jhs-prevent",
                       "-v", proposal_version,
                       ".docx")
)

manuscript_tar <- tar_render(
  manuscript,
  path = here("doc/manuscript.Rmd"),
  output_file = paste0("manuscript", "-v", manuscript_version, "/",
                       "manuscript-", "jhs-prevent",
                       "-v", manuscript_version,
                       ".docx")
)


targets <- list(
  jhs_init_tar,
  jhs_exclude_tar,
  jhs_derive_tar,
  jhs_clean_tar,
  jhs_imputed_tar,
  labels_tar,
  tbl_sample_tar,
  tbl_chars_tar,
  outcomes_lvh_tar,
  outcomes_lvm_tar,
  analysis_ctns_tar,
  tbl_prevent_dist_cmbn_tar,
  fit_htn_cmbn_tar,
  tbl_fit_htn_tar,
  fit_lvm_htn_cmbn_tar,
  fit_lvm_norm_cmbn_tar,
  tbl_fit_lvm_htn_tar,
  tbl_fit_lvm_norm_tar,
  fit_lvh_cmbn_tar,
  tbl_fit_lvh_tar,
  proposal_tar,
  manuscript_tar
)

tar_hook_before(
  targets = targets,
  hook = {source("conflicts.R")},
  names = everything()
)
