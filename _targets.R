source("packages.R")
source("conflicts.R")

# Switch from pooled cohort 10-year to PREVENT 10-year
# MAKE BP CATEGORY A FACTOR

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

library(future)
library(future.callr)
plan(callr)



proposal_version <- 2

if(!dir.exists(glue("doc/proposal-v{proposal_version}"))){
  dir.create(glue("doc/proposal-v{proposal_version}"))
}


labels_tar <- tar_target(
  labels,
  list(variable_name = "variable label")
)

jhs_init_tar <- tar_target(jhs_init, jhs_load())
jhs_exclude_tar <- tar_target(jhs_excluded, jhs_exclude(jhs_init))
jhs_derive_tar <- tar_target(jhs_derived, jhs_derive(jhs_excluded))
jhs_clean_tar <- tar_target(jhs_cleaned, jhs_clean(jhs_derived))


# library(miceRanger)
#
# imps <- miceRanger(data = jhs_cleaned,
#                    vars = setdiff(names(jhs_cleaned), 'subjid'))
#
# completeData(imps) %>%
#   map(~ lm(lvmi_height ~ cvd_prevent_30,
#            data = .x,
#            subset = htn_v3 == "Yes")) %>%
#   map(anova)

# fit <- lm(lvmi_height ~ cvd_prevent_30,
#           data = jhs_cleaned,
#           subset = htn_v3 == "Yes")
#
# fit <- glm(lvh_height ~ cvd_prevent_30,
#           data = jhs_cleaned,
#           family = 'binomial',
#           subset = htn_v3 == "Yes")
# tidy(fit)

tbl_sample_tar <- tar_target(tbl_sample, jhs_count(jhs_cleaned))

proposal_tar <- tar_render(
  proposal,
  path = here("doc/proposal.Rmd"),
  output_file = paste0("proposal", "-v", proposal_version, "/",
                       "proposal-", "jhs-prevent",
                       "-v", proposal_version,
                       ".docx")
)


targets <- list(
  jhs_init_tar,
  jhs_exclude_tar,
  jhs_derive_tar,
  jhs_clean_tar,
  labels_tar,
  tbl_sample_tar,
  proposal_tar
)

tar_hook_before(
  targets = targets,
  hook = {source("conflicts.R")},
  names = everything()
)
