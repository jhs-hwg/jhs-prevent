

jhs_count <- function(jhs_cleaned) {

  counts_overall <- jhs_count_worker(jhs_cleaned)

  counts_bp_cat <- table(jhs_cleaned$bp_cat) %>%
    c(overall = nrow(jhs_cleaned))

  counts_by_bp_cat <- jhs_cleaned %>%
    split(.$bp_cat) %>%
    map(jhs_count_worker)

  list(overall = counts_overall) %>%
    c(counts_by_bp_cat) %>%
    bind_rows(.id = 'type') %>%
    group_by(type, count_type) %>%
    mutate(
      type = recode(
        type,
        overall  = table_glue("Overall\n(n = {counts_bp_cat['overall']})"),
        norm     = table_glue("Normal blood pressure\n(n = {counts_bp_cat['norm']})"),
        elevated = table_glue("Elevated blood pressure\n(n = {counts_bp_cat['elevated']})"),
        stage_1  = table_glue("Stage 1 hypertension\n(n = {counts_bp_cat['stage_1']})")
      )
    )


}


jhs_count_worker <- function(jhs_data){

  counts_by_10yr_risk <- jhs_data %>%
    count(cvd_prevent_bnry_10)

  counts_by_30yr_risk <- jhs_data %>%
    count(cvd_prevent_cat_30)

  counts_by_30yr_risk_among_low_10yr_risk <- jhs_data %>%
    filter(cvd_prevent_bnry_10 == "< 10%") %>%
    count(cvd_prevent_cat_30)

  bind_rows(
    `10-year total CVD risk`  = counts_by_10yr_risk,
    `30-year total CVD risk` = counts_by_30yr_risk,
    `30-year total CVD risk among those with 10- year total CVD risk <10%` = counts_by_30yr_risk_among_low_10yr_risk,
    .id = 'count_type'
  ) %>%
    transmute(count_type,
              cvd_cat = coalesce(cvd_prevent_bnry_10, cvd_prevent_cat_30),
              n)

}
