#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

jhs_load <- function() {

  v1 <- read_csv("U:/data/Analysis Data/1-data/CSV/analysis1.csv")

  v2 <- read_csv("U:/data/Analysis Data/1-data/CSV/analysis2.csv") %>%
    transmute(subjid, htn_v2 = HTN, time_v2 = DaysFromV1 / 365.25)

  v3 <- read_csv("U:/data/Analysis Data/1-data/CSV/analysis3.csv") %>%
    transmute(subjid, htn_v3 = HTN, bsa_v3 = bsa, time_v3 = DaysFromV1 / 365.25)

  v3_mri <- read_csv("U:/data/Visit 3/1-data/CSV/mrib.csv") %>%
    select(subjid = `PARTICIPANT ID`,
           lvm_mrv = `6. Mrv Left Ventricle Myocardial Mass at End Diastole in (grams)`,
           lrm_qmass = `11. Qmass Left Ventricle Myocardial Mass at End Diastole in (grams)`,
           lvm_cim = `16. CIM Left Ventrical Myocardial Mass in (grams)`)

  events_merge <- load_jhs_events() %>%
    left_join(select(v1, subjid, VisitDate)) %>%
    transmute(
      subjid,
      last_contact_chd,
      last_contact_stroke
    )

  events <- read_csv("D:/JHS-ABPM-vs-office/data/events/jhs_events.csv") %>%
    select(subjid,
           time_chd,
           time_stroke,
           status_chd = status_chd_int,
           status_stroke = status_stroke_int,
           time_ascvd = time_chd_stroke,
           status_ascvd = status_chd_stroke_int) %>%
    right_join(events_merge)

  cystatin_c <- read_csv(
    "U:/data/Visit 1/1-data/CSV/cena.csv",
    col_select = c(
      subjid = `PARTICIPANT ID`,
      cystatin_c = `Concentration of cystatin C (Serum mg/L)`
    ),
    show_col_types = FALSE
  )

  output <- v1 %>%
    left_join(cystatin_c) %>%
    transmute(subjid,
              height_m = height / 100,
              age,
              sex,
              bmi = BMI,
              bp_meds = BPmedsSelf,
              statin_meds = statinMeds,
              sbp,
              dbp,
              htn_v1 = HTN,
              hba1c = HbA1c,
              diabetes = Diabetes,
              chol_total = totchol,
              chol_hdl = hdl,
              chol_ldl = ldl,
              smoke_current = currentSmoker,
              egfr_2021 = nephro::CKDEpi_RF.creat.cys(
                cystatin = cystatin_c,
                creatinine = SCrCC,
                sex = as.numeric(sex == "Male"),
                age = age
              )
    ) %>%
    left_join(events, by = 'subjid') %>%
    left_join(v2, by = 'subjid') %>%
    left_join(v3, by = 'subjid') %>%
    left_join(select(v3_mri, subjid, lvmass_v3 = lvm_cim)) %>%
    relocate(htn_v2, htn_v3, .after = htn_v1)

  output

}
