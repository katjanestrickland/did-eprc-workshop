#!/usr/bin/env Rscript
# Reproducible workshop panels, Texas revision 2026-10-18. No adjustment covariates.
suppressPackageStartupMessages({library(dplyr); library(readr); library(haven)})
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script <- if (length(arg)) sub("^--file=", "", arg[1]) else "code/01_generate_workshop_data.R"
script <- gsub("~+~", " ", script, fixed = TRUE)
root <- normalizePath(file.path(dirname(script), ".."))
dir.create(file.path(root, "validation"), showWarnings = FALSE)
set.seed(20261014)
calibration <- read_csv(file.path(root, "data", "texas_calibration.csv"), show_col_types = FALSE)
n <- as.integer(calibration$N_Districts)
rho <- calibration$AR1_Coefficient
residual_sd <- sqrt(calibration$Var_Residual)
district_fe <- rnorm(n, 0, sqrt(calibration$Var_Between))
year_fe <- rnorm(7, 0, sqrt(calibration$Var_Year))
errors <- t(replicate(n, {
  e <- numeric(7)
  e[1] <- rnorm(1, 0, residual_sd)
  for (t in 2:7) e[t] <- rho * e[t-1] + rnorm(1, 0, residual_sd * sqrt(1-rho^2))
  e
}))
base <- expand.grid(year = 1:7, district_id = seq_len(n)) %>%
  arrange(district_id, year) %>%
  mutate(state = "TX", y0 = district_fe[district_id] + year_fe[year] + as.vector(t(errors)))
# Match Pure Approach 1: standardize the full untreated panel before adding effects.
reference_mean <- mean(base$y0)
reference_sd <- sd(base$y0)
base$y0 <- (base$y0 - reference_mean) / reference_sd
stopifnot(abs(mean(base$y0)) < 1e-12, abs(sd(base$y0) - 1) < 1e-12)
make_panel <- function(starts, dynamic = FALSE) {
  assignment <- tibble(district_id = sample(seq_len(n)), treatment_start_year = starts)
  left_join(base, assignment, by = "district_id") %>%
    mutate(
      cohort = ifelse(treatment_start_year == 0, "never", paste0("treated_year", treatment_start_year)),
      treated_ever = as.integer(treatment_start_year > 0),
      treated = as.integer(treated_ever == 1 & year >= treatment_start_year),
      event_time = ifelse(treated_ever == 1, year - treatment_start_year, NA_integer_),
      true_effect_std = ifelse(treated == 0, 0,
        if (dynamic) 0.10 + 0.10 * event_time + 0.05 * (treatment_start_year - 5) else 0.20),
      outcome_std = y0 + true_effect_std
    )
}
panels <- list(
  simple_did_district = make_panel(rep(c(5L, 0L), each = 416)),
  staggered_did_district = make_panel(rep(c(5L, 6L, 0L), times = c(208, 208, 416)), TRUE)
)
summaries <- list()
for (name in names(panels)) {
  p <- panels[[name]]
  expected_effect <- if (name == "simple_did_district") {
    ifelse(p$treated == 1, 0.20, 0)
  } else {
    ifelse(p$treated == 0, 0,
           ifelse(p$treatment_start_year == 5, 0.10 * (p$year - 4),
                  0.15 + 0.10 * (p$year - 6)))
  }
  stopifnot(max(abs(p$true_effect_std - expected_effect)) < 1e-12,
            abs(mean(p$true_effect_std[p$treated == 1]) - 0.20) < 1e-12,
            all(p$state == "TX"))
  teaching <- select(p, state, district_id, year, cohort, treatment_start_year,
                     treated_ever, treated, event_time, outcome_std)
  stopifnot(nrow(teaching) == 5824, !anyDuplicated(teaching[c("district_id", "year")]),
            all(table(teaching$district_id) == 7), !anyNA(teaching$outcome_std),
            ncol(teaching) == 9, !("outcome_raw" %in% names(teaching)))
  expected <- if (name == "simple_did_district") c(`0`=416L, `5`=416L) else c(`0`=416L, `5`=208L, `6`=208L)
  counts <- table(teaching$treatment_start_year[teaching$year == 1])
  stopifnot(identical(names(counts), names(expected)), all(as.integer(counts) == expected),
            all(teaching$treated == as.integer(teaching$treatment_start_year > 0 &
                                               teaching$year >= teaching$treatment_start_year)),
            all(is.na(teaching$event_time) == (teaching$treatment_start_year == 0)))
  for (v in c("district_id", "year", "treatment_start_year", "treated_ever", "treated", "event_time")) teaching[[v]] <- as.integer(teaching[[v]])
  write_csv(teaching, file.path(root, "data", paste0(name, ".csv")))
  csv_check <- read_csv(file.path(root, "data", paste0(name, ".csv")), show_col_types = FALSE)
  stopifnot(isTRUE(all.equal(as.data.frame(teaching), as.data.frame(csv_check), check.attributes=FALSE)))
  write_dta(teaching, file.path(root, "data", paste0(name, ".dta")))
  check <- read_dta(file.path(root, "data", paste0(name, ".dta")))
  stopifnot(isTRUE(all.equal(as.data.frame(teaching), as.data.frame(check), check.attributes=FALSE)))
  truth <- transmute(p, district_id, year, untreated_outcome_std = y0,
                     true_effect_std)
  write_csv(truth, file.path(root, "validation", paste0(name, "_truth.csv")))
  summaries[[name]] <- teaching %>% distinct(district_id, cohort, treatment_start_year) %>%
    count(cohort, treatment_start_year, name="districts") %>%
    mutate(dataset=name, years=7L, rows=districts*7L,
           post_periods=ifelse(treatment_start_year == 0, 0, 8-treatment_start_year))
}
write_csv(bind_rows(summaries), file.path(root, "data", "data_generation_summary.csv"))
dictionary <- tibble::tribble(
  ~variable, ~description,
  "state", "State whose SEDA-derived parameters calibrate this simulation: TX.",
  "district_id", "Synthetic district identifier (1-832), not a real district ID; repeated in each of seven years.",
  "year", "Study period 1-7, not a calendar date.",
  "cohort", "Treatment cohort: treated_year5, treated_year6, or never (as applicable). No Year 7 adoption cohort.",
  "treatment_start_year", "First treated year; 0 means never treated. Constant within district.",
  "treated_ever", "1 for a district eventually treated; 0 for never-treated districts.",
  "treated", "1 from the district's first treated year onward; otherwise 0.",
  "event_time", "year minus treatment_start_year; missing (NA in CSV, . in Stata) for never-treated districts.",
  "outcome_std", "Observed synthetic achievement on a common reference-SD scale. The full untreated seven-year Texas panel defines the reference mean and SD; observed outcomes are not restandardized after treatment."
)
write_csv(dictionary, file.path(root, "data", "data_dictionary.csv"))
write_csv(tibble(seed=20261014, reference_mean, reference_sd,
                 simple_true_ATT_std=0.20, staggered_true_simple_ATT_std=0.20),
          file.path(root, "validation", "generation_parameters.csv"))
cat("Generated and checked two seven-year panels: 832 districts / 5824 rows each.\n")
