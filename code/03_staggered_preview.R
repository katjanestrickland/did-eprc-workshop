staggered_data <- read.csv("assets/data/staggered_did_district.csv")

cohort_counts <- aggregate(
  district_id ~ cohort + treatment_start_year,
  data = unique(staggered_data[c("district_id", "cohort", "treatment_start_year")]),
  FUN = length
)

names(cohort_counts)[names(cohort_counts) == "district_id"] <- "n_districts"
print(cohort_counts)

twfe_staggered <- lm(
  outcome_std ~ treated + factor(district_id) + factor(year),
  data = staggered_data
)

summary(twfe_staggered)
