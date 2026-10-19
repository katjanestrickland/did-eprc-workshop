## ----packages-----------------------------------------------------------------
# Load for each session.
library(readr)
library(dplyr)
library(fixest)
library(did)


## ----import-------------------------------------------------------------------
simple <- read_csv("simple_did_district.csv")
staggered <- read_csv("staggered_did_district.csv")


## ----counts-------------------------------------------------------------------
simple %>%
  distinct(district_id, treatment_start_year) %>%
  count(treatment_start_year, name = "districts")
staggered %>%
  distinct(district_id, treatment_start_year) %>%
  count(treatment_start_year, name = "districts")


## ----treatment-indicators-----------------------------------------------------
simple <- simple %>%
  mutate(
    treated_ever = as.integer(treatment_start_year > 0),
    treated = as.integer(
      treatment_start_year > 0 & year >= treatment_start_year
    )
  )
staggered <- staggered %>%
  mutate(
    treated_ever = as.integer(treatment_start_year > 0),
    treated = as.integer(
      treatment_start_year > 0 & year >= treatment_start_year
    )
  )


## ----two-period---------------------------------------------------------------
two_period <- simple %>%
  filter(year %in% c(4, 5))
model_2x2 <- feols(
  outcome_std ~ treated | district_id + year,
  data = two_period,
  vcov = ~ district_id
)
summary(model_2x2)


## ----twfe---------------------------------------------------------------------
model_twfe <- feols(
  outcome_std ~ treated | district_id + year,
  data = simple,
  vcov = ~ district_id
)
summary(model_twfe)


## ----event-study--------------------------------------------------------------
simple <- simple %>%
  mutate(relative_year = year - 5)
model_event <- feols(
  outcome_std ~ i(relative_year, treated_ever, ref = -1) |
    district_id + year,
  data = simple,
  vcov = ~ district_id
)
summary(model_event)


## ----csa----------------------------------------------------------------------
set.seed(20261014)
model_csa <- att_gt(
  yname = "outcome_std",
  tname = "year",
  idname = "district_id",
  gname = "treatment_start_year",
  xformla = ~ 1,
  data = staggered,
  panel = TRUE,
  control_group = "nevertreated",
  base_period = "universal",
  anticipation = 0,
  bstrap = TRUE,
  biters = 1000,
  clustervars = "district_id",
  cband = TRUE
)
summary(model_csa)


## ----group-aggregation--------------------------------------------------------
group_att <- aggte(model_csa, type = "group")
summary(group_att)


## ----calendar-aggregation-----------------------------------------------------
calendar_att <- aggte(model_csa, type = "calendar")
summary(calendar_att)


## ----aggregation--------------------------------------------------------------
overall_csa <- aggte(model_csa, type = "simple")
summary(overall_csa)


## ----session------------------------------------------------------------------
sessionInfo()

