## ----Install-packages--------------------------------------------------------
# Install packages only if they are not already installed.
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest")
if (!requireNamespace("did", quietly = TRUE)) install.packages("did")

## ----Set up packages----------------------------------------------------------
# Load for each session.
library(readr)
library(dplyr)
library(fixest)
library(did)


## ----Load the Data------------------------------------------------------------
# Import the common-adoption and staggered-adoption panels
traditional <- read_csv("traditional_did_district.csv")
staggered <- read_csv("staggered_did_district.csv")


## ----Describe the Cohorts----------------------------------------------------
# Count each district once, rather than counting its seven observations
# A treatment start year of 0 identifies never-treated districts
traditional %>%
  distinct(district_id, treatment_start_year) %>%
  count(treatment_start_year, name = "districts")
staggered %>%
  distinct(district_id, treatment_start_year) %>%
  count(treatment_start_year, name = "districts")


################################################################################
######################Traditional DID###########################################
## ----Estimate the Classic Two-by-Two DID--------------------------------------
# Two groups and two time periods
# Keep one pre-treatment year and one post-treatment year
two_period <- traditional %>%
  filter(year %in% c(4, 5))
# Model: Y_it = α_i + λ_t + βD_it + u_it
# Cluster standard errors by district
model_2x2 <- feols(
  outcome_std ~ treated | district_id + year,
  data = two_period,
  vcov = ~ district_id
)
summary(model_2x2)


## ----Estimate TWFE Using All Seven Years--------------------------------------
# Model: Y_it = α_i + λ_t + βD_it + u_it
model_twfe <- feols(
  outcome_std ~ treated | district_id + year,
  data = traditional,
  vcov = ~ district_id
)
summary(model_twfe)
# The coefficient is the increase in average achievement 
# from pre-treatment periods (Year 1-4) to post-treatment periods (Year 5-7)


## ----Estimate an Event Study--------------------------------------------------
# k=t-g
# Event time 0 is Year 5, and event time -1 is Year 4
traditional <- traditional %>%
  mutate(relative_year = year - 5)

# Estimate separate year-specific effects relative to Year 4
# Model: Y_it = α_i + λ_t + Σ_{k ≠ -1} β_k [1{t - g = k} × D_i] + u_it
model_event <- feols(
  outcome_std ~ i(relative_year, treated_ever, ref = -1) |
    district_id + year,
  data = traditional,
  vcov = ~ district_id
)
summary(model_event)
# Reference period: Year 4 (k = -1)

################################################################################
######################Staggered DID#############################################
## ----The Callaway–Sant’Anna (CSA) Method--------------------------------------
# Make the bootstrap reproducible
set.seed(20261014)
# Estimate ATT(g,t) for each adoption group g and calendar year t
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
# Group 5, Time 5 is ATT(5,5), about 0.150 SD
# This is a group-by-time effect


## ----Group-Specific Effects---------------------------------------------------
# Average post-treatment effects within each adoption group
group_att <- aggte(model_csa, type = "group")
summary(group_att)
# Group 5 averages ATT(5,5), ATT(5,6), and ATT(5,7), about 0.251 SD
# Group 6 averages ATT(6,6) and ATT(6,7), about 0.241 SD
# The overall group average weights these by cohort size, about 0.246 SD


## ----Calendar Time Effects----------------------------------------------------
# Average effects for groups already treated in the same calendar year
calendar_att <- aggte(model_csa, type = "calendar")
summary(calendar_att)
# Year 5 uses ATT(5,5) only, about 0.150 SD
# Years 6 combine ATT(5,6) and ATT(6,6), about 0.200 and 0.342 SD
# Years 7 combine ATT(5,7) and ATT(6,7), about 0.200 and 0.342 SD
# The overall calendar average weights these three years equally


## ----Simple ATT--------------------------------------------------------------
# Combine all post-treatment group-time effects using cohort-size weights
overall_csa <- aggte(model_csa, type = "simple")
summary(overall_csa)
# # Cohort 5: ATT(5,5), ATT(5,6), ATT(5,7)
# Cohort 6: ATT(6,6), ATT(6,7)
# simple ATT: Weighted average of all Group-by-time ATT with weights based on cohort size
# Each of the five cells has weight 0.200 here


## ----Dynamic Effects------------------------------------------------------
# Average across groups at the same time since treatment began
dynamic_att <- aggte(model_csa, type = "dynamic")
summary(dynamic_att)
# Event time 0 combines ATT(5,5) and ATT(6,6), about 0.171 SD
# Event time 1 combines ATT(5,6) and ATT(6,7), about 0.249 SD
# Event time 2 uses ATT(5,7) only, about 0.393 SD
# The overall dynamic average is about 0.271 SD

