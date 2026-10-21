## ----packages-----------------------------------------------------------------
# Load for each session.
library(readr)
library(dplyr)
library(fixest)
library(did)
library(etwfe)


## ----import-------------------------------------------------------------------
# Import the common-adoption and staggered-adoption panels
simple <- read_csv("simple_did_district.csv")
staggered <- read_csv("staggered_did_district.csv")


## ----counts-------------------------------------------------------------------
# Count each district once, rather than counting its seven observations
# A treatment start year of 0 identifies never-treated districts
simple %>%
  distinct(district_id, treatment_start_year) %>%
  count(treatment_start_year, name = "districts")
staggered %>%
  distinct(district_id, treatment_start_year) %>%
  count(treatment_start_year, name = "districts")


## ----observed-means, echo=FALSE, results='asis'-------------------------------
# Calculate observed means and changes without rounding the analysis values
observed_means <- simple %>%
  filter(year %in% c(4, 5)) %>%
  group_by(treated_ever) %>%
  summarise(
    year4 = mean(outcome_std[year == 4]),
    year5 = mean(outcome_std[year == 5]),
    .groups = "drop"
  ) %>%
  mutate(change = year5 - year4) %>%
  arrange(desc(treated_ever))
did_from_means <- observed_means$change[1] - observed_means$change[2]
means_display <- observed_means %>%
  transmute(
    Group = if_else(treated_ever == 1, "Adopting districts", "Never-treated districts"),
    `Mean in Year 4` = sprintf("%.3f", year4),
    `Mean in Year 5` = sprintf("%.3f", year5),
    Change = sprintf("%.3f", change)
  )
knitr::kable(means_display, format = "pipe", align = c("l", "r", "r", "r"))


## ----two-period---------------------------------------------------------------
# Keep the year before adoption and the first treated year
two_period <- simple %>%
  filter(year %in% c(4, 5))
# Estimate DID with district and year fixed effects
# Cluster standard errors by district
model_2x2 <- feols(
  outcome_std ~ treated | district_id + year,
  data = two_period,
  vcov = ~ district_id
)
summary(model_2x2)
# The treated coefficient is the two-by-two DID estimate, about 0.212 SD


## ----twfe---------------------------------------------------------------------
# Use all seven years with the same fixed effects and clustering
model_twfe <- feols(
  outcome_std ~ treated | district_id + year,
  data = simple,
  vcov = ~ district_id
)
summary(model_twfe)
# The treated coefficient summarizes the effect across Years 5-7


## ----event-time---------------------------------------------------------------
# Event time 0 is Year 5, and event time -1 is Year 4
simple <- simple %>%
  mutate(relative_year = year - 5)


## ----event-study--------------------------------------------------------------
# Estimate separate year-specific effects relative to Year 4
# treated_ever distinguishes adopters from never-treated districts
model_event <- feols(
  outcome_std ~ i(relative_year, treated_ever, ref = -1) |
    district_id + year,
  data = simple,
  vcov = ~ district_id
)
summary(model_event)
# relative_year::1:treated_ever is the Year 6 effect, about 0.169 SD


## ----csa----------------------------------------------------------------------
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
# This is a group-by-time effect, not an average across post-treatment years


## ----pretrends-plot, fig.path="assets/figures/lab-", fig.width=8, fig.height=5.5, dpi=160, out.width="100%", fig.alt="Group-time estimates for Year 5 and Year 6 adopters, with pre-treatment comparisons and simultaneous confidence bands"----
# Examine pre-treatment comparisons and their simultaneous confidence bands
ggdid(model_csa)


## ----pretest------------------------------------------------------------------
# Show the model results, including the joint pre-trends test
summary(model_csa)
# Extract its p-value; a large value does not prove parallel trends
model_csa$Wpval


## ----group-aggregation--------------------------------------------------------
# Average post-treatment effects within each adoption group
group_att <- aggte(model_csa, type = "group")
summary(group_att)
# Group 5 averages ATT(5,5), ATT(5,6), and ATT(5,7), about 0.251 SD
# Group 6 averages ATT(6,6) and ATT(6,7), about 0.241 SD
# The overall group average weights these by cohort size, about 0.246 SD


## ----extract-effects----------------------------------------------------------
# Collect each group-by-time estimate and its bootstrap standard error
# Use the model's critical value for 95% simultaneous confidence bounds
post_att <- data.frame(
  group = model_csa$group,
  year = model_csa$t,
  estimate = model_csa$att,
  SE = model_csa$se,
  lower_95_sim = model_csa$att - model_csa$c * model_csa$se,
  upper_95_sim = model_csa$att + model_csa$c * model_csa$se
) %>%
  filter(year >= group) %>%
  arrange(group, year) %>%
  mutate(`ATT(g,t)` = paste0("ATT(", group, ",", year, ")"), .before = group)
# Show post-treatment cells, rounding only the displayed table
post_att %>%
  mutate(across(c(estimate, SE, lower_95_sim, upper_95_sim), ~ sprintf("%.3f", .x)))
# Group 5, Year 5 is ATT(5,5), about 0.150 SD
# SE describes uncertainty, and the last two columns give the band bounds




## ----calendar-aggregation-----------------------------------------------------
# Average effects for groups already treated in the same calendar year
calendar_att <- aggte(model_csa, type = "calendar")
summary(calendar_att)
# Year 5 uses Group 5 only, about 0.150 SD
# Years 6 and 7 combine both groups, about 0.200 and 0.342 SD
# The overall calendar average weights these three years equally


## ----aggregation--------------------------------------------------------------
# Combine all post-treatment group-time effects using cohort-size weights
overall_csa <- aggte(model_csa, type = "simple")
summary(overall_csa)
# Each of the five cells has weight 0.200 here
# Simple ATT is approximately 0.247 SD


## ----dynamic-aggregation------------------------------------------------------
# Average across groups at the same time since treatment began
dynamic_att <- aggte(model_csa, type = "dynamic")
summary(dynamic_att)
# Event time 0 combines ATT(5,5) and ATT(6,6), about 0.171 SD
# Event time 1 combines ATT(5,6) and ATT(6,7), about 0.249 SD
# Event time 2 uses ATT(5,7) only, about 0.393 SD
# The overall dynamic average is about 0.271 SD




## ----etwfe-post---------------------------------------------------------------
# Estimate five post-treatment effects with district and year fixed effects
# Use never-treated and not-yet-treated observations as controls
model_etwfe_post <- etwfe(
  fml = outcome_std ~ 0,
  tvar = year,
  gvar = treatment_start_year,
  ivar = district_id,
  gref = 0,
  data = staggered,
  cgroup = "notyet",
  fe = "vs",
  vcov = ~ district_id
)

# Summarize the post-treatment effects as a simple ATT
post_simple <- emfx(model_etwfe_post, type = "simple")
post_simple

# Summarize effects by time since adoption
emfx(model_etwfe_post, type = "event")


## ----etwfe-never--------------------------------------------------------------
# Match the never-treated comparison group used in the CSA model
model_etwfe_never <- etwfe(
  fml = outcome_std ~ 0,
  tvar = year,
  gvar = treatment_start_year,
  ivar = district_id,
  gref = 0,
  data = staggered,
  cgroup = "never",
  fe = "vs",
  vcov = ~ district_id
)


## ----etwfe-summaries----------------------------------------------------------
# Simple ATT combines post-treatment effects, about 0.247 SD here
etwfe_simple <- emfx(model_etwfe_never, type = "simple")
etwfe_simple

# Average within each adoption group
etwfe_group <- emfx(model_etwfe_never, type = "group")
etwfe_group

# Average within each calendar year
etwfe_calendar <- emfx(model_etwfe_never, type = "calendar")
etwfe_calendar

# Average across groups at the same time since adoption
etwfe_event <- emfx(model_etwfe_never, type = "event")
etwfe_event


## ----etwfe-manual-------------------------------------------------------------
# Identify adoption groups without changing the downloaded dataset
staggered_manual <- staggered %>%
  mutate(
    g5 = as.integer(treatment_start_year == 5),
    g6 = as.integer(treatment_start_year == 6)
  )

# Include each cohort's year interactions, omitting its last untreated year
model_manual <- feols(
  outcome_std ~ i(year, g5, ref = 4) +
    i(year, g6, ref = 5) | district_id + year,
  data = staggered_manual,
  vcov = ~ district_id
)
summary(model_manual)
# year::5:g5 is ATT(5,5), about 0.150 SD
# The output includes both pre-treatment comparisons and treatment effects


## ----etwfe-comparison---------------------------------------------------------
# Estimate the single-coefficient TWFE model on the staggered panel
model_twfe_staggered <- feols(
  outcome_std ~ treated | district_id + year,
  data = staggered,
  vcov = ~ district_id
)
summary(model_twfe_staggered)




## ----session------------------------------------------------------------------
# Record R and package versions to help reproduce these results
sessionInfo()

