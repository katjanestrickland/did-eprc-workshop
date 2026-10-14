## ----packages-----------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(fixest)
library(did)
library(etwfe)
library(knitr)
set.seed(20261014)


## ----import-------------------------------------------------------------------
read_workshop_data <- function(filename) {
  candidates <- c(file.path("assets/data", filename), file.path("data", filename), filename)
  local <- candidates[file.exists(candidates)]
  source <- if (length(local)) local[1] else paste0(
    "https://katjanestrickland.github.io/did-eprc-workshop/assets/data/", filename)
  read_csv(source, show_col_types = FALSE)
}
simple <- read_workshop_data("simple_did_district.csv")
staggered <- read_workshop_data("staggered_did_district.csv")
kable(head(simple), digits = 2)


## ----descriptives, echo=FALSE-------------------------------------------------
design <- bind_rows(
  mutate(simple, simulation = "1: Common start"),
  mutate(staggered, simulation = "2: Staggered start")
) |>
  group_by(simulation, treatment_start_year) |>
  summarise(districts = n_distinct(district_id), rows = n(),
            mean_score = mean(outcome_raw), sd_score = sd(outcome_raw), .groups = "drop")
kable(design, digits = 2)
stopifnot(nrow(simple) == 1120, nrow(staggered) == 1120,
          all(table(simple$district_id) == 7), all(table(staggered$district_id) == 7))


## ----simple-trends, echo=FALSE------------------------------------------------
simple_means <- simple |>
  mutate(group = ifelse(treated_ever == 1, "Adopts in Year 5", "Never treated")) |>
  group_by(group, year) |>
  summarise(mean_score = mean(outcome_raw), .groups = "drop")
ggplot(simple_means, aes(year, mean_score, color = group)) +
  geom_vline(xintercept = 4.5, linetype = 2, color = "grey55") +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_x_continuous(breaks = 1:7) +
  scale_color_manual(values = c("#2454a6", "#117c78")) +
  labs(x = "Study year", y = "Mean achievement (raw score points)", color = NULL) +
  theme_minimal(base_size = 13) + theme(legend.position = "bottom")


## ----twfe---------------------------------------------------------------------
twfe <- feols(
  outcome_raw ~ treated | district_id + year,
  data = simple,
  vcov = ~ district_id
)


## ----twfe-summary-------------------------------------------------------------
summary(twfe)


## ----twfe-table, echo=FALSE---------------------------------------------------
twfe_ci <- confint(twfe, "treated")
twfe_table <- data.frame(
  estimate = coef(twfe)["treated"], std_error = se(twfe)["treated"],
  lower_95 = twfe_ci[1, 1], upper_95 = twfe_ci[1, 2])
kable(twfe_table, digits = 3, row.names = FALSE)


## ----manual-did---------------------------------------------------------------
changes <- simple |>
  group_by(treated_ever) |>
  summarise(change = mean(outcome_raw[year >= 5]) - mean(outcome_raw[year < 5]))
manual_did <- changes$change[changes$treated_ever == 1] - changes$change[changes$treated_ever == 0]
stopifnot(abs(manual_did - coef(twfe)["treated"]) < 1e-8)
manual_did


## ----timing, echo=FALSE-------------------------------------------------------
timing <- staggered |>
  distinct(cohort, treatment_start_year, year, treated)
ggplot(timing, aes(year, reorder(cohort, -treatment_start_year), fill = factor(treated))) +
  geom_tile(color = "white", width = 0.94, height = 0.75) +
  scale_x_continuous(breaks = 1:7, position = "top") +
  scale_fill_manual(values = c("#e1e7eb", "#2454a6"), labels = c("Untreated", "Treated")) +
  labs(x = "Study year", y = NULL, fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(), legend.position = "bottom")


## ----staggered-trends, echo=FALSE---------------------------------------------
staggered_means <- staggered |>
  group_by(cohort, year) |>
  summarise(mean_score = mean(outcome_raw), .groups = "drop")
ggplot(staggered_means, aes(year, mean_score, color = cohort)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  scale_x_continuous(breaks = 1:7) +
  scale_color_manual(values = c("#5b6773", "#2454a6", "#117c78", "#b3435d")) +
  labs(x = "Study year", y = "Mean achievement (raw score points)", color = NULL) +
  theme_minimal(base_size = 13) + theme(legend.position = "bottom")


## ----csa----------------------------------------------------------------------
set.seed(20261014)
cs <- att_gt(
  yname = "outcome_raw",
  tname = "year",
  idname = "district_id",
  gname = "treatment_start_year",
  xformla = ~ 1,
  data = as.data.frame(staggered),
  panel = TRUE,
  control_group = "nevertreated",
  base_period = "universal",
  anticipation = 0,
  est_method = "reg",
  bstrap = TRUE,
  biters = 999,
  clustervars = "district_id",
  cband = TRUE
)


## ----csa-summary--------------------------------------------------------------
summary(cs)


## ----csa-table, echo=FALSE----------------------------------------------------
cells <- tibble(cohort = cs$group, year = cs$t, ATT = cs$att, SE = cs$se) |>
  filter(year >= cohort)
kable(cells, digits = 3)


## ----aggregation--------------------------------------------------------------
simple_att <- aggte(cs, type = "simple")
summary(simple_att)


## ----aggregation-table, echo=FALSE--------------------------------------------
cohort_sizes <- staggered |>
  distinct(district_id, treatment_start_year) |>
  count(treatment_start_year, name = "districts")
weighted_cells <- cells |>
  left_join(cohort_sizes, by = c("cohort" = "treatment_start_year")) |>
  mutate(weight = districts / sum(districts), contribution = weight * ATT)
kable(select(weighted_cells, cohort, year, districts, ATT, weight, contribution), digits = 3)
manual_simple_att <- sum(weighted_cells$contribution)
stopifnot(abs(manual_simple_att - simple_att$overall.att) < 1e-8)


## ----manual-cells-------------------------------------------------------------
manual_cell <- function(g, t) {
  treated_change <- with(staggered, mean(outcome_raw[treatment_start_year == g & year == t]) -
                          mean(outcome_raw[treatment_start_year == g & year == g-1]))
  control_change <- with(staggered, mean(outcome_raw[treatment_start_year == 0 & year == t]) -
                          mean(outcome_raw[treatment_start_year == 0 & year == g-1]))
  treated_change - control_change
}
manual_effects <- mapply(manual_cell, cells$cohort, cells$year)
stopifnot(max(abs(manual_effects - cells$ATT)) < 1e-8)


## ----etwfe--------------------------------------------------------------------
extended <- etwfe(outcome_raw ~ 0, tvar = year, gvar = treatment_start_year,
                  data = staggered, cgroup = "never", gref = 0,
                  vcov = ~district_id)
extended_att <- emfx(extended, type = "simple")
kable(as.data.frame(extended_att)[, c("estimate", "std.error", "conf.low", "conf.high")], digits = 3)


## ----event-study--------------------------------------------------------------
event_model <- feols(outcome_raw ~ i(year, treated_ever, ref = 4) | district_id + year,
                     data = simple, vcov = ~district_id)


## ----event-study-plot, echo=FALSE---------------------------------------------
iplot(event_model, xlab = "Study year (Year 4 reference)",
      ylab = "Effect relative to Year 4 (raw points)",
      main = "Simulation 1: common-start event study")


## ----session------------------------------------------------------------------
sessionInfo()

