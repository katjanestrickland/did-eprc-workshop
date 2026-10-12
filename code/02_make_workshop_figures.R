#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath("code/02_make_workshop_figures.R", winslash = "/", mustWork = TRUE)
}

root_dir <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(root_dir, "data")
figure_dir <- file.path(root_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

theme_workshop <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

simple_data <- read_csv(file.path(data_dir, "simple_did_district.csv"), show_col_types = FALSE)
staggered_data <- read_csv(file.path(data_dir, "staggered_did_district.csv"), show_col_types = FALSE)
simple_means <- simple_data |>
  mutate(group = ifelse(cohort == "treated_year5", "Treated in year 5", "Never treated")) |>
  group_by(year, group) |>
  summarise(mean_outcome = mean(outcome), .groups = "drop")

p_parallel <- ggplot(simple_means, aes(x = year, y = mean_outcome, color = group)) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_x_continuous(breaks = 1:11) +
  labs(
    title = "Simple DID Setup: One District Cohort Adopts in Year 5",
    x = "Year",
    y = "Mean standardized math outcome",
    color = NULL
  ) +
  theme_workshop()

ggsave(file.path(figure_dir, "simple_parallel_trends.png"), p_parallel, width = 8, height = 4.8, dpi = 300, bg = "white")

did_cells <- simple_data |>
  mutate(
    group = ifelse(cohort == "treated_year5", "Treated", "Comparison"),
    period = ifelse(year < 5, "Pre", "Post")
  ) |>
  group_by(group, period) |>
  summarise(mean_outcome = mean(outcome), .groups = "drop") |>
  mutate(
    period = factor(period, levels = c("Pre", "Post")),
    group = factor(group, levels = c("Comparison", "Treated"))
  )

p_did <- ggplot(did_cells, aes(x = period, y = mean_outcome, group = group, color = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", mean_outcome)), vjust = -0.9, show.legend = FALSE) +
  labs(
    title = "DID Compares Changes, Not Just Levels",
    x = NULL,
    y = "Mean standardized math outcome",
    color = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.14, 0.24))) +
  theme_workshop()

ggsave(file.path(figure_dir, "simple_did_2x2.png"), p_did, width = 7, height = 4.8, dpi = 300, bg = "white")

comparison_pre <- did_cells |>
  filter(group == "Comparison", period == "Pre") |>
  pull(mean_outcome)
comparison_post <- did_cells |>
  filter(group == "Comparison", period == "Post") |>
  pull(mean_outcome)
treated_pre <- did_cells |>
  filter(group == "Treated", period == "Pre") |>
  pull(mean_outcome)
treated_post <- did_cells |>
  filter(group == "Treated", period == "Post") |>
  pull(mean_outcome)
treated_counterfactual_post <- treated_pre + (comparison_post - comparison_pre)

classic_did_lines <- tibble::tribble(
  ~series, ~period, ~mean_outcome,
  "Comparison observed", "Pre", comparison_pre,
  "Comparison observed", "Post", comparison_post,
  "Treated observed", "Pre", treated_pre,
  "Treated observed", "Post", treated_post,
  "Treated counterfactual", "Pre", treated_pre,
  "Treated counterfactual", "Post", treated_counterfactual_post
) |>
  mutate(
    period = factor(period, levels = c("Pre", "Post")),
    series = factor(
      series,
      levels = c("Comparison observed", "Treated observed", "Treated counterfactual")
    )
  )

did_effect_label <- treated_counterfactual_post + 0.5 * (treated_post - treated_counterfactual_post)

p_classic_did <- ggplot(classic_did_lines, aes(x = period, y = mean_outcome, group = series, color = series, linetype = series)) +
  geom_line(linewidth = 1.25) +
  geom_point(size = 3) +
  annotate(
    "segment",
    x = 2.08,
    xend = 2.08,
    y = treated_counterfactual_post,
    yend = treated_post,
    linewidth = 0.9,
    arrow = grid::arrow(length = grid::unit(0.16, "inches"), ends = "both"),
    color = "#B3435D"
  ) +
  annotate("text", x = 2.15, y = did_effect_label, label = "DID estimate", hjust = 0, size = 3.6, color = "#B3435D") +
  annotate("text", x = 1.52, y = treated_counterfactual_post + 0.025, label = "counterfactual trend", size = 3.2, color = "gray30") +
  scale_color_manual(
    values = c(
      "Comparison observed" = "#2454A6",
      "Treated observed" = "#117C78",
      "Treated counterfactual" = "#595959"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Comparison observed" = "solid",
      "Treated observed" = "solid",
      "Treated counterfactual" = "dashed"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.16, 0.26))) +
  labs(
    title = "Classic DID Picture: Observed Treated Outcome vs Counterfactual",
    x = NULL,
    y = "Mean standardized math outcome",
    color = NULL,
    linetype = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_workshop()

ggsave(file.path(figure_dir, "classic_did_counterfactual.png"), p_classic_did, width = 8, height = 4.8, dpi = 300, bg = "white")

event_time_means <- simple_data |>
  mutate(
    group = ifelse(cohort == "treated_year5", "Treated (entry year 5)", "Comparison (never treated)"),
    event_time_plot = year - 5
  ) |>
  group_by(event_time_plot, group) |>
  summarise(mean_outcome = mean(outcome), .groups = "drop") |>
  group_by(group) |>
  mutate(
    reference_outcome = mean_outcome[event_time_plot == -1],
    outcome_relative_to_reference = mean_outcome - reference_outcome
  ) |>
  ungroup()

event_y_max <- max(event_time_means$outcome_relative_to_reference)
event_y_min <- min(event_time_means$outcome_relative_to_reference)
event_y_text <- event_y_max + 0.09 * (event_y_max - event_y_min)

p_event_time <- ggplot(event_time_means, aes(x = event_time_plot, y = outcome_relative_to_reference, color = group)) +
  annotate("rect", xmin = -Inf, xmax = -0.5, ymin = -Inf, ymax = Inf, fill = "#EEF3F7", alpha = 0.65) +
  annotate("rect", xmin = -0.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#FFF4E6", alpha = 0.65) +
  geom_hline(yintercept = 0, color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray35") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  annotate("text", x = -2.7, y = event_y_text, label = "Pre-treatment:\ncheck trends", size = 3.4, color = "gray25") +
  annotate("text", x = 3.1, y = event_y_text, label = "Post-treatment:\ndynamic effects", size = 3.4, color = "gray25") +
  annotate("text", x = -1, y = 0.07, label = "reference year (-1)", size = 3.1, color = "gray25") +
  annotate("text", x = 0.55, y = -0.08, label = "treatment begins\n(k = 0)", size = 3.1, color = "gray25") +
  scale_x_continuous(breaks = -4:6) +
  labs(
    title = "Event-Time View: Outcomes Centered at Year -1",
    x = "Years relative to treatment start (0 = year 5)",
    y = "Mean outcome relative to year -1",
    color = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_workshop()

ggsave(file.path(figure_dir, "normalized_event_time_trends.png"), p_event_time, width = 8, height = 4.8, dpi = 300, bg = "white")

timing <- staggered_data |>
  distinct(district_id, cohort, treatment_start_year) |>
  mutate(
    cohort_label = case_when(
      cohort == "early_year5" ~ "Early adopters (30%)",
      cohort == "late_year8" ~ "Later adopters (20%)",
      TRUE ~ "Never treated (50%)"
    )
  ) |>
  count(cohort_label, treatment_start_year, name = "n_districts") |>
  mutate(
    cohort_label = factor(cohort_label, levels = c("Never treated (50%)", "Later adopters (20%)", "Early adopters (30%)")),
    y = as.numeric(cohort_label),
    untreated_end = ifelse(treatment_start_year > 0, treatment_start_year, 11),
    treated_start = ifelse(treatment_start_year > 0, treatment_start_year, NA_real_)
  )

p_timing <- ggplot(timing) +
  geom_segment(aes(x = 1, xend = untreated_end, y = y, yend = y), linewidth = 4, color = "#B8C2CC") +
  geom_segment(
    data = timing |> filter(treatment_start_year > 0),
    aes(x = treated_start, xend = 11, y = y, yend = y, color = cohort_label),
    linewidth = 4
  ) +
  geom_point(
    data = timing |> filter(treatment_start_year > 0),
    aes(x = treatment_start_year, y = y),
    size = 2.1,
    color = "#111111"
  ) +
  geom_text(
    data = timing |> filter(treatment_start_year > 0),
    aes(x = treatment_start_year, y = y + 0.18, label = paste0("adopts year ", treatment_start_year)),
    size = 3.3
  ) +
  scale_x_continuous(breaks = 1:11, limits = c(1, 11)) +
  scale_y_continuous(
    breaks = timing$y,
    labels = as.character(timing$cohort_label),
    limits = c(0.5, 3.6)
  ) +
  labs(
    title = "Preview for Staggered DID: Same Policy, Different Adoption Years",
    x = "Year",
    y = NULL,
    color = NULL
  ) +
  scale_color_manual(values = c("Early adopters (30%)" = "#F8766D", "Later adopters (20%)" = "#00BA38")) +
  theme_workshop() +
  theme(panel.grid.major.y = element_blank(), legend.position = "none")

ggsave(file.path(figure_dir, "staggered_timing_plot.png"), p_timing, width = 8, height = 5, dpi = 300, bg = "white")

staggered_means <- staggered_data |>
  mutate(
    group = case_when(
      cohort == "early_year5" ~ "Early entry: year 5",
      cohort == "late_year8" ~ "Later entry: year 8",
      TRUE ~ "Never treated"
    ),
    group = factor(group, levels = c("Never treated", "Later entry: year 8", "Early entry: year 5"))
  ) |>
  group_by(year, group) |>
  summarise(mean_outcome = mean(outcome), .groups = "drop")

p_staggered_trends <- ggplot(staggered_means, aes(x = year, y = mean_outcome, color = group)) +
  geom_vline(xintercept = c(5, 8), linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  annotate("text", x = 5, y = max(staggered_means$mean_outcome) + 0.04, label = "year 5", size = 3.4, hjust = -0.1) +
  annotate("text", x = 8, y = max(staggered_means$mean_outcome) + 0.04, label = "year 8", size = 3.4, hjust = -0.1) +
  scale_x_continuous(breaks = 1:11) +
  labs(
    title = "Staggered DID Preview: Early and Later Treatment Groups Over Time",
    x = "Year",
    y = "Mean standardized achievement outcome",
    color = NULL
  ) +
  theme_workshop()

ggsave(file.path(figure_dir, "staggered_group_trends.png"), p_staggered_trends, width = 8, height = 4.8, dpi = 300, bg = "white")

simple_formula <- lm(outcome ~ treated_ever * post, data = simple_data)
twfe_simple <- lm(outcome ~ treated + factor(district_id) + factor(year), data = simple_data)
twfe_staggered <- lm(outcome ~ treated + factor(district_id) + factor(year), data = staggered_data)

true_simple_att <- simple_data |> filter(treated == 1) |> summarise(value = mean(true_effect)) |> pull(value)
true_staggered_att <- staggered_data |> filter(treated == 1) |> summarise(value = mean(true_effect)) |> pull(value)

estimate_rows <- tibble::tribble(
  ~dataset, ~model, ~estimand, ~estimate, ~true_value,
  "simple_did_district", "2x2 DID regression", "Treated x Post", coef(simple_formula)[["treated_ever:post"]], true_simple_att,
  "simple_did_district", "District and year fixed effects", "Treatment indicator", coef(twfe_simple)[["treated"]], true_simple_att,
  "staggered_did_district", "Naive TWFE preview", "Treatment indicator", coef(twfe_staggered)[["treated"]], true_staggered_att
)

write_csv(estimate_rows, file.path(data_dir, "workshop_estimates.csv"))

cat("Created workshop figures under:", figure_dir, "\n")
