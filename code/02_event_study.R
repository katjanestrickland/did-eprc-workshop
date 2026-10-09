simple_data <- read.csv("assets/data/simple_did_district.csv")

event_times <- -4:6
event_times <- event_times[event_times != -1]

make_event_name <- function(k) {
  if (k < 0) {
    paste0("event_m", abs(k))
  } else {
    paste0("event_p", k)
  }
}

for (k in event_times) {
  simple_data[[make_event_name(k)]] <- as.integer(
    simple_data$cohort == "treated_year5" &
      simple_data$year - simple_data$treatment_start_year == k
  )
}

event_terms <- vapply(event_times, make_event_name, character(1))
event_formula <- as.formula(paste(
  "outcome ~",
  paste(event_terms, collapse = " + "),
  "+ factor(district_id) + factor(year)"
))

event_fit <- lm(event_formula, data = simple_data)
event_coefs <- coef(summary(event_fit))[event_terms, , drop = FALSE]

event_results <- data.frame(
  event_time = event_times,
  estimate = event_coefs[, "Estimate"],
  std_error = event_coefs[, "Std. Error"],
  row.names = NULL
)

event_results$conf_low <- event_results$estimate - 1.96 * event_results$std_error
event_results$conf_high <- event_results$estimate + 1.96 * event_results$std_error

print(event_results)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(event_results, ggplot2::aes(x = event_time, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray45") +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray45") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf_low, ymax = conf_high),
      width = 0.15,
      color = "#2454a6"
    ) +
    ggplot2::geom_point(size = 2.4, color = "#2454a6") +
    ggplot2::scale_x_continuous(breaks = event_times) +
    ggplot2::labs(
      title = "Event-Study Version of the Simple DID Example",
      x = "Years relative to treatment",
      y = "Estimated effect relative to year -1"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    "assets/figures/simple_event_study.png",
    p,
    width = 8,
    height = 4.8,
    dpi = 300,
    bg = "white"
  )
}
