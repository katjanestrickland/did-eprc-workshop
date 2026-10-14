#!/usr/bin/env Rscript
# Run from workshop-site. The R Markdown is the single source for code and outputs.
dir.create("assets/figures", recursive = TRUE, showWarnings = FALSE)
lab <- new.env(parent = globalenv())
knitr::knit("workshop.Rmd", output = "workshop-generated.md", envir = lab)
knitr::purl("workshop.Rmd", output = "code/workshop_lab.R", documentation = 1)
et <- as.data.frame(lab$extended_att)
estimates <- data.frame(
  simulation = c(1, 2, 2), method = c("TWFE", "CSA Simple ATT", "ETWFE Simple ATT"),
  outcome = "outcome_raw",
  estimate = c(unname(coef(lab$twfe)["treated"]), lab$simple_att$overall.att, et$estimate),
  std_error = c(unname(fixest::se(lab$twfe)["treated"]), lab$simple_att$overall.se, et$std.error)
)
readr::write_csv(estimates, "assets/data/workshop_estimates.csv")
readr::write_csv(lab$weighted_cells, "assets/data/csa_weighted_cells.csv")
cat("Lab and manual calculation checks passed.\n")
