#!/usr/bin/env Rscript
# Execute the page against published local CSVs.
lab <- new.env(parent = globalenv())
knitr::knit("workshop.Rmd", output = "workshop-generated.md", envir = lab)
knitr::purl("workshop.Rmd", output = "code/workshop_lab.R", documentation = 1)
cells <- data.frame(cohort=lab$model_csa$group, year=lab$model_csa$t, ATT=lab$model_csa$att)
post <- cells[cells$year >= cells$cohort, ]
stopifnot(nrow(post) == 5, abs(mean(post$ATT) - lab$overall_csa$overall.att) < 1e-8)
cat("Lab executed and aggregation check passed.\n")
stopifnot(nrow(lab$simple) == 5824, nrow(lab$staggered) == 5824,
          length(unique(lab$simple$district_id)) == 832)
contrast <- function(d, g, t, baseline) {
  mean(d$outcome_raw[d$treatment_start_year == g & d$year == t]) -
    mean(d$outcome_raw[d$treatment_start_year == g & d$year == baseline]) -
    mean(d$outcome_raw[d$treatment_start_year == 0 & d$year == t]) +
    mean(d$outcome_raw[d$treatment_start_year == 0 & d$year == baseline])
}
stopifnot(abs(contrast(lab$simple, 5, 5, 4) - coef(lab$model_2x2)["treated"]) < 1e-8)
manual <- mapply(function(g,t) contrast(lab$staggered,g,t,g-1), post$cohort, post$year)
stopifnot(max(abs(manual - post$ATT)) < 1e-8)
cat("Texas sample sizes and manual DID contrasts verified.\n")
