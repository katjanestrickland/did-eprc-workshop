#!/usr/bin/env Rscript
# Execute the page against published local CSVs.
lab <- new.env(parent = globalenv())
knitr::knit("workshop.Rmd", output = "workshop-generated.md", envir = lab)
knitr::purl("workshop.Rmd", output = "code/workshop_lab.R", documentation = 1)
cells <- data.frame(cohort=lab$model_csa$group, year=lab$model_csa$t, ATT=lab$model_csa$att)
post <- cells[cells$year >= cells$cohort, ]
stopifnot(nrow(post) == 5, abs(mean(post$ATT) - lab$overall_csa$overall.att) < 1e-8)
cat("Lab executed and aggregation check passed.\n")
