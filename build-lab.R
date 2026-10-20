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
  mean(d$outcome_std[d$treatment_start_year == g & d$year == t]) -
    mean(d$outcome_std[d$treatment_start_year == g & d$year == baseline]) -
    mean(d$outcome_std[d$treatment_start_year == 0 & d$year == t]) +
    mean(d$outcome_std[d$treatment_start_year == 0 & d$year == baseline])
}
stopifnot(abs(contrast(lab$simple, 5, 5, 4) - coef(lab$model_2x2)["treated"]) < 1e-8)
manual <- mapply(function(g,t) contrast(lab$staggered,g,t,g-1), post$cohort, post$year)
stopifnot(max(abs(manual - post$ATT)) < 1e-8)
cat("Texas sample sizes and manual DID contrasts verified.\n")
for (name in c("simple_did_district", "staggered_did_district")) {
  input <- readr::read_csv(paste0("assets/data/", name, ".csv"), show_col_types=FALSE)
  stopifnot(identical(names(input), c("district_id", "year", "treatment_start_year", "outcome_std", "treated_ever", "treated")),
            !anyNA(input), nrow(input) == 5824,
            !anyDuplicated(input[c("district_id", "year")]),
            all(table(input$district_id) == 7),
            all(input$treated_ever == as.integer(input$treatment_start_year > 0)),
            all(input$treated == as.integer(input$treatment_start_year > 0 & input$year >= input$treatment_start_year)))
  expected <- if (name == "simple_did_district") c(`0`=416, `5`=416) else c(`0`=416, `5`=208, `6`=208)
  counts <- table(input$treatment_start_year[input$year == 1])
  stopifnot(identical(names(counts), names(expected)), all(as.numeric(counts) == expected))
}
group_manual <- tapply(post$ATT, post$cohort, mean)
calendar_manual <- tapply(post$ATT, post$year, mean)
stopifnot(max(abs(group_manual[as.character(lab$group_att$egt)] - lab$group_att$att.egt)) < 1e-8,
          max(abs(calendar_manual[as.character(lab$calendar_att$egt)] - lab$calendar_att$att.egt)) < 1e-8,
          abs(mean(group_manual) - lab$group_att$overall.att) < 1e-8,
          abs(mean(calendar_manual) - lab$calendar_att$overall.att) < 1e-8)
stopifnot(abs(lab$g5 - lab$group_att$att.egt[lab$group_att$egt == 5]) < 1e-8,
          abs(lab$g6 - lab$group_att$att.egt[lab$group_att$egt == 6]) < 1e-8,
          abs(mean(c(lab$c5,lab$c6,lab$c7)) - lab$calendar_att$overall.att) < 1e-8)
cat("Six-column inputs and all worked aggregation calculations verified.\n")
dynamic_manual <- tapply(post$ATT, post$year - post$cohort, mean)
dynamic_post <- lab$dynamic_att$egt >= 0
stopifnot(max(abs(dynamic_manual[as.character(lab$dynamic_att$egt[dynamic_post])] -
                   lab$dynamic_att$att.egt[dynamic_post])) < 1e-8,
          abs(mean(dynamic_manual) - lab$dynamic_att$overall.att) < 1e-8,
          max(abs(unname(dynamic_manual) - c(lab$d0, lab$d1, lab$d2))) < 1e-8)
cat("Dynamic estimates and overall dynamic average verified.\n")
print(dynamic_manual)
post_index <- which(lab$model_csa$t >= lab$model_csa$group)
post_index <- post_index[order(lab$model_csa$group[post_index], lab$model_csa$t[post_index])]
stopifnot(
  isTRUE(lab$model_csa$DIDparams$cband),
  lab$model_csa$alp == 0.05,
  max(abs(lab$post_att$SE - lab$model_csa$se[post_index])) < 1e-8,
  max(abs(lab$post_att$lower_95_sim -
            (lab$model_csa$att - lab$model_csa$c * lab$model_csa$se)[post_index])) < 1e-8,
  max(abs(lab$post_att$upper_95_sim -
            (lab$model_csa$att + lab$model_csa$c * lab$model_csa$se)[post_index])) < 1e-8
)
cat("Displayed standard errors and simultaneous bands match the model summary.\n")
