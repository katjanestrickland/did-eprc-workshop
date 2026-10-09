simple_data <- read.csv("assets/data/simple_did_district.csv")

did_cells <- aggregate(
  outcome ~ treated_ever + post,
  data = simple_data,
  FUN = mean
)

print(did_cells)

did_fit <- lm(outcome ~ treated_ever * post, data = simple_data)
summary(did_fit)

twfe_fit <- lm(
  outcome ~ treated + factor(district_id) + factor(year),
  data = simple_data
)
summary(twfe_fit)
