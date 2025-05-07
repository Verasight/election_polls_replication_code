# Reproducible Code - TAP Analysis

library(haven)
library(dplyr)
library(survey)
library(broom)

# Data can be downloaded from the Roper center here:
#.   https://doi.org/10.25940/ROPER-31122294 
tap_data <- read_dta(file.choose())

# Combine two vote intention questions 
tap_data <- tap_data %>%
  mutate(
    vote = if_else(!is.na(q87), q87, q88)
  )

# Survey design: Random sampling with weights 
tap_design <- svydesign(
  ids = ~1,
  weights = ~weight,
  data = tap_data
)

# Analysis: Overall Vote Intentions (All Respondents)
tap_design_nomiss <- subset(tap_design, !is.na(vote))

# Tabulate
vote_table <- svymean(~factor(vote), design = tap_design_nomiss)
confint(vote_table)

# Calculate Error 
props <- coef(vote_table)

error_overall <- (49.71 - 48.24) - 100 * (props[2] - props[1])
error_overall

# Analysis: Likely Voters
likely_design <- subset(tap_design_nomiss, q89 >= 1 & q89 <= 2)

# Tabulate among likely voters
likely_vote_table <- svymean(~factor(vote), design = likely_design)
confint(likely_vote_table)

# Calculate Error
props_likely <- coef(likely_vote_table)

# 2024 general election results pulled from this link: 
#.   https://uselectionatlas.org/RESULTS/index.html
error_likely <- (49.71 - 48.24) - 100 * (props_likely[2] - props_likely[1])
error_likely
