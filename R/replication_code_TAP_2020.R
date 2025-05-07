library(haven)
library(dplyr)
library(survey)

# Data can be downloaded from the Roper center here:
#.   https://doi.org/10.25940/ROPER-31122294 

tap2020_data <- read_dta(file.choose())

# Combine Q2 and Q2A to create vote variable 
tap2020_data <- tap2020_data %>%
  mutate(
    vote = Q2,  # Start with Q2
    vote = if_else(Q2 >= 3 & Q2 <= 98 & Q2A >= 1 & Q2A <= 2, Q2A, vote),
    vote = if_else(vote == 98, NA_real_, vote)  # recode 98 as missing
  )

# Survey design: Random sampling with weights 
tap2020_data$Q1_num <- as.numeric(tap2020_data$Q1)
tap2020_design <- svydesign(
  ids = ~1,
  weights = ~weight,
  data = tap2020_data
)

# Analysis: Likely voters 
subset_design <- subset(tap2020_design, Q1_num >= 9 & Q1_num <= 11)
subset_design_nomiss <- subset(subset_design, !is.na(vote))

vote_table <- svymean(~factor(vote), design = subset_design_nomiss)
confint(vote_table)

# calculate error 
props <- coef(vote_table)

# Trump - Biden difference
error <- (46.90 - 51.25) - 100 * (props[2] - props[1])
error
