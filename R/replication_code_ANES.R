# Reproducible code - ANES Analysis 

library(haven)
library(dplyr)
library(survey)
library(lubridate)

# April 30th 2025 edition of the preliminary release files available for 
#.   Download here: https://electionstudies.org/data-center/2024-time-series-study/
data <- read_csv(file.choose())

# Combine V241043 and V241039 for vote variable
data <- data %>%
  mutate(
    vote = if_else(V241043 > 0, V241043, NA_real_),
    vote = if_else(is.na(vote) & V241039 > 0, V241039, vote)
  )

# Combine V243051 and V243052 for completion date
data <- data %>%
  mutate(
    completedate = if_else(trimws(V243051) != ".", V243051, V243052),
    completedate = if_else(trimws(completedate) == ".", NA_character_, completedate),
    date = ymd(completedate)
  )

# Create Survey Design
data <- data %>% filter(! is.na(V240105c))

design <- svydesign(
  ids = ~V240105c,
  strata = ~V240105d,
  weights = ~V240105a,
  data = data,
  nest = TRUE
)



# Analysis: Vote Intentions from Sept. 1 to Election Day ------------------


# Filter data
subset_design <- subset(design, date >= ymd("2024-09-01"))
subset_design_nomiss <- subset(subset_design, !is.na(vote))

# Weighted tabulation
svy_table <- svymean(~factor(vote), design = subset_design_nomiss)
svy_table
confint(svy_table)

# Extract proportions
props <- coef(svy_table)
error <- (49.91 - 48.43) - 100 * (props[2] - props[1])
error


# Analysis: All Pre-Election Responses ------------------------------------

design_nomiss <- subset(design, !is.na(vote))
svy_table_all <- svymean(~factor(vote), design = design_nomiss)
confint(svy_table_all)

props_all <- coef(svy_table_all)
error_all <- (49.71 - 48.24) - 100 * (props_all[2] - props_all[1])
error_all

tab_all <- data %>% count(vote)
