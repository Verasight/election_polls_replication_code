# Replication code for Enns, Goranson, Rothschild, and Streett (2025)
# Jake Rothschild and Gretchen Streett 2025-05-06

# load required packages
if (! require(pacman)) install.packages('pacman')
pacman::p_load(tidyverse)

# data setup ----

# read in polls and filter to 2024
polls <- read_csv('data/president_polls_historical.csv') %>%
  filter(cycle == 2024)

# filter to national polls
polls_nat_long <- polls %>% dplyr::filter(is.na(state))

# reshape to wide
polls_nat <- polls_nat_long %>%
  pivot_wider(names_from = answer, values_from = pct,
              values_fn = sum,
              id_cols = c(pollster, methodology, poll_id, question_id,
                          population, start_date, end_date, url, sponsors,
                          transparency_score)) 

# calculate Trump - Harris margin and error
polls_nat <- polls_nat %>%
  mutate(trump_margin = Trump - Harris,
         error  = trump_margin - 1.5)

# filter to polls starting 2024-09-01
polls_report <- polls_nat %>%
  filter(mdy(start_date) >= mdy("9/1/2024"))

# filter to polls with Harris and Trump results; keep one result per poll
polls_report <- polls_report %>%
  filter(!is.na(Harris), !is.na(Trump)) %>%
  mutate(row_id = 1:nrow(.)) %>%
  mutate(across(Harris:`P. Murphy`,
                ~!is.na(.x),
                .names = "{.col}_present")) %>%
  rowwise() %>%
  mutate(cand_count = sum(c_across(Harris_present:`P. Murphy_present`))) %>%
  ungroup() %>%
  group_by(poll_id, population) %>%
  slice_min(cand_count) %>%
  slice_max(question_id) %>%
  ungroup() %>%
  arrange(row_id)

# manually code methodology as prob/nonprob
polls_report <- polls_report %>%
  mutate(
    method_prob = case_when(
      methodology %in% c(
        'App Panel',
        'Online Ad',
        'Online Matched Sample',
        'Online Opt-In Panel',
        'IVR/Live Phone/Online Opt-In Panel',
        'IVR/Online Opt-In Panel',
        'IVR/Online Opt-In Panel/Text-to-Web',
        'Live Phone/Online Opt-In Panel/App Panel',
        'Live Phone/Online Opt-In Panel/Text',
        'Live Phone/Online Opt-In Panel/Text-to-Web',
        'Online Opt-In Panel/Text-to-Web',
        'Text-to-Web/Online Ad'
      ) ~ 'nonprobability',
      methodology %in% c(
        'IVR',
        'IVR/Text',
        'IVR/Text-to-Web',
        'Live Phone',
        'Live Phone/Text-to-Web',
        'Probability Panel',
        'Text-to-Web'
      ) ~ 'probability',
      poll_id %in% c(88174, 88456, 89134) ~ 'nonprobability')
  )

# filter to LV polls with non-missing coding
polls_lv <- filter(polls_report, population == 'lv', ! is.na(method_prob))

# summary analysis ----
polls_lv %>% 
  group_by(method_prob) %>%
  summarize(surveys = n(), mean_error = mean(error))

# analysis with hand-coded AAPOR TI and Roper contributor status ----
hand_coded <- read_csv("data/pollsters-aapor-roper-hand-coded.csv")
polls_lv <- left_join(polls_lv, hand_coded) %>% 
  mutate(
    method_ar = case_when(
            method_prob == 'probability' ~ 'probability',
            method_prob == 'nonprobability' & (aapor == 'Yes' | roper == 'Yes') ~
              'nonprobability AAPOR/Roper',
            TRUE ~ 'nonprobability non-AAPOR/Roper'
          ),
    method_ar2 = case_when(
      method_prob == 'probability' & (aapor == 'Yes' | roper == 'Yes') ~ 
        'probability AAPOR/Roper',
      method_prob == 'probability' ~ "probability non-AAPOR/Roper",
      method_prob == 'non-probability' & (aapor == 'Yes' | roper == 'Yes') ~ 
        'non-probability AAPOR/Roper',
      TRUE ~ 'non-probability non-AAPOR/Roper'
    )
  )

polls_lv %>% 
  group_by(method_prob) %>%
  summarize(surveys = n(), mean_error = mean(error)) %>% 
  mutate(grouping = 'nonprobability polls together') %>% 
  bind_rows(
    polls_lv %>% 
      group_by(method_prob = method_ar) %>% 
      summarize(surveys = n(), mean_error = mean(error)) %>%
      mutate(grouping = 'nonprobability polls split')
  )

# significance testing ----
# mean error by prob/nonprob
summary(lm(error ~ method_prob, data = polls_lv))

# mean error by AAPOR/Roper status among nonprob polls
summary(lm(error ~ method_ar, 
           data = filter(polls_lv, method_prob == 'nonprobability')))

# mean error by AAPOR/Roper status among prob polls
summary(lm(error ~ method_ar2, 
           data = filter(polls_lv, method_prob == 'probability')))

# ANES comparison ----
anes_error <- 5.7

summarize(polls_lv, pct_worse = mean(abs(anes_error) > abs(error)))
