library("tidyverse")

email_data <- read.csv("~/Hobby/cibook/input/email_data.csv")

head(email_data)

# Data Preparation
## Delete mails for women

male_df <- email_data %>%
  filter(segment != "Women E-Mail") %>%
  mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))

# Comparison with Grouping
summary_by_segment <- male_df %>%
  group_by(treatment) %>%
  summarise(conversion_rate = mean(conversion),
            spend_mean = mean(spend),
            count = n())

## Get the data for the group to which Mens E-Mails have been sent
mens_male <- male_df %>%
  filter(treatment == 1) %>%
  pull(spend)

## Get the data for the group to which E-Mails have not been sent
no_mail <- male_df %>%
  filter(treatment == 0) %>%
  pull(spend)

## Execute t-test
rcc_ttest <- t.test(mens_male, no_mail, var.equal = TRUE)

# Create selection-biased dataset
set.seed(1)

obs_rate_c <- 0.5
obs_rate_t <- 0.5

## Create biased data
biased_data <- male_df %>%
  mutate(obs_rate_c = if_else(
    (history > 300) | (recency < 6) | (channel == "Multichannel"),
    obs_rate_c, 1),
    obs_rate_t = if_else(
      (history > 300) | (recency < 6) | (channel == "Multichannel"),
    1, obs_rate_t),
    random_number = runif(n = NROW(male_df))) %>%
  filter((treatment == 0 & random_number < obs_rate_c) |
         (treatment == 1 & random_number < obs_rate_t))

# Compare the mean in biased data
summary_by_segment_biased = biased_data %>%
  group_by(treatment) %>%
  summarise(conversion_rate = mean(conversion),
            spend_mean = mean(spend),
            count = n())

mens_mail_biased <- biased_data %>%
  filter(treatment == 1) %>%
  pull(spend)

no_mail_biased <- biased_data %>%
  filter(treatment == 0) %>%
  pull(spend)

rcc_ttest_biased <- t.test(mens_mail_biased, no_mail_biased, var.equal = TRUE)
