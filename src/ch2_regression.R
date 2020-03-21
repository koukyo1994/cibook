library("tidyverse")
library("broom")

email_data <- read.csv("~/Hobby/cibook/input/email_data.csv")

head(email_data)

## Create biased data
# Create selection-biased dataset
male_df <- email_data %>%
  filter(segment != "Womens E-Mail") %>%
  mutate(treatment = if_else(segment == "Mens E-Mail", 1, 0))

set.seed(1)

obs_rate_c <- 0.5
obs_rate_t <- 0.5

biased_data <- male_df %>%
  mutate(
    obs_rate_c = if_else(
      (history > 300) | (recency < 6) | (channel == "Multichannel"),
      obs_rate_c, 1),
    obs_rate_t = if_else(
      (history > 300) | (recency < 6) | (channel == "Multichannel"),
      1, obs_rate_t),
    random_number = runif(n = NROW(male_df))) %>%
  filter((treatment == 0 & random_number < obs_rate_c) |
           (treatment == 1 & random_number < obs_rate_t))

## Regression on biased data

### execute regression
biased_reg <- lm(data = biased_data, 
                 formula = spend ~ treatment + history)
summary(biased_reg)

# get the inferenced coefficients
biased_reg_coef <- tidy(biased_reg)

## compare regression result of rct and biased
### regression on rct
rct_reg <- lm(data = male_df,
              formula = spend ~ treatment)
rct_reg_coef <- summary(rct_reg) %>% tidy()

### regression on biased data
nonrct_reg <- lm(data = biased_data,
                 formula = spend ~ treatment)
nonrct_reg_coef <- summary(nonrct_reg) %>% tidy()

rct_reg_coef
nonrct_reg_coef

## compare regression result of rct and biased using covariant
### regression on rct
rct_mreg <- lm(data = male_df,
               formula = spend ~ treatment + recency + channel + history)
rct_mreg_coef <- summary(rct_mreg) %>% tidy()

### regression on biased data
nonrct_mreg <- lm(data = biased_data,
                  formula = spend ~ treatment + recency + channel + history)
nonrct_mreg_coef <- summary(nonrct_mreg) %>% tidy()

rct_mreg_coef
nonrct_mreg_coef

## Check Omitted Variable Bias
### vectorize model
formula_vec <- c(spend ~ treatment + recency + channel,  # model A
                 spend ~ treatment + recency + channel + history,  # model B
                 history ~ treatment + recency + channel)  # model C
### name formulas
names(formula_vec) <- paste("reg", LETTERS[1:3], sep="_")

### make models vector into dataframe
models <- formula_vec %>%
  enframe(name = "model_index", value = "formula")

### execute regression at the same time
df_models <- models %>%
  mutate(model = map(.x = formula, .f = lm, data = biased_data)) %>%
  mutate(lm_result = map(.x = model, .f = tidy))

### clean the result
df_results <- df_models %>%
  mutate(formula = as.character(formula)) %>%
  select(formula, model_index, lm_result) %>%
  unnest(cols = c(lm_result))

df_results

### get estimated parameter of treatment of model A, B, C
treatment_coef <- df_results %>%
  filter(term == "treatment") %>%
  pull(estimate)

history_coef <- df_results %>%
  filter(model_index == "reg_B", term == "history") %>%
  pull(estimate)

### check OVB
OVB <- history_coef * treatment_coef[3]
coef_gap <- treatment_coef[1] - treatment_coef[2]

OVB
coef_gap

## add variable that should not be included
cor_visit_treatment <- lm(data = biased_data,
                          formula = treatment ~ visit + channel + recency + history) %>%
  tidy()
cor_visit_treatment

### execute regression adding visit as variable
bad_control_reg <- lm(data = biased_data,
                      formula = spend ~ treatment + channel + recency + history + visit) %>%
  tidy()
bad_control_reg