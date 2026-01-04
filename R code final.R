

# 0. Packages
library(haven)        # read_sav
library(dplyr)        # data wrangling
library(lme4)         # multilevel models
library(broom.mixed)  # tidy() for lme4 models
library(ggplot2)      # plots
library(knitr)        # kable() for LaTeX tables
library(forcats)      # factor helpers

# 1. Load and prepare data 
gss <- read_sav("C:/Users/GSS_subset.sav")

gss_hier <- gss 
  filter(year c(1996, 2004, 2014, 2022, 2024))
  mutate(
    # Factors
    region_f = as_factor(region),
    race_f   = as_factor(race),
    sex_f    = as_factor(sex),   # not signifficant
    year_f = relevel(factor(year), ref = "2014"),
    
    # Numeric versions
    educ_num   = as.numeric(educ),
    age_num    = as.numeric(age),
    income_num = as.numeric(income)
  )

# Within-/between-region education (contextual effects)
gss_hier <- gss_hier 
  group_by(region_f) 
  mutate(
    region_mean_educ = mean(educ_num, na.rm = TRUE)
  ) 
  ungroup() 
  mutate(
    educ_c_region       = educ_num - region_mean_educ,
    region_mean_educ_c  = region_mean_educ - mean(region_mean_educ, na.rm = TRUE),
    educ_w              = as.numeric(scale(educ_c_region)),      # within-region (z)
    educ_b              = as.numeric(scale(region_mean_educ_c)), # between-region (z)
    age10               = (age_num - mean(age_num, na.rm = TRUE)) / 10,
    income_z            = as.numeric(scale(income_num))
  )

# Recode outcome variables

gss_hier <- gss_hier 
  mutate(
    immjobs_num  = as.numeric(immjobs),
    immameco_num = as.numeric(immameco),
    letin_num    = as.numeric(letin1),
    amcit_num    = as.numeric(amcitizn),
    
    # IMMJOBS – Immigrants take jobs away (negative if agree)
    immjobs_anti = case_when(
      immjobs_num c(1, 2) ~ 1,        # agree strongly / agree → negative
      immjobs_num c(3, 4, 5) ~ 0,     # neutral / disagree → not negative
      TRUE ~ NA_real_
    ),
    
    # IMMAMECO – Immigrants good for America (negative if disagree)
    immameco_anti = case_when(
      immameco_num c(4, 5) ~ 1,       # disagree / strongly disagree → negative
      immameco_num c(1, 2, 3) ~ 0,    # agree / neutral → not negative
      TRUE ~ NA_real_
    ),
    
    # LETIN1 – number of immigrants should be… (negative if reduced)
    letin_restrict = case_when(
      letin_num c(4, 5) ~ 1,          # reduced a little / a lot → restrictive
      letin_num c(1, 2, 3) ~ 0,       # increased / same → not restrictive
      TRUE ~ NA_real_
    ),
    
    # AMCITIZN – would rather be a citizen of America (high vs weaker attachment)
    cit_attach = case_when(
      amcit_num c(1, 2) ~ 1,          # strongly agree / agree → strong attachment
      amcit_num c(3, 4, 5) ~ 0,       # neutral / disagree → weaker attachment
      TRUE ~ NA_real_
    )
  )

# 2. Descriptive tables and summary stats

# Sample sizes and outcome proportions by year (attitudes_year)
attitudes_year <- gss_hier %>%
  group_by(year) %>%
  summarise(
    n           = n(),
    p_immjobs   = mean(immjobs_anti, na.rm = TRUE),
    p_immameco  = mean(immameco_anti, na.rm = TRUE),
    p_letin     = mean(letin_restrict, na.rm = TRUE),
    p_citattach = mean(cit_attach, na.rm = TRUE)
  ) %>%
  ungroup()

# LaTeX Table: descriptive_outcomes
kable(
  attitudes_year 
    mutate(
      `Jobs threat (%)`          = round(100 * p_immjobs, 1),
      `Economic threat (%)`      = round(100 * p_immameco, 1),
      `Restrict immigration (%)` = round(100 * p_letin, 1),
      `Citizenship attachment (%)` = round(100 * p_citattach, 1)
    ) 
    select(
      Year = year,
      N = n,
      `Jobs threat (%)`,
      `Economic threat (%)`,
      `Restrict immigration (%)`,
      `Citizenship attachment (%)`
    ),
  format   = "latex",
  booktabs = TRUE,
  caption  = "Proportion of respondents expressing anti-immigrant positions by year.",
  label    = "descriptive_outcomes"
)

# Descriptive statistics for key predictors
descriptive_predictors <- gss_hier 
  summarise(
    N         = n(),
    mean_educ = mean(educ_num, na.rm = TRUE),
    sd_educ   = sd(educ_num, na.rm = TRUE),
    mean_age  = mean(age_num, na.rm = TRUE),
    sd_age    = sd(age_num, na.rm = TRUE),
    mean_inc  = mean(income_num, na.rm = TRUE),
    sd_inc    = sd(income_num, na.rm = TRUE)
  )

kable(
  descriptive_predictors,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Descriptive statistics for key predictors (pooled sample).",
  label    = "descriptive_predictors"
)

#3. Construct analysis datasets

# IMMJOBS: all waves with non-missing outcome
jobs_all <- gss_hier 
  filter(!is.na(immjobs_anti))

# IMMJOBS: 2022–2024 subset
jobs_recent <- gss_hier 
  filter(year c(2022, 2024)) 
  mutate(
    year_f = relevel(factor(year), ref = "2022")
  )

# IMMJOBS: income-complete subset (2022–2024)
jobs_income <- jobs_recent 
  filter(!is.na(income_z))

# IMMAMECO: waves with that item (1996, 2004, 2014, 2024)
eco_all <- gss_hier 
  filter(!is.na(immameco_anti))

# LETIN1: waves with that item (1996, 2004, 2014, 2024)
letin_all <- gss_hier 
  filter(!is.na(letin_restrict))

# AMCITIZN: waves with that item (1996, 2004, 2014, 2024)
cit_all <- gss_hier
  filter(!is.na(cit_attach))

# 4. Multilevel models (final versions WITHOUT sex)

# IMMJOBS – all waves
model_jobs_all_nosex <- glmer(
  immjobs_anti ~ educ_w + educ_b + year_f + age10 + race_f + (1 | region_f),
  data   = jobs_all,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_jobs_all_nosex)
exp(fixef(model_jobs_all_nosex))
VarCorr(model_jobs_all_nosex)

# Table: IMMJOBS all waves (odds ratios)
jobs_all_tidy <- tidy(model_jobs_all_nosex, effects = "fixed", conf.int = TRUE, conf.method = "Wald") %>%
  mutate(
    OR     = exp(estimate),
    CI_low = exp(conf.low),
    CI_high= exp(conf.high),
    p_fmt  = format.pval(p.value, digits = 3, eps = 0.001),
    term_pretty = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "educ_w"      ~ "Education (within region, z)",
      term == "educ_b"      ~ "Education (between region, z)",
      term == "year_f1996"  ~ "Year: 1996 (ref. 2014)",
      term == "year_f2004"  ~ "Year: 2004 (ref. 2014)",
      term == "year_f2022"  ~ "Year: 2022 (ref. 2014)",
      term == "year_f2024"  ~ "Year: 2024 (ref. 2014)",
      term == "age10"       ~ "Age (per 10 years, z)",
      term == "race_fblack" ~ "Black (ref. White)",
      term == "race_fother" ~ "Other race (ref. White)",
      TRUE                  ~ term
    )
  )

jobs_all_tidy_print <- jobs_all_tidy
  mutate(
    OR     = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high= round(CI_high, 2)
  ) 
  select(Variable = term_pretty, OR, `CI low` = CI_low, `CI high` = CI_high, p = p_fmt)

kable(
  jobs_all_tidy_print,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Multilevel logistic regression of jobs–threat attitudes (IMMJOBS), 1996--2024. Odds ratios with 95\\% confidence intervals.",
  label    = "immjobs_allwaves_nosex"
)


#IMMJOBS – 2022–2024 subset
model_jobs_recent_nosex <- glmer(
  immjobs_anti ~ educ_w + educ_b + year_f + age10 + race_f + (1 | region_f),
  data   = jobs_recent,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_jobs_recent_nosex)
exp(fixef(model_jobs_recent_nosex))
VarCorr(model_jobs_recent_nosex)


# IMMJOBS – income robustness (2022–2024)

# Base model on income-complete sample (no income)
model_base_income_nosex <- glmer(
  immjobs_anti ~ educ_w + educ_b + year_f + age10 + race_f + (1 | region_f),
  data   = jobs_income,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Model with income_z
model_income_nosex <- glmer(
  immjobs_anti ~ educ_w + educ_b + year_f + age10 + race_f + income_z + (1 | region_f),
  data   = jobs_income,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

anova(model_base_income_nosex, model_income_nosex, test = "Chisq")
exp(fixef(model_income_nosex))

#  summary table
robust_jobs_income <- tibble::tibble(
  Model              = c("Without income", "With income"),
  OR_educ_within     = round(exp(c(
    fixef(model_base_income_nosex)["educ_w"],
    fixef(model_income_nosex)["educ_w"]
  )), 2),
  OR_educ_between    = round(exp(c(
    fixef(model_base_income_nosex)["educ_b"],
    fixef(model_income_nosex)["educ_b"]
  )), 2),
  OR_age10           = round(exp(c(
    fixef(model_base_income_nosex)["age10"],
    fixef(model_income_nosex)["age10"]
  )), 2),
  OR_2024_vs_2022    = round(exp(c(
    fixef(model_base_income_nosex)["year_f2024"],
    fixef(model_income_nosex)["year_f2024"]
  )), 2),
  OR_income_z        = c(NA, round(exp(fixef(model_income_nosex)["income_z"]), 2)),
  p_income           = c(NA, format.pval(coef(summary(model_income_nosex))["income_z","Pr(>|z|)"],
                                         digits = 3, eps = 0.001))
)

kable(
  robust_jobs_income,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Robustness checks for jobs–threat attitudes (IMMJOBS), 2022--2024: multilevel models with and without standardized income.",
  label    = "immjobs_income_robust_nosex"
)

# IMMAMECO – economic attitudes, all waves
model_eco_all_nosex <- glmer(
  immameco_anti ~ educ_w + educ_b + year_f + age10 + race_f + (1 | region_f),
  data   = eco_all,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_eco_all_nosex)
exp(fixef(model_eco_all_nosex))
VarCorr(model_eco_all_nosex)

eco_tidy <- tidy(
  model_eco_all_nosex,
  effects = "fixed",
  conf.int = TRUE,
  conf.method = "Wald"
)
  mutate(
    OR     = exp(estimate),
    CI_low = exp(conf.low),
    CI_high= exp(conf.high),
    p_fmt  = format.pval(p.value, digits = 3, eps = 0.001),
    term_pretty = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "educ_w"      ~ "Education (within region, z)",
      term == "educ_b"      ~ "Education (between region, z)",
      term == "year_f1996"  ~ "Year: 1996 (ref. 2014)",
      term == "year_f2004"  ~ "Year: 2004 (ref. 2014)",
      term == "year_f2024"  ~ "Year: 2024 (ref. 2014)",
      term == "age10"       ~ "Age (per 10 years, z)",
      term == "race_fblack" ~ "Black (ref. White)",
      term == "race_fother" ~ "Other race (ref. White)",
      TRUE                  ~ term
    )
  )

eco_tidy_print <- eco_tidy
  mutate(
    OR     = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high= round(CI_high, 2)
  ) 
  select(
    Variable = term_pretty,
    OR,
    `CI low`  = CI_low,
    `CI high` = CI_high,
    p         = p_fmt
  )

kable(
  eco_tidy_print,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Multilevel logistic regression of economic threat attitudes (IMMAMECO), 1996--2024. Odds ratios with 95\\% confidence intervals.",
  label    = "immameco_allwaves_nosex"
)



#IMMAMECO income robustness (all available waves: 1996, 2004, 2014, 2024)

# Subset: non-missing outcome & income
eco_income <- eco_all
  filter(!is.na(income_z))

# Base model on income-complete sample (no income_z)
model_eco_base_income <- glmer(
  immameco_anti ~ educ_w + educ_b + year_f + age10 + race_f + (1 | region_f),
  data   = eco_income,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Model with income_z added
model_eco_income <- glmer(
  immameco_anti ~ educ_w + educ_b + year_f + age10 + race_f + income_z + (1 | region_f),
  data   = eco_income,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Likelihood-ratio test
anova(model_eco_base_income, model_eco_income, test = "Chisq")

# summary table
robust_eco_income <- tibble::tibble(
  Model              = c("Without income", "With income"),
  OR_educ_within     = round(exp(c(
    fixef(model_eco_base_income)["educ_w"],
    fixef(model_eco_income)["educ_w"]
  )), 2),
  OR_educ_between    = round(exp(c(
    fixef(model_eco_base_income)["educ_b"],
    fixef(model_eco_income)["educ_b"]
  )), 2),
  OR_age10           = round(exp(c(
    fixef(model_eco_base_income)["age10"],
    fixef(model_eco_income)["age10"]
  )), 2),
  OR_2024_vs_2014    = round(exp(c(
    fixef(model_eco_base_income)["year_f2024"],
    fixef(model_eco_income)["year_f2024"]
  )), 2),
  OR_income_z        = c(NA, round(exp(fixef(model_eco_income)["income_z"]), 2)),
  p_income           = c(
    NA,
    format.pval(
      coef(summary(model_eco_income))["income_z", "Pr(>|z|)"],
      digits = 3, eps = 0.001
    )
  )
)

kable(
  robust_eco_income,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Robustness checks for economic threat attitudes (IMMAMECO), 1996--2024: multilevel models with and without standardized income.",
  label    = "immameco_income_robust_nosex"
)


# LETIN1 – immigration restriction
model_letin_all_nosex <- glmer(
  letin_restrict ~ educ_w + educ_b + year_f + age10 + race_f + (1 | region_f),
  data   = letin_all,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_letin_all_nosex)
exp(fixef(model_letin_all_nosex))
VarCorr(model_letin_all_nosex)

letin_tidy <- tidy(
  model_letin_all_nosex,
  effects = "fixed",
  conf.int = TRUE,
  conf.method = "Wald"
)
  mutate(
    OR     = exp(estimate),
    CI_low = exp(conf.low),
    CI_high= exp(conf.high),
    p_fmt  = format.pval(p.value, digits = 3, eps = 0.001),
    term_pretty = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "educ_w"      ~ "Education (within region, z)",
      term == "educ_b"      ~ "Education (between region, z)",
      term == "year_f1996"  ~ "Year: 1996 (ref. 2014)",
      term == "year_f2004"  ~ "Year: 2004 (ref. 2014)",
      term == "year_f2024"  ~ "Year: 2024 (ref. 2014)",
      term == "age10"       ~ "Age (per 10 years, z)",
      term == "race_fblack" ~ "Black (ref. White)",
      term == "race_fother" ~ "Other race (ref. White)",
      TRUE                  ~ term
    )
  )

letin_tidy_print <- letin_tidy
  mutate(
    OR     = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high= round(CI_high, 2)
  ) 
  select(
    Variable = term_pretty,
    OR,
    `CI low`  = CI_low,
    `CI high` = CI_high,
    p         = p_fmt
  )

kable(
  letin_tidy_print,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Multilevel logistic regression of immigration restriction attitudes (LETIN1), 1996--2024. Odds ratios with 95\\% confidence intervals.",
  label    = "letin_allwaves_nosex"
)

#LETIN1 income robustness (waves with LETIN1 and income)

letin_income <- letin_all
  filter(!is.na(income_z))

# Base model on income-complete sample (no income_z)
model_letin_base_income <- glmer(
  letin_restrict ~ educ_w + educ_b + year_f + age10 + race_f + (1 | region_f),
  data   = letin_income,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Model with income_z
model_letin_income <- glmer(
  letin_restrict ~ educ_w + educ_b + year_f + age10 + race_f + income_z + (1 | region_f),
  data   = letin_income,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

anova(model_letin_base_income, model_letin_income, test = "Chisq")

robust_letin_income <- tibble::tibble(
  Model              = c("Without income", "With income"),
  OR_educ_within     = round(exp(c(
    fixef(model_letin_base_income)["educ_w"],
    fixef(model_letin_income)["educ_w"]
  )), 2),
  OR_educ_between    = round(exp(c(
    fixef(model_letin_base_income)["educ_b"],
    fixef(model_letin_income)["educ_b"]
  )), 2),
  OR_age10           = round(exp(c(
    fixef(model_letin_base_income)["age10"],
    fixef(model_letin_income)["age10"]
  )), 2),
  OR_2024_vs_2014    = round(exp(c(
    fixef(model_letin_base_income)["year_f2024"],
    fixef(model_letin_income)["year_f2024"]
  )), 2),
  OR_income_z        = c(NA, round(exp(fixef(model_letin_income)["income_z"]), 2)),
  p_income           = c(
    NA,
    format.pval(
      coef(summary(model_letin_income))["income_z", "Pr(>|z|)"],
      digits = 3, eps = 0.001
    )
  )
)

kable(
  robust_letin_income,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Robustness checks for immigration restriction attitudes (LETIN1), 1996--2024: multilevel models with and without standardized income.",
  label    = "letin_income_robust_nosex"
)


#Citizenship attachment – AMCITIZN
model_cit_all_nosex <- glmer(
  cit_attach ~ educ_w + educ_b + year_f + age10 + race_f + (1 | region_f),
  data   = cit_all,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_cit_all_nosex)
exp(fixef(model_cit_all_nosex))
VarCorr(model_cit_all_nosex)

cit_tidy <- tidy(
  model_cit_all_nosex,
  effects = "fixed",
  conf.int = TRUE,
  conf.method = "Wald"
) 
  mutate(
    OR     = exp(estimate),
    CI_low = exp(conf.low),
    CI_high= exp(conf.high),
    p_fmt  = format.pval(p.value, digits = 3, eps = 0.001),
    term_pretty = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "educ_w"      ~ "Education (within region, z)",
      term == "educ_b"      ~ "Education (between region, z)",
      term == "year_f1996"  ~ "Year: 1996 (ref. 2014)",
      term == "year_f2004"  ~ "Year: 2004 (ref. 2014)",
      term == "year_f2024"  ~ "Year: 2024 (ref. 2014)",
      term == "age10"       ~ "Age (per 10 years, z)",
      term == "race_fblack" ~ "Black (ref. White)",
      term == "race_fother" ~ "Other race (ref. White)",
      TRUE                  ~ term
    )
  )

cit_tidy_print <- cit_tidy
  mutate(
    OR     = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high= round(CI_high, 2)
  ) 
  select(
    Variable = term_pretty,
    OR,
    `CI low`  = CI_low,
    `CI high` = CI_high,
    p         = p_fmt
  )

kable(
  cit_tidy_print,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Multilevel logistic regression of citizenship attachment (AMCITIZN), 1996--2024. Odds ratios with 95\\% confidence intervals.",
  label    = "citattach_allwaves_nosex"
)


# AMCITIZN income robustness (citizenship attachment)

cit_income <- cit_all
  filter(!is.na(income_z))

# Base model on income-complete sample (no income_z)
model_cit_base_income <- glmer(
  cit_attach ~ educ_w + educ_b + year_f + age10 + race_f + (1 | region_f),
  data   = cit_income,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

# Model with income_z
model_cit_income <- glmer(
  cit_attach ~ educ_w + educ_b + year_f + age10 + race_f + income_z + (1 | region_f),
  data   = cit_income,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

anova(model_cit_base_income, model_cit_income, test = "Chisq")

robust_cit_income <- tibble::tibble(
  Model              = c("Without income", "With income"),
  OR_educ_within     = round(exp(c(
    fixef(model_cit_base_income)["educ_w"],
    fixef(model_cit_income)["educ_w"]
  )), 2),
  OR_educ_between    = round(exp(c(
    fixef(model_cit_base_income)["educ_b"],
    fixef(model_cit_income)["educ_b"]
  )), 2),
  OR_age10           = round(exp(c(
    fixef(model_cit_base_income)["age10"],
    fixef(model_cit_income)["age10"]
  )), 2),
  OR_2024_vs_2014    = round(exp(c(
    fixef(model_cit_base_income)["year_f2024"],
    fixef(model_cit_income)["year_f2024"]
  )), 2),
  OR_income_z        = c(NA, round(exp(fixef(model_cit_income)["income_z"]), 2)),
  p_income           = c(
    NA,
    format.pval(
      coef(summary(model_cit_income))["income_z", "Pr(>|z|)"],
      digits = 3, eps = 0.001
    )
  )
)

kable(
  robust_cit_income,
  format   = "latex",
  booktabs = TRUE,
  caption  = "Robustness checks for citizenship attachment (AMCITIZN), 1996--2024: multilevel models with and without standardized income.",
  label    = "citattach_income_robust_nosex"
)

# 5. Graphs
#Line plot: proportion with negative attitudes by year
attitudes_long <- attitudes_year 
  tidyr::pivot_longer(
    cols      = starts_with("p_"),
    names_to  = "outcome",
    values_to = "prop"
  ) 
  mutate(
    outcome = factor(
      outcome,
      levels = c("p_immjobs", "p_immameco", "p_letin", "p_citattach"),
      labels = c(
        "Jobs threat (IMMJOBS)",
        "Economic threat (IMMAMECO)",
        "Restrict immigration (LETIN1)",
        "Citizenship attachment (AMCITIZN)"
      )
    )
  )

ggplot(attitudes_long, aes(x = year, y = prop, color = outcome)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Proportion of respondents",
    color = "Outcome",
    title = "Trends in attitudes toward immigrants and citizenship attachment"
  ) +
  theme_minimal()

# 5.2 Histogram of education and age (pooled)
ggplot(gss_hier, aes(x = educ_num)) +
  geom_histogram(bins = 20, na.rm = TRUE) +
  labs(x = "Years of education", y = "Count", title = "Distribution of education in the pooled sample") +
  theme_minimal()

ggplot(gss_hier, aes(x = age_num)) +
  geom_histogram(bins = 20, na.rm = TRUE) +
  labs(x = "Age", y = "Count", title = "Age distribution in the pooled sample") +
  theme_minimal()

# Simple forest plot of ORs for education (within/between) by outcome
or_edu <- tibble::tibble(
  outcome = c("Jobs threat", "Economic threat", "Immigration restriction", "Citizenship attachment"),
  OR_within = c(
    exp(fixef(model_jobs_all_nosex)["educ_w"]),
    exp(fixef(model_eco_all_nosex)["educ_w"]),
    exp(fixef(model_letin_all_nosex)["educ_w"]),
    exp(fixef(model_cit_all_nosex)["educ_w"])
  ),
  OR_between = c(
    exp(fixef(model_jobs_all_nosex)["educ_b"]),
    exp(fixef(model_eco_all_nosex)["educ_b"]),
    exp(fixef(model_letin_all_nosex)["educ_b"]),
    exp(fixef(model_cit_all_nosex)["educ_b"])
  )
) %>%
  tidyr::pivot_longer(cols = starts_with("OR_"), names_to = "type", values_to = "OR") 
  mutate(
    type = recode(type,
                  OR_within = "Within-region education",
                  OR_between = "Between-region education")
  )

ggplot(or_edu, aes(x = outcome, y = OR, color = type)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  coord_flip() +
  labs(
    x = "Outcome",
    y = "Odds ratio",
    color = "Education effect",
    title = "Education effects (within and between regions) across outcomes"
  ) +
  theme_minimal()





# Graphs for thesis with larger fonts
theme_thesis <- function() {
  theme_minimal(base_size = 16) +
    theme(
      plot.title   = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title   = element_text(size = 16),
      axis.text    = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text  = element_text(size = 13),
      panel.grid.minor = element_blank()
    )
}


attitudes_long <- attitudes_ye
  tidyr::pivot_longer(
    cols      = starts_with("p_"),
    names_to  = "outcome",
    values_to = "prop"
  ) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("p_immjobs", "p_immameco", "p_letin", "p_citattach"),
      labels = c(
        "Jobs threat (IMMJOBS)",
        "Economic threat (IMMAMECO)",
        "Restrict immigration (LETIN1)",
        "Citizenship attachment (AMCITIZN)"
      )
    )
  )

ggplot(attitudes_long, aes(x = year, y = prop, color = outcome)) +
  geom_line(size = 1.3) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Dark2") +   # clearly distinguishable colours
  labs(
    x = "Year",
    y = "Proportion of respondents",
    color = "Outcome",
    title = "Trends in attitudes toward immigrants and citizenship attachment"
  ) +
  theme_thesis()



# Education
ggplot(gss_hier, aes(x = educ_num)) +
  geom_histogram(
    bins = 20,
    na.rm = TRUE,
    fill = "#4C72B0",   # blue
    color = "white",
    alpha = 0.9
  ) +
  labs(
    x = "Years of education",
    y = "Count",
    title = "Distribution of education in the pooled sample"
  ) +
  theme_thesis()

# Age
ggplot(gss_hier, aes(x = age_num)) +
  geom_histogram(
    bins = 20,
    na.rm = TRUE,
    fill = "#55A868",   # green
    color = "white",
    alpha = 0.9
  ) +
  labs(
    x = "Age",
    y = "Count",
    title = "Age distribution in the pooled sample"
  ) +
  theme_thesis()


or_edu <- tibble::tibble(
  outcome = c("Jobs threat", "Economic threat",
              "Immigration restriction", "Citizenship attachment"),
  OR_within = c(
    exp(fixef(model_jobs_all_nosex)["educ_w"]),
    exp(fixef(model_eco_all_nosex)["educ_w"]),
    exp(fixef(model_letin_all_nosex)["educ_w"]),
    exp(fixef(model_cit_all_nosex)["educ_w"])
  ),
  OR_between = c(
    exp(fixef(model_jobs_all_nosex)["educ_b"]),
    exp(fixef(model_eco_all_nosex)["educ_b"]),
    exp(fixef(model_letin_all_nosex)["educ_b"]),
    exp(fixef(model_cit_all_nosex)["educ_b"])
  )
) %>%
  tidyr::pivot_longer(
    cols = starts_with("OR_"),
    names_to = "type",
    values_to = "OR"
  ) %>%
  mutate(
    type = recode(
      type,
      OR_within  = "Within-region education",
      OR_between = "Between-region education"
    )
  )

ggplot(or_edu, aes(x = outcome, y = OR, color = type)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.7) +
  geom_point(
    position = position_dodge(width = 0.5),
    size = 4
  ) +
  coord_flip() +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Outcome",
    y = "Odds ratio",
    color = "Education effect",
    title = "Education effects (within and between regions) across outcomes"
  ) +
  theme_thesis()


