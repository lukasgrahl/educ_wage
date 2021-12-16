library(tidyverse)
library(tidyr)
library(fastDummies)
library(recipes)

rm(list = ls())
setwd("C:/Users/Lukas Grahl/Documents/r_project")

output_dir <- sprintf("%s/output", getwd())
src_dir <- sprintf("%s/src", getwd())
data_dir <- sprintf("%s/data", getwd())
filename <- "df.Rda"

source(sprintf("%s/func_src.R", src_dir))
load(file = sprintf("%s/%s", data_dir, filename))

df <- asstype_df(df)
df <- filter_df(df)

# PLOTTING
# wage
ggplot(data = df, 
       mapping = aes(x = incwage, y = ..density..)) + 
  geom_histogram(binwidth = 5000, fill = "grey", alpha = 0.7) +
  geom_density(colour = "red")

ggplot(data = df, 
       mapping = aes(x = log(incwage), y = ..density..)) + 
  geom_histogram(bins = 50, fill = "grey", alpha = 0.7) +
  geom_density(colour = "red")

## OUTLIER SELECTION
ggplot(data = df)+
  geom_boxplot(mapping = aes(x = log(incwage)))

# Identification of outliers from log transformed wage: Log already penalises outlier
df$is_wage_outl <- is_outlier(arr = log(df$incwage), method = "iqr")
df <- filter(df, is_wage_outl == FALSE)

ggplot(data = df)+
  geom_boxplot(mapping = aes(x = log(incwage)))

## Feature Eng
df <- featureeng_df(df)

## Plotting Inc Dist
female_plot <- plot_inc_dist(df, sex_val = 2)
male_plot <- plot_inc_dist(df, sex_val = 1)

female_plot
male_plot

ggsave(sprintf("%s/male_plot.png", output_dir), male_plot)
ggsave(sprintf("%s/female_plot.png", output_dir), female_plot)

## FILTER
filter <- TRUE
if (filter == TRUE){
  df_lm <- df[, lm_cols]
  print("Filter is on")
  df_lm <- df_lm %>% filter((is_higsch == TRUE | 
                               is_bachel == TRUE | 
                               is_master == TRUE))
  print(nrow(df_lm))
} else {
  print("Filter is off")
  df_lm <- df[, lm_cols]
  print(nrow(df_lm))
}

## Dropping 0 cols
df_lm <- dropp_zero_cols(df_lm)

## Dropping is paid_hour
df_lm <- df_lm[, !names(df_lm) %in% c("is_bachel_cum")]

## COR
corr_matrix(df_lm, thresh = 0.4)

## LM
lm1 <- lm(log_wage ~
            is_higsch +
            is_master, 
            # citizen +
            # age +
            # male +
            # famsize +
            # has_children + 
            # nchild +
            # has_many_child +
            # sing_par +
            # immig_first +
            # immig_secn +
            # is_hispan +
            # is_negro +
            # is_asian +
            # is_nativamec +
            # is_firm_large +
            # firmsize4 +
            # firmsize6 +
            # firmsize7 +
            # firmsize8 +
            # firmsize9 +
            # pension_inc +
            # pension_exl +
            # union_mem +
            # union_mem_incl +
            # is_poorheal +
            # # is_paidhour +
            # is_disabled,
          data = df_lm
)

lm2 <- lm(log_wage ~
            is_higsch +
            is_master +
          citizen +
          age +
          male +
          famsize +
          has_children +
          nchild +
          # has_many_child +
          sing_par +
          immig_first +
          immig_secn +
          is_hispan +
          is_negro +
          is_asian +
          is_nativamec, 
          # is_firm_large +
          # firmsize5 +
          # firmsize6 +
          # firmsize7 +
          # firmsize8 +
          # firmsize9 +
          # pension_inc +
          # pension_exl +
          # union_mem +
          # union_mem_incl +
          # is_poorheal +
          # # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm3 <- lm(log_wage ~
            is_higsch +
            is_master +
            citizen +
            age +
            male +
            famsize +
            has_children +
            nchild +
            # has_many_child +
            sing_par +
            immig_first +
            immig_secn +
            is_hispan +
            is_negro +
            is_asian +
            is_nativamec +
          is_firm_large +
          firmsize4 +
          firmsize6 +
          firmsize7 +
          firmsize8 +
          firmsize9 +
          pension_inc +
          pension_exl +
          union_mem +
          union_mem_incl, 
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm4 <- lm(log_wage ~
            is_higsch +
            is_master +
            citizen +
            age +
            male +
            famsize +
            has_children +
            nchild +
            # has_many_child +
            sing_par +
            # immig_first +
            # immig_secn +
            # is_hispan +
            # is_negro +
            # is_asian +
            # is_nativamec +
            # is_firm_large +
            firmsize4 +
            firmsize6 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            pension_exl +
            union_mem +
            union_mem_incl, 
          # is_poorheal +
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm5 <- lm(log_wage ~
            is_higsch +
            is_master +
            citizen +
            age +
            male +
            famsize +
            has_children +
            nchild +
            # has_many_child +
            sing_par +
            immig_first +
            immig_secn +
            is_hispan +
            is_negro +
            is_asian +
            is_nativamec +
            # is_firm_large +
            firmsize4 +
            firmsize6 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            pension_exl +
            union_mem +
            union_mem_incl +
          is_poorheal +
          is_paidhour +
          is_disabled,
          data = df_lm
)

lm6 <- lm(log_wage ~
            is_higsch +
            is_master +
            citizen +
            age +
            male +
            famsize +
            has_children +
            nchild +
            # has_many_child +
            sing_par +
            immig_first +
            immig_secn +
            is_hispan +
            is_negro +
            # is_asian +
            is_nativamec +
            # is_firm_large +
            firmsize4 +
            firmsize6 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            # pension_exl +
            # union_mem +
            # union_mem_incl +
            is_poorheal,
            # is_paidhour +
            # is_disabled,
          data = df_lm
)

lm7 <- lm(log_wage ~
            is_higsch * male +
            is_master * male +
            citizen +
            age +
            male +
            famsize +
            has_children +
            nchild +
            has_many_child +
            sing_par +
            immig_first +
            immig_secn +
            is_hispan +
            is_negro +
            # is_asian +
            is_nativamec +
            # is_firm_large +
            firmsize4 +
            firmsize6 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            pension_exl +
            # union_mem +
            # union_mem_incl +
            is_poorheal,
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

lm8 <- lm(log_wage ~
            is_higsch * is_firm_large +
            is_master * is_firm_large +
            citizen +
            age +
            male +
            famsize +
            has_children +
            nchild +
            has_many_child +
            sing_par +
            immig_first +
            immig_secn +
            is_hispan +
            is_negro +
            # is_asian +
            is_nativamec +
            is_firm_large +
            firmsize4 +
            firmsize6 +
            # firmsize7 +
            # firmsize8 +
            # firmsize9 +
            pension_inc +
            pension_exl +
            # union_mem +
            # union_mem_incl +
            is_poorheal,
          # is_paidhour +
          # is_disabled,
          data = df_lm
)

summary(lm8)

## TABLE
models <- list(
  "(1)" = lm1,
  "(2)" = lm2,
  "(3)" = lm3,
  "(4)" = lm4,
  "(5)" = lm5,
  "(6)" = lm6,
  "(7)" = lm7,
  "(8)" = lm8
)

coefficient_names <- c(
  'is_higschTRUE' = 'Highschool degree only',
  'is_masterTRUE' = 'Masters degree',
  'citizen' = 'US Citizen',
  'age'= 'Age',
  'maleTRUE'= 'Gender (male)',
  'famsize' = 'No. of direct family members',
  'has_childrenTRUE' = 'Has one or more children',
  'nchild' = 'No. of children',
  'sing_parTRUE' = 'Single parent',
  'immig_firstTRUE' = 'First generation immigrant',
  'immig_secnTRUE' = 'Second generation immigrant',
  'is_hispanTRUE' = 'Hispanic',
  'is_negroTRUE' = 'Person of colour',
  'is_asianTRUE' = 'Asian',
  'is_nativamecTRUE' = 'Native American',
  'pension_incTRUE' = 'Included in pension scheme',
  # 'pension_exlTRUE' = 'Not included in existing pension scheme',
  'union_memTRUE' = 'Member of a workers union',
  # 'union_mem_inclTRUE' = 'Covered by workers union but no member',
  'is_poorhealTRUE' = 'Poor health condition',
  'is_paidhourTRUE' = 'Paid by hour',
  'is_disabledTRUE' = 'Disabled',
  'is_firm_largeTRUE' = 'Firm with 500+ employees',
  # 'has_many_childTRUE' = 'More than 3 children',
  'is_higschTRUE:maleTRUE' = 'Interaction highschool & male',
  'maleTRUE:is_masterTRUE' = 'Interaction master & male',
  'is_higschTRUE:is_firm_largeTRUE' = 'Interaction highschool & large firm',
  'is_firm_largeTRUE:is_masterTRUE' = 'Interaction master & large',
  'firmsize4TRUE' = 'firmsize4',
  'firmsize6TRUE' = 'firmsize6',
  'firmsize7TRUE' = 'firmsize7', 
  'firmsize8TRUE' = 'firmsize8',
  'firmsize9TRUE' = 'firmsize9',
  '(Intercept)' = 'Constant'
)

tab <- get_outputtab(models, coefficient_names, to_ppt = FALSE)
tab


