library(tidyverse)
library(tidyr)
library(fastDummies)
library(recipes)

rm(list = ls())
setwd("C:/Users/Lukas Grahl/Documents/r_project")
source("func_src.r")

load(file = "./data/df.Rda")

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

## INCOME Comparison
bachelor_plot = 
  ggplot() +
  geom_density(data = df[df[, "sex"] == 1 & df[, "educ"] == "111", ], 
               mapping = aes(x = incwage),
               colour = "blue", 
               fill = "blue", 
               alpha = 0.2) +
  geom_vline(mapping = 
               aes(xintercept = 
                     median(df[df[, "sex"] == 1 & df[, "educ"] == "111", ]$incwage),
                   colour = "blue")) +
  geom_vline(mapping = 
               aes(xintercept = 
                     median(df[df[, "sex"] == 2 & df[, "educ"] == "111", ]$incwage),
                   colour = "red"), 
             size = 1) +
  geom_density(data = df[df[, "sex"] == 2 & df[, "educ"] == "111", ],
               mapping = aes(x = incwage),
               colour = "red",
               fill = "red",
               size = .5,
               alpha = 0.2) +
  scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) +
  scale_colour_manual(name = 'Coulour Coding', 
                      values =c('blue'='blue','red'='red'), labels = c('male','female')) +
  ggtitle("Log Income distribution Bachelors Degree", ) +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  theme_minimal()

ggsave("test.png", bachelor_plot)




ggplot() +
  geom_density(data = df[df[, "sex"] == 1 & df[, "educ"] == "123", ], 
               mapping = aes(x = incwage), 
               fill = "blue",
               colour = "blue", 
               alpha = 0.5) +
  geom_vline(mapping = 
               aes(xintercept = 
                     median(df[df[, "sex"] == 1 & df[, "educ"] == "123", ]$incwage),
                   colour = "blue")) +
  geom_vline(mapping = 
               aes(xintercept = 
                     median(df[df[, "sex"] == 2 & df[, "educ"] == "123", ]$incwage),
                   colour = "red")) +
  geom_density(data = df[df[, "sex"] == 2 & df[, "educ"] == "123", ],
               mapping = aes(x = incwage),
               colour = "red",
               fill = "red",
               alpha = 0.5) +
  ggtitle("Log Income distribution Masters Degree")


summary(df[df[, "sex"] == 1 & df[, "educ"] == "111", ]$incwage)
summary(df[df[, "sex"] == 2 & df[, "educ"] == "111", ]$incwage)

## Feature Eng
df <- featureeng_df(df)


df_lm <- df[, lm_cols]
df$is_higsch

## FILTER
filter <- TRUE
if (filter == TRUE){
  df_lm <- df[, lm_cols]
  print("Filter is on")
  df_lm <- df_lm %>% filter((is_higsch == TRUE | is_bachel == TRUE))
  print(nrow(df_lm))
} else {
  print("Filter is off")
  df_lm <- df[, lm_cols]
  print(nrow(df_lm))
}

## Dropping 0 cols
df_lm <- dropp_zero_cols(df_lm)

## Dropping is paid_hour
df_lm <- df_lm[, !names(df_lm) %in% c("is_paidhour", "is_bachel_cum")]

## COR
corr_matrix(df_lm)


## LM
lm1 <- lm(log_wage ~
            is_bachel * is_firm_large+
            citizen +
            age +
            male +
            famsize +
            has_children + 
            nchild +
            has_many_child +
            # sing_par +
            immig_first +
            immig_secn +
            is_hispan +
            is_negro +
            # is_asian +
            # is_nativamec +
            is_firm_large +
            # firmsize2 +
            # firmsize5 +
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

summary(lm1)

## TABLE
models <- list(
  "(1)" = lm1
  # "(2)" = lm2
  # "(3)" = lm3,
  # "(4)" = lm4,
  # "(5)" = lm5,
  # "(6)" = lm6
)

tab <- get_outputtab(model_list = models)
tab





