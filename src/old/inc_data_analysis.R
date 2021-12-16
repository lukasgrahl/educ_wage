library(tidyverse)
library(tidyr)
library(fastDummies)
library(recipes)

rm(list = ls())

setwd("C:/Users/Lukas Grahl/Documents/r_project/data")

# load(file = "data11.Rda")
# df <- data11
# save(df, file="df.Rda")

rm(list = ls())
load(file = "df.Rda")

df$age <- as.numeric(df$age)
df$occ <- as.factor((df$occ))
df$educ <- as.factor(df$educ)

rel_var <- c(
  "relate", # household leader is more likely to compete fully in labour market
  "race", # OVB - structural racism
  "sex", # OVB - gender pay gap
  "age", # OVB - indication of career progression
  "marst", # married women vs. non-married, single parents
  "popstat", # civilian only
  "vetstat", # veteran status - disability through injury
  "famsize", # large families (all living in one place) as proxy of lower socio-economics status
  "nchild", # no children - socio economic status ?
  "famkind",
  "citizen", 
  "nativity", # immigrants most likely have lower income -  first or second generation ?
  "yrimmig", # time since immigration - predictor of inclusion
  "hispan", 
  "empstat", # exclude all not in the labour force
  "labforce", # - "" - 
  "occ", # occupation
  "ind", # industry
  # "oocly", # occupation last year
  "indly", # industry last year
  # "classwrk", # treat gov empl differently ? 
  "uhrswork1", # hours worked per week on avg
  "durunem2", # time employee has been looking for work
  "wnftlook", # when last worked full time two consecutive weeks
  "wkstat", # full of part-time - exclude part time
  "fullpart", # full or part-time last year
  "educ", # highest educatinal attainment
  "schlcoll", # currently in school or college
  "workly", # worked last year bool
  "firmsize", # number of employees at firm
  "inctot", # aggregated anual income
  "incbus", # non farm business income
  "incfarm", # farm income
  "incwage", # income from wages 
  "incss", # social security income - edge case
  "incwelfr", # income from welfare - transfer payment
  "incunemp", # income from unemployment benefits
  "incwkcom", # worker compensation for injury
  "incvet", # income from veteran - survivor or disability benefits
  "incsurv", # survivor benefits - e.g. coal mining
  "incdisab", # income from disability benefits
  "incretir", # retirement income
  "incint", # income from interest
  "incdivid", # income from dividens
  "incrent", # income from rent or subletting
  "hourwage", # hourly wage - y variable, better ?
  "paidhour", # paid by the hour - indicator of working class lower level job
  "migrate1", # people who have moved last year - trailer park movers - low social status
  "health", # health status - include in model low as this may affect income
  "quitsick", # retired for health reasons - exclude
  "union", # union membership as indication of blue colar
  "earnweek", # how much is earned per week
  "pension",
  "incchild" # child support
)

df <- df[rel_var]


## FILTERING
x <- nrow(df)

# civilians
df <- df[df$popstat == 1, ]

# labour force
df <- df[df$labforce == 2, ]

# in education exclude
df <- df[df$age > 20, ]
# df <- df[df$schlcoll == 5, ]

# part time work 
df <- df[df$fullpart == 1, ]

# no wage
df <- df[df$incwage > 0, ]
sprintf("Data now has %f of org size", round((x - nrow(df)) / x, 2))

# PLOTTING
# wage
ggplot(data = df, 
       mapping = aes(x = incwage, y = ..density..)) + 
  geom_histogram(binwidth = 5000, fill = "grey", alpha = 0.7) +
  geom_density(colour = "red")

ggplot(data = df, 
       mapping = aes(x = log(incwage), y = ..density..)) + 
  geom_histogram(bins = 100, fill = "grey", alpha = 0.7) +
  geom_density(colour = "red")

## OUTLIER SELECTION
ggplot(data = df)+
  geom_boxplot(mapping = aes(x = log(incwage)))

is_outlier <- function(arr, method = "iqr"){
  mean <- median(arr)
  iqr <- quantile(arr, 0.75) - quantile(arr, 0.25)
  up_bound <- mean + 2 * iqr
  low_bound <- mean - 2 * iqr
  
  return (arr > up_bound | arr < low_bound)
}

# Identification of outliers from log transformed wage: Log already penalises outlier
df$is_wage_outl <- is_outlier(arr = log(df$incwage))
df <- filter(df, is_wage_outl == FALSE)

ggplot(data = df)+
  geom_boxplot(mapping = aes(x = log(incwage)))

ggplot() + 
            geom_histogram(data = df[df[, "sex"] == 1 & df[, "educ"] == "125", ], 
                           mapping = aes(x = incwage),
                           bins = 50,
                           fill = "blue") +
            geom_histogram(data = df[df[, "sex"] == 2 & df[, "educ"] == "125", ], 
                           mapping = aes(x = incwage),
                           bins = 50,
                           fill = "pink",
                           colour = "black",
                           alpha = 0.5) +
            ggtitle("Men (blue) & female (red) inc distribution")


## Feature Eng
# Education dummy encode
df$is_master <- df$educ == 123
df$is_bachel <- df$educ == 111
df$is_phd <- df$educ == 125
df$is_higsch <- (df$educ == 81 | df$educ == 73)
df$is_bachel_cum <- (df$is_master == TRUE | df$is_bachel == TRUE)

# Wage log
df$log_wage <- log(df$incwage)

# singel parent
df$sing_par <- c(
  (df$marst == 4 | df$marst == 5 | df$marst == 3) &
  (df$nchild > 0))

# immigrant first or second gen
df$immig_first <- df$nativity == 5 & df$yrimmig > 0
df$immig_secn <- df$nativity == 4 | df$nativity == 3 |df$nativity == 2

# race dummy
df$is_hispan <- df$hispan != 0 & (df$hispan != 902 | df$hispan != 903)
df$is_asian <- (df$race == 650 | df$race == 651)
df$is_negro <- (df$race == 200 | df$race == 801)
df$is_nativamec <- df$race == 300

# dummy firmsize
x <- fastDummies::dummy_cols(df$firmsize, remove_first_dummy = TRUE)
x <- x[, c(2:ncol(x))]
colnames(x) <- c("firmsize2", "firmsize5", "firmsize7", "firmsize8", "firmsize9")
df <- cbind(df, x)

# dummy pension
df$pension_inc <- df$pension == 3 # included in pension
df$pension_exl <- df$pension == 2 # pension exists but not incl

# dummy union
df$union_mem <- df$union == 2 # union member
df$union_mem_incl <- df$union == 3 # not member but coverage

# dummy health
df$is_poorheal <- df$health == 5 # poor health

# dummy disabled
df$is_disabled <- df$incdisab > 0 # is disabled

# sex
df$male <- df$sex == 1

# paid by hour
df$paidhour[df$paidhour == 0] <- NA
df$is_paidhour <- df$paidhour == 2

# citizen
df$is_citizen <- df$citizen != 5

## MODEL

lm_cols <- c(
  # wage
  "log_wage",
  
  # educ
  "is_master", 
  "is_bachel", 
  "is_higsch",
  "is_phd",
  "is_bachel_cum",
  
  # age
  "age",
  
  # sex
  "male",
  
  # famsize
  "famsize",
  
  # singel_parent
  "sing_par",
  # immigration
  "immig_first",
  "immig_secn",
  "citizen",
  # race
  "is_hispan",
  "is_negro",
  "is_asian",
  "is_nativamec",
  # firm size
  "firmsize2",
  "firmsize5", 
  "firmsize7",
  "firmsize8",
  "firmsize9",
  # dummy pension 
  "pension_inc",
  "pension_exl",
  # union
  "union_mem",
  "union_mem_incl",
  # health
  "is_poorheal",
  # disabled
  "is_disabled",
  # paid by hour
  "is_paidhour"
  )

## FILTER

filter <- TRUE
if (filter == TRUE){
  df_lm <- df[, lm_cols]
  print("Filter is on")
  df_lm <- df_lm %>% filter((is_master == TRUE | is_bachel == TRUE))
  print(nrow(df_lm))
} else {
  print("Filter is off")
  df_lm <- df[, lm_cols]
  print(nrow(df_lm))
}

## Dropping 0 cols
x <- data.frame(sapply(df_lm, sum))
colnames(x) <- c("Sum")
x <- c(rownames(filter(x, Sum == 0)))
sprintf("Dropping %s cols", x)
df_lm <- df_lm[ , !(names(df_lm) %in% x)]


## COR
x <- abs(cor(df_lm)) > 0.3
x <- x * 1
x[is.na(x)] = 0
heatmap(x)


## JOINT
# df_lm[, c("log_wage", "age")] %>% plot

## LM
lm1 <- lm(log_wage ~
            is_master * male+
            citizen +
            age +
            male +
            famsize +
            # sing_par +
            immig_first +
            immig_secn +
            is_hispan +
            is_negro +
            # is_asian +
            is_nativamec +
            firmsize2 +
            firmsize5 +
            firmsize7 +
            firmsize8 +
            firmsize9 +
            pension_inc +
            pension_exl +
            union_mem +
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

globalstats_names <- list(
  list("raw" = "nobs", "clean" = "No. of observations", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R²", "fmt" = 4),
  list("raw" = "adj.r.squared", "clean" = "Adjusted R²", "fmt" = 4)
)

tab <- modelsummary(
  ## This refers to the list 'models' specified above.
  models,
  ## A technical adjustment referring to the method used to estimate standard errors
  vcov = "stata",
  ## Include asterisks to indicate level of significance
  stars = TRUE,
  ## This refers to the list 'coefficient_names' specified above.
  # coef_map = coefficient_names,
  ## This refers to the list 'globalstats_names' specified above.
  gof_map = globalstats_names,
  ## This specifies which R-package is used to produce the table.
  output = 'flextable'
)

print(autofit(tab), preview = "pptx")



