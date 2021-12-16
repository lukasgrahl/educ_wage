## BEFORE you run the script, please make sure that these packages are installed:
## readxl
## sandwich
## modelsummary
## flextable

## Here we load the required packages:
library(readxl)
library(sandwich)
library(modelsummary)
library(flextable)

## WARNING: The next command removes all object from your global environment! 
## Please remove the following line if you do not want this to happen.
rm(list = ls())

## Change the working directory, please adapt for your setting.
setwd("C:/Users/Lukas Grahl/Documents/r_project/data")

## Import the dataset from the Excel-file, please adapt for your setting.
cali <- read_excel("california_schooling.xlsx")

## Generate log of avging, which is required for the regressions specified below.
cali$ln_inc <- log(cali$avginc)

## Generate dummy for el_pct >= 10, which is required for the regressions specified below.
cali$high_el <- as.numeric(cali$el_pct >= 10)

## Now we estimate all the models that we want to see in the table.
## The names of the model objects (model1, model2 etc.) can be chosen  arbitrarily.
model1 <- lm(testscr ~ str + el_pct + meal_pct, data = cali)
model2 <- lm(testscr ~ str + el_pct + meal_pct + ln_inc, data = cali)
model3 <- lm(testscr ~ str + high_el + str * high_el, data = cali)
model4 <- lm(testscr ~ str + high_el + str * high_el + meal_pct + ln_inc, data = cali)
model5 <- lm(testscr ~ str + I(str^2) + I(str^3) + high_el + meal_pct + ln_inc, data = cali)
model6 <- lm(testscr ~ str + I(str^2) + I(str^3) + high_el + str * high_el + I(str^2) * high_el + I(str^3) * high_el + meal_pct + ln_inc, data = cali)
model7 <- lm(testscr ~ str + I(str^2) + I(str^3) + el_pct + meal_pct + ln_inc, data = cali)

## Here, we specify which models we want to include in our table and how we label them in the first line.
## The format of each entry in the list is:
## "Name to appear in the first line of the table" = name of saved regression model above
models <- list(
  "(1)" = model1, 
  "(2)" = model2,
  "(3)" = model3, 
  "(4)" = model4, 
  "(5)" = model5,
  "(6)" = model6,
  "(7)" = model7
)

## Here, we specify variable descriptions that will appear in the first column of our table.
## The format of each entry in the list is:
## 'variable used in the lm()-commands above' = 'Name to appear in the first column of the table'
coefficient_names <- c(
  'str' = 'Student-teacher ratio (STR)',
  'I(str^2)' = 'STR²',
  'I(str^3)' = 'STR³',
  'el_pct'= 'Percentage of English learners, continuous',
  'high_el'= 'Percentage of English learners, binary dummy for >= 10%',
  'str:high_el' = 'STR interacted with binary dummy for English learners',
  'I(str^2):high_el' = 'STR² interacted with binary dummy for English learners',
  'I(str^3):high_el' = 'STR³ interacted with binary dummy for English learners',
  'meal_pct' = 'Percentage eligible for subsidized lunch',
  'ln_inc' = 'Average district income (natural log)',
  '(Intercept)' = 'Constant'
)

## Here, we specify the descriptions of the general statistics to appear in the last lines of the table.
## I recommend not to change this.
globalstats_names <- list(
  list("raw" = "nobs", "clean" = "No. of observations", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R²", "fmt" = 4),
  list("raw" = "adj.r.squared", "clean" = "Adjusted R²", "fmt" = 4)
)

## Here, we specify the content of the table to be created.
tab <- modelsummary(
  ## This refers to the list 'models' specified above.
  models,
  ## A technical adjustment referring to the method used to estimate standard errors
  vcov = "stata",
  ## Include asterisks to indicate level of significance
  stars = TRUE,
  ## This refers to the list 'coefficient_names' specified above.
  coef_map = coefficient_names,
  ## This refers to the list 'globalstats_names' specified above.
  gof_map = globalstats_names,
  ## This specifies which R-package is used to produce the table.
  output = 'flextable'
)

## Here, we specify that the table created is to be set as a Powerpoint-slide.
print(autofit(tab), preview = "pptx")
## Here, we specify that the table created is to be set as a Word-document.
## Please uncomment the following line to activate this.
# print(autofit(tab), preview = "docx")