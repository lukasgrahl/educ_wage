library(readxl)
library(tidyverse)
library(corrr)
library(tidyr)
library(sjPlot)
library(sjmisc)

wd <- "C:/Users/Lukas Grahl/Documents/GIT/educ_wage"
setwd(wd)
data_dir <- sprintf("%s/data", wd)

# LOAD DATA
cali <- read_excel(path = sprintf("%s/california_schooling.xlsx", data_dir))
# view(cali)

cali$eps <- cali$expn_stu / 1000

cali$str_large <- cali$str >= 20

summary(cali$avginc)
cali$is_low_avginc <- (cali$avginc <= quantile(cali$avginc, 0.25))

x <- lm(testscr ~
     expn_stu * log(avginc) + 
     # is_low_avginc +
     # str + 
     str_large +
     el_pct + 
     meal_pct + 
     log(avginc),
   data = cali)
summary(x)

plot_model(x, type = "pred")# terms = c("expn_stu", "is_low_avginc"))

# CHECK FOR MULTICOLINARIRTY
# naive approach
p <- cali %>% select(str, comp_pct, el_pct, meal_pct, calw_pct) %>% cor %>% heatmap

# corrr library put to use
cali %>% select(str, comp_pct, el_pct, meal_pct, calw_pct) %>% correlate

cali %>% select(str, comp_pct, el_pct, meal_pct, calw_pct) %>% pairs

reg <- lm(cali$testscr ~ cali$str + cali$calw_pct)
summary(reg)

hist(reg$residuals, col = "skyblue", breaks = 40)
mean(reg$residuals)
# plot(reg)

cali$small <- cali$large == 0
cali$small <- cali$small * 1

reg2 <- lm(testscr ~ small + large + calw_pct, data = cali)
summary(reg2)
hist(reg2$residuals, col = "red", breaks = "FD")

# estimate class size effect

# correlation
x <- cor(cali[, c("str", "el_pct", "meal_pct")])
heatmap(x)

reg_uni <- lm(testscr ~ str, data = cali) %>% summary(reg_uni)
# hist(reg_uni$residuals, breaks = "FD")

reg_multi2 <- lm(testscr ~ str + el_pct, data = cali)
summary(reg_multi2)
# plot(reg_multi2$residuals)

reg_multi3 <- lm(testscr ~ str + el_pct + meal_pct, data = cali)
summary(reg_multi3)
# plot(reg_multi3$residuals)

# NON LINEAR REGRESSION
# on the fly method
lm(testscr ~ avginc + I(avginc^2), data = cali) %>% summary

cali$avginc_sq <- cali$avginc^2

cali %>% select(testscr, str, avginc, avginc_sq) %>% pairs

# INTERACTION EFFECTS
# calculate interaction effect unsing english learners and str dummies

cali$high_str <- ifelse(cali$str >= 20, 1, 0)
cali$high_el <- ifelse(cali$el_pct >= 10, 1, 0)

cali[c("testscr", "str", "meal_pct", "avginc")] %>% pairs

int_reg <- lm(testscr ~ high_str
              + high_el
              + high_str * high_el 
              + meal_pct
              + log(avginc)
              ,data = cali)
summary(int_reg)










     