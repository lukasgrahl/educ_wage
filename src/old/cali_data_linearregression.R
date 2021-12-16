library(readxl)
library(tidyverse)

cali <- read_excel(path = "./r_project/data/california_schooling.xlsx")

# cali["small"] <- cali$str < 20
cali["small"] <- ifelse(cali$str < 20, 1, 0)

view(cali)

# t-testing
# t.test(testscr~large, data = cali)

# ggplot(cali, aes(str, testscr)) + geom_point()

# summary(cali)
# view(cali)
# head(cali)

# plot(cali$str, cali$testscr)

# linear regression
# plot(testscr ~ calw_pct + meal_pct + str, data = cali)

x <- cali[c("calw_pct", "meal_pct", "computer")]

cor(x)

reg_output <- lm(testscr ~ calw_pct + meal_pct + computer, data=cali)
summary(reg_output)

hist(resid(reg_output))

# plot(reg_output)


# Class 4: regress a bivariate variable

# regress against str == small
reg_output <- lm(testscr ~ small, data=cali)
summary(reg_output)

# t-test str == small
x <- t.test(testscr ~ small, data=cali)


# regress against str == large
reg_output <- lm(testscr ~ large, data=cali)

summary(reg_output)
