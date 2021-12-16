library(readxl)
library(tidyverse)

cali <- read_excel(path = "./r_project/data/california_schooling.xlsx")

# t-testing
t.test(testscr~large, data = cali)

ggplot(cali, aes(enrl_tot, testscr)) + geom_point()
