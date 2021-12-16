library(haven)

setwd("C:/Users/Lukas Grahl/Documents/r_project/data")
rm(list = ls())

df <- read_dta("mass_data_set.dta")
view(df)


