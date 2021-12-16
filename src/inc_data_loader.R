library(haven)
library(tidyr)
library(tidyverse)

# set working dir
setwd("C:/Users/Lukas Grahl/Documents")

# remove all data in evironment
rm(list = ls())

data <- read_dta("./r_project/data/CPS dataset.dta")
rel_cols <- c("race", "relate", "sex", "age")

# split years
data00 <- data[data$year == 2000, ]
data01 <- data[data$year == 2001, ]
data02 <- data[data$year == 2002, ]
data03 <- data[data$year == 2003, ]
data04 <- data[data$year == 2004, ]
data05 <- data[data$year == 2005, ]
data06 <- data[data$year == 2006, ]
data07 <- data[data$year == 2007, ]
data08 <- data[data$year == 2008, ]
data09 <- data[data$year == 2009, ]
data10 <- data[data$year == 2010, ]
data11 <- data[data$year == 2011, ]
data12 <- data[data$year == 2012, ]
data13 <- data[data$year == 2013, ]
data14 <- data[data$year == 2014, ]
data15 <- data[data$year == 2015, ]
data16 <- data[data$year == 2016, ]
data17 <- data[data$year == 2017, ]
data18 <- data[data$year == 2018, ]
data19 <- data[data$year == 2019, ]
data20 <- data[data$year == 2020, ]

save(data00, file="./r_project/data/data00.Rda")
save(data01, file="./r_project/data/data01.Rda")
save(data02, file="./r_project/data/data02.Rda")
save(data03, file="./r_project/data/data03.Rda")
save(data04, file="./r_project/data/data04.Rda")
save(data05, file="./r_project/data/data05.Rda")
save(data06, file="./r_project/data/data06.Rda")
save(data07, file="./r_project/data/data07.Rda")
save(data08, file="./r_project/data/data08.Rda")
save(data09, file="./r_project/data/data09.Rda")
save(data10, file="./r_project/data/data10.Rda")
save(data11, file="./r_project/data/data11.Rda")
save(data12, file="./r_project/data/data12.Rda")
save(data13, file="./r_project/data/data13.Rda")
save(data14, file="./r_project/data/data14.Rda")
save(data15, file="./r_project/data/data15.Rda")
save(data16, file="./r_project/data/data16.Rda")
save(data17, file="./r_project/data/data17.Rda")
save(data18, file="./r_project/data/data18.Rda")
save(data19, file="./r_project/data/data19.Rda")
save(data20, file="./r_project/data/data20.Rda")