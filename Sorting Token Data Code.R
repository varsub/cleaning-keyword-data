


rm(list = ls())
setwd("~/Desktop/Varun/DDL")
library(tidyr)

test <- read.csv("mco2-usd-max.csv")
View(test)



test[grep("2017-*", test$snapped_at), "Year"] = "2017"
test[grep("2018-*", test$snapped_at), "Year"] = "2018"
test[grep("2019-*", test$snapped_at), "Year"] = "2019"
test[grep("2020-*", test$snapped_at), "Year"] = "2020"
test[grep("2021-*", test$snapped_at), "Year"] = "2021"
test[grep("2022-*", test$snapped_at), "Year"] = "2022"

test <- test[!is.na(test$Year),]

tapply(test$market_cap, test$Year, mean, na.rm=TRUE)
