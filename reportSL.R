setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readr)
library(visdat)
library(ggplot2)
library(GGally)

train <- as.data.frame(read_csv("bank_marketing_train.csv"))
test <- as.data.frame(read_csv("bank_marketing_test.csv"))
head(train)
head(test)
vis_miss(train)
vis_miss(test)
