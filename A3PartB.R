#Load necessary packages and libraries
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot")
library(readr)
library(dplyr)
library(ggplot2)
#load the dataset
setwd("C:\\Users\\Dell\\Desktop\\MICAH")
getwd()
data = read.csv("NSSO68main.csv")
head(data)
#perform probit model
data$target <- ifelse(rowSums(data[, c('eggsno_q', 'fishprawn_q', 'goatmeat_q','beef_q', 'pork_q', 'chicken_q', 'othrbirds_q')], na.rm = TRUE) > 0, 1, 0)
probit_model <- glm(target ~ Sector + Sex + Age + MPCE_MRP, data = data, 
                    family = binomial(link = "probit"))
summary(probit_model)