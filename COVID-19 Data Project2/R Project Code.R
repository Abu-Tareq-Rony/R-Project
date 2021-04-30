##library for our analysis
library(plm) #model packages
library(readr)#to read csv file
library(lmtest)

#Set working directory
setwd("C:/Users/Abu Tareq Rony/OneDrive/Desktop")

##Read dataset from csv file
dataset <- read_csv("data.csv")

# Difference in differences (DID) model code 
Difference_in_difference_model<-lm(inv ~ capital , data = dataset)
summary(Difference_in_difference_model)#see summary of the model

#first difference(FD) model code
first_difference_model<- plm(inv ~ capital -1, data = dataset,
                  index = c("firm", "year"), 
                  effect = "individual", model = "fd")
summary(first_difference_model)#see summary of the model

#Fixed effects (FE) model code
Fixed_effect_model <- lm(inv ~ capital, data = dataset)
summary(Fixed_effect_model)#see summary of the model

#Random Effects (RE) model code
Random_Effect_Model <- plm(inv ~ capital, data = dataset, 
                    index = c("firm", "year"), 
                    effect = "individual", model = "random")
summary(Random_Effect_Model)#see summary of the model

#Pooled OLS Model code
OLS_model <- lm(inv ~ capital, data = dataset)
summary(OLS_model)#see summary of the model

#Hausman Test code
phtest(first_difference_model, Random_Effect_Model)

#Arellano and Bond code
Arellano_and_Bond_code_model <- coeftest(first_difference_model, vcov.=vcovHC(first_difference_model, method = c("arellano")))
summary(Arellano_and_Bond_code_model)#see summary of the model
