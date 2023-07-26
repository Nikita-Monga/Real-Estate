
library(tidymodels)
library(visdat)
library(tidyr)
library(car)
ld_train=read.csv("C:/Users/Ankush Monga/Downloads/housing_train (2).csv")
ld_test=read.csv("C:/Users/Ankush Monga/Downloads/housing_test (2).csv")
summary(ld_train)


#replace NA with mean in original dataset(ld_train)
ld_train$Bedroom2[is.na(ld_train$Bedroom2)] = 2.786
ld_train$Bathroom[is.na(ld_train$Bathroom)] = 1.50
ld_train$Car[is.na(ld_train$Car)] = 1.526
ld_train$Landsize[is.na(ld_train$Landsize)] = 454.2
ld_train$BuildingArea[is.na(ld_train$BuildingArea)] = 144.6
ld_train$YearBuilt[is.na(ld_train$YearBuilt)] = 1961
#splitting data into train & test
set.seed(2)
library(caTools)
split= sample.split(ld_train, SplitRatio = 0.8)
split
train= subset(ld_train, split='TRUE')
test=subset(ld_train, split='FALSE')
train
test
#creating model
model=lm(Price~Distance+Bedroom2+Bathroom+Car+Landsize+BuildingArea, data = train)
summary(model)
#prediction
pred=predict(model, test)
#finding accuracy
attach(train)
attach(ld_train)
rmse= sqrt(mean(pred-Price)^2)
rmse
write.csv(pred, 'project1.csv', row.names = F)
