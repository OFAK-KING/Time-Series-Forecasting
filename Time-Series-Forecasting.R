# Load needed packages
library(forecast)
library(zoo)
library(tidyverse)

# Importing data needed for the analysis.
Dataset <- read.csv("AustralianWines.csv")

#assessing the imported data
str(Dataset)
head(Dataset)
tail(Dataset)

#reserve the complete entries for the final analysis
Dataset <- Dataset[complete.cases(Dataset), ]

#========================= DATA PREPROCESSING =======================================

# Convert data into time series object
TimeSeriesDataset <- ts(Dataset$Red, start = c(1980, 1), frequency = 12)

#assessing the imported data
str(TimeSeriesDataset)
head(TimeSeriesDataset)
tail(TimeSeriesDataset)

#plot the entire dataset to observe trends, seasons, etc.
par(mar = rep(2, 4))
plot(TimeSeriesDataset, main = " ", xlab = " ", ylab = " ") 
title(main = "Time series plot of Australian red wine sales between 1980 and 1994",
     xlab = "Year", ylab = "Red Wine Sales")

#Partition of the Data set to training set and test set so that the later will be used 
#evaluate the trained by the former, Data set is made up of 180 in total, this analysis
#will save 54 which represent 13% for test set but, these 24 will be the lastest entries of the dataset.

Reserved <- 24
Used <- length(TimeSeriesDataset) - Reserved

#Assigning the training and test set to a continer variables

TrainSet <- window(TimeSeriesDataset, start = c(1980, 1), end = c(1980, Used))
Testset <- window(TimeSeriesDataset, start = c(1980, Used + 1), end = c(1980, Used + Reserved))

#assessing the patitioned datasets
#Trainset
glimpse(TrainSet)

#Testset
glimpse(Testset)

#========================== MODEL TRAINING ==========================================

#======= LINEAR TREND MODEL =========================================================

LinearTrendModel <- tslm(TrainSet ~ trend)
summary(LinearTrendModel)
PredBasedOnLTM <- forecast(LinearTrendModel, h = Reserved, level = 0)

#Evaluating the Trained LINEAR TREND MODEL Performance

accuracy(PredBasedOnLTM, Testset)

#======= LINEAR SEASONALITY MODEL =====================================================

LinearSeasonModel <- tslm(TrainSet ~ season)
summary(LinearSeasonModel)
PredBasedOnLSM <- forecast(LinearSeasonModel, h = Reserved, level = 0)

#Evaluating the Trained LINEAR SEASONALITY MODEL Performance

accuracy(PredBasedOnLSM, Testset)

#======= LINEAR TREND & SEASONALITY MODEL ==============================================

LinearTrendSasonModel <- tslm(TrainSet ~ trend + season)
summary(LinearTrendSasonModel)
PredBasedOnLTSM <- forecast(LinearTrendSasonModel, h = Reserved, level = 0)

#Evaluating the Trained LINEAR TREND & SEASONALITY MODEL Performance

accuracy(PredBasedOnLTSM, Testset)

#======= SIMPLE EXPONENTIAL SMOOTHING MODEL ==============================================

SimpleExponentialSmootHModel <- ses(TrainSet, alpha = NULL, h = Reserved)
summary(SimpleExponentialSmootHModel)

#Evaluating the Trained SIMPLE EXPONENTIAL SMOOTHING MODEL Performance

accuracy(SimpleExponentialSmootHModel, Testset)