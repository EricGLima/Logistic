# May 04, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Setup Configurations

# Libraries
library(adabag)
library(boot)
library(caret)
library(modelr)
library(qdap)
library(randomForest)
library(tidyverse)
library(tree)

# Working Directory
setwd("C:/Users/Eric/Desktop/Logística/FirstWork/Databases")

# Seed
seed = 10

# Database
data = read.csv('winequality-red.csv', header=T, sep=';')

# Defining quality
#data$quality = as.factor(data$quality)
#data$quality = as.factor(ifelse(data$quality==8, 'Aprovado', 'Reprovado')) 
data$quality = as.factor(ifelse(data$quality>=6, 'Aprovado', 'Reprovado')) 

#data$quality = as.factor(
#  ifelse(
#    data$quality >= 7, 'Aprovado', ifelse(
#      data$quality >= 5, 'Media', 'Reprovado'
#    )
#  )

#)