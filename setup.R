# May 04, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Setup Configurations

# Libraries
library(adabag)
library(caret)
library(modelr)
library(qdap)
library(randomForest)
library(tidyverse)
library(tree)

# Working Directory
setwd("C:/Users/Eric/Desktop/Logística/Trabalhos/Databases")

# Seed
seed = 10

# Database
data = read.csv('winequality-red.csv', header=T, sep=';')
