# Predctive_Modelling_Gedo_Displacement
This project develops several regression models to predict displacement in the Gedo region, Somalia. It compares and evaluates several models: Linear Regression, Decision Tree Regression, and Random Forest Regression. The best-performing model is then selected and used to predict displacement.

## Tools
- R Studio
- R shiny Web App

## Loading necessary packages
``` R
# Load required packages
library(shiny)
library(shinydashboard)
library(readxl)
library(rpart)
library(caret)
library(randomForest)
library(ggplot2)

```

## Loading Data
``` R
# Read data
All_data <- read_excel("UNHCR-PRMN-Displacement-Dataset.xlsx", sheet = "Sheet1")
```

### Data Processing
``` R
# Column renaming
All_data$Displacement <- All_data$`Number of Individuals`

# Convert the reason column to a factor
All_data$Reason <- as.factor(All_data$Reason)

All_data$Weekyear <- as.numeric(All_data$Weekyear)

```
