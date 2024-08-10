# Predictive_Modelling_Gedo_Displacement
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
## Set the seed
``` R
# Set a seed for reproducibility
set.seed(123)
```
## Data Splitting
``` R
# Split the data into training and testing sets
train_indices <- createDataPartition(All_data$Displacement, p = 0.8, list = FALSE)
train_data <- All_data[train_indices, ]
test_data <- All_data[-train_indices, ]
```

## 1: Decision Tree Model
``` R
# Train a decision tree model
tree_model <- rpart(Displacement ~ Weekyear + Reason, data = train_data)

# Make predictions on the testing set using the decision tree model
tree_predictions <- predict(tree_model, newdata = test_data)

```
![DecisionTree](https://github.com/user-attachments/assets/e8a415ed-0ebb-417c-93df-6bcfd6affadc)


## 2: Linear Regression Model
``` R
# Train a linear regression model
linear_model <- lm(Displacement ~ Weekyear + Reason, data = train_data)

# Make predictions on the testing set using the linear regression model
linear_predictions <- predict(linear_model, newdata = test_data)
```
![Linear_Regression](https://github.com/user-attachments/assets/c2210dbf-a626-41ce-a3db-30fe1f54d7e2)


## 3: Random Forest Model
``` R
# Train a random forest model
forest_model <- randomForest(Displacement ~ Weekyear + Reason, data = train_data)

# Make predictions on the testing set using the random forest model
forest_predictions <- predict(forest_model, newdata = test_data)

```
![Random_Forest](https://github.com/user-attachments/assets/3e2b7c0b-1ec8-4349-a744-3c502d38fb63)


## Calculating Performance Metrics
``` R
# Calculate performance metrics for decision tree regression
tree_performance <- data.frame(
  RMSE = sqrt(mean((tree_predictions - test_data$Displacement)^2)),
  MAE = mean(abs(tree_predictions - test_data$Displacement))
)

# Calculate performance metrics for linear regression
linear_performance <- data.frame(
  RMSE = sqrt(mean((linear_predictions - test_data$Displacement)^2)),
  MAE = mean(abs(linear_predictions - test_data$Displacement))
)

# Calculate performance metrics for random forest regression
forest_performance <- data.frame(
  RMSE = sqrt(mean((forest_predictions - test_data$Displacement)^2)),
  MAE = mean(abs(forest_predictions - test_data$Displacement))
)
```

## Building User Interface (ui) Side 
``` R
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Displacement Prediction Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Regression Analysis", tabName = "regression_analysis"),
      menuItem("Error Visualization", tabName = "error_visualiz"),
      menuItem("Displacement Predictor", tabName = "displacement_predictor")
      
    )
  ),
  dashboardBody(
    tabItems(
      # Regression Analysis tab
      tabItem(tabName = "regression_analysis",
              fluidRow(
                box(
                  title = "Decision Tree Regression",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("tree_plot")
                ),
                box(
                  title = "Linear Regression",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("linear_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Random Forest Regression",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("forest_plot")
                )
              )
              
      ),
      
      
      ### Error Visualization Tab
      tabItem(tabName = "error_visualiz",
              fluidRow(
                box(
                  title = "Performance Metrics",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  tableOutput("performance_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Root Mean Square Error",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("RMSE_error")
                )
              ),
              
              fluidRow(
                box(
                  title = "Mean Absolute Error",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("MAE_error")
                )
              )
              
              
      ),
      
      # Displacement Predictor tab
      tabItem(tabName = "displacement_predictor",
              fluidRow(
                box(
                  title = "Displacement Predictor",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  numericInput("week_input", "Number of Weeks in the Year:", value = NULL),
                  selectInput("reason_input", "Reason of Displacement:", choices = unique(All_data$Reason)),
                  actionButton("predict_button", "Predict"),
                  textOutput("prediction_output")
                )
              )
      )
    )
  )
)

```
