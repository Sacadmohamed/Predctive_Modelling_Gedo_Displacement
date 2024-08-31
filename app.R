# Load required packages
library(shiny)
library(shinydashboard)
library(readxl)
library(rpart)
library(caret)
library(randomForest)
library(ggplot2)

# Read data
All_data <- read_excel("UNHCR-PRMN-Displacement-Dataset.xlsx", sheet = "Sheet1")

# Column renaming
All_data$Displacement <- All_data$`Number of Individuals`

# Convert the reason column to a factor
All_data$Reason <- as.factor(All_data$Reason)

All_data$Weekyear <- as.numeric(All_data$Weekyear)

# Set a seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
train_indices <- createDataPartition(All_data$Displacement, p = 0.8, list = FALSE)
train_data <- All_data[train_indices, ]
test_data <- All_data[-train_indices, ]

# Train a decision tree model
tree_model <- rpart(Displacement ~ Weekyear + Reason, data = train_data)

# Make predictions on the testing set using the decision tree model
tree_predictions <- predict(tree_model, newdata = test_data)

# Train a linear regression model
linear_model <- lm(Displacement ~ Weekyear + Reason, data = train_data)

# Make predictions on the testing set using the linear regression model
linear_predictions <- predict(linear_model, newdata = test_data)

# Train a random forest model
forest_model <- randomForest(Displacement ~ Weekyear + Reason, data = train_data)

# Make predictions on the testing set using the random forest model
forest_predictions <- predict(forest_model, newdata = test_data)

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

# Define server
server <- function(input, output) {
  # Render decision tree plot
  output$tree_plot <- renderPlot({
    ggplot(data = test_data, aes(x = Weekyear, y = Displacement)) +
      geom_point(color = "blue", size = 3) +
      geom_line(aes(y = tree_predictions), color = "red", size = 1) +
      labs(title = "Decision Tree Regression: Predicted vs. Actual", x = "Week of the Year", y = "Displacement")
  })
  
  # Render linear regression plot
  output$linear_plot <- renderPlot({
    ggplot(data = test_data, aes(x = Weekyear, y = Displacement)) +
      geom_point(color = "blue", size = 3) +
      geom_line(aes(y = linear_predictions), color = "red", size = 1) +
      labs(title = "Linear Regression: Predicted vs. Actual", x = "Week of the Year", y = "Displacement")
  })
  
  # Render random forest plot
  output$forest_plot <- renderPlot({
    ggplot(data = test_data, aes(x = Weekyear, y = Displacement)) +
      geom_point(color = "blue", size = 3) +
      geom_line(aes(y = forest_predictions), color = "red", size = 1) +
      labs(title = "Random Forest Regression: Predicted vs. Actual", x = "Week of the Year", y = "Displacement")
  })
  
  # Render performance metrics table
  output$performance_table <- renderTable({
    performance_data <- data.frame(
      Model = c("Decision Tree", "Linear Regression", "Random Forest"),
      RMSE = c(tree_performance$RMSE, linear_performance$RMSE, forest_performance$RMSE),
      MAE = c(tree_performance$MAE, linear_performance$MAE, forest_performance$MAE)
    )
    performance_data
  })
  
  
  # RMSE plot
  output$RMSE_error <- renderPlot({
    performance_data_RMSE <- data.frame(
      Model = c("Decision Tree", "Linear Regression", "Random Forest"),
      RMSE = c(tree_performance$RMSE, linear_performance$RMSE, forest_performance$RMSE)
    )
    
    ggplot(data = performance_data_RMSE, aes(x = Model, y = RMSE)) +
      geom_col(fill = "#070db5") +
      labs(x = "Model", y = "RMSE")
  })
  
  
  
  
  # MAE plot
  output$MAE_error <- renderPlot({
    performance_data_MAE <- data.frame(
      Model = c("Decision Tree", "Linear Regression", "Random Forest"),
      MAE = c(tree_performance$MAE, linear_performance$MAE, forest_performance$MAE)
    )
    
    ggplot(data = performance_data_MAE, aes(x = Model, y = MAE)) +
      geom_col(fill = "orange") +
      labs(x = "Model", y = "MAE")
  })
  
  # Displacement Predictor
  observeEvent(input$predict_button, {

    
    # Prepare the input data for prediction
    predict_data <- data.frame(
      Weekyear = as.numeric(input$week_input),
      Reason = as.factor(input$reason_input)
    )
    
    # Convert the reason column to a factor
    levels(predict_data$Reason) <- levels(All_data$Reason)
    
    # Make prediction using the random forest model
    prediction <- predict(forest_model, newdata = predict_data)
    
    # Output the prediction
    output$prediction_output <- renderText({
      paste("Predicted number of displaced people:", prediction)
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)