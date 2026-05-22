library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("Hospital Length of Stay Prediction App"),
  
  tabsetPanel(
    tabPanel(
      "Length of Stay Predictor",
      sidebarLayout(
        sidebarPanel(
          numericInput("age", "Average Patient Age", 53, min = 30, max = 80),
          numericInput("infection", "Infection Risk", 4.4, min = 0, max = 10),
          numericInput("xray", "Routine Chest X-Ray Ratio", 82, min = 0, max = 150),
          selectInput("region", "Region", choices = c("NE", "NC", "S", "W")),
          numericInput("census", "Average Daily Census", 191, min = 0, max = 1000),
          numericInput("nurses", "Average Nurses", 173, min = 0, max = 800)
        ),
        mainPanel(
          h3("Predicted Average Length of Stay"),
          verbatimTextOutput("prediction"),
          p("Prediction is based on the final Objective 1 multiple linear regression model.")
        )
      )
    ),
    
    tabPanel(
      "Model Comparison",
      h3("Objective 2 Model Comparison"),
      DTOutput("model_table"),
      plotOutput("rmse_plot"),
      plotOutput("r2_plot")
    )
  )
)

server <- function(input, output) {
  
  predicted_los <- reactive({
    region_nc <- ifelse(input$region == "NC", 1, 0)
    region_s  <- ifelse(input$region == "S", 1, 0)
    region_w  <- ifelse(input$region == "W", 1, 0)
    
    3.044 +
      0.072 * input$age +
      0.456 * input$infection +
      0.014 * input$xray -
      0.932 * region_nc -
      1.224 * region_s -
      2.005 * region_w +
      0.0097 * input$census -
      0.0072 * input$nurses
  })
  
  output$prediction <- renderText({
    paste0(round(predicted_los(), 2), " days")
  })
  
  model_results <- reactive({
    data.frame(
      Model = c("Objective 1 MLR", "Interaction MLR", "Random Forest"),
      RMSE = c(1.2315, 1.2333, 1.3454),
      Rsquared = c(0.5887, 0.5807, 0.5080),
      MAE = c(0.9305, 0.9263, 0.9837)
    )
  })
  
  output$model_table <- renderDT({
    datatable(model_results(), options = list(pageLength = 5))
  })
  
  output$rmse_plot <- renderPlot({
    ggplot(model_results(), aes(x = Model, y = RMSE)) +
      geom_col() +
      labs(
        title = "RMSE Comparison Across Models",
        x = "Model",
        y = "RMSE"
      ) +
      theme_minimal()
  })
  
  output$r2_plot <- renderPlot({
    ggplot(model_results(), aes(x = Model, y = Rsquared)) +
      geom_col() +
      labs(
        title = "R² Comparison Across Models",
        x = "Model",
        y = "R²"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)