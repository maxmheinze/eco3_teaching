library(shiny)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("body { font-family: 'Inter', sans-serif; } 
                      #r2Output { font-size: 20px; font-weight: bold; }"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("variation", "Select Variation:", 
                  choices = c("With Random Variation", "Perfect Line")),
      sliderInput("slope", "Slope:", min = 0, max = 1.8, value = 0, step = 0.1)
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      textOutput("r2Output")
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    set.seed(123)
    x <- rnorm(100, mean = 0.5, sd = 0.4)
    x <- pmax(pmin(x, 1), 0)  # Ensure x values are between 0 and 1
    
    if (input$variation == "Perfect Line") {
      y <- x
    } else {
      y <- x + rnorm(100, mean = 0, sd = 0.2)
    }
    y <- pmax(pmin(y, 1), 0)  # Ensure y values are between 0 and 1
    
    data.frame(x, y)
  })
  
  output$scatterPlot <- renderPlot({
    df <- data()
    
    ggplot(df, aes(x, y)) +
      geom_point() +
      geom_abline(intercept = 0.5 - input$slope * 0.5, slope = input$slope, color = "#4072c2", size = 1) +
      labs(x = "x", y = "y") +
      theme_bw()
  })
  
  output$r2Output <- renderText({
    df <- data()
    fitted_y <- (0.5 - input$slope * 0.5) + input$slope * df$x
    ss_total <- sum((df$y - mean(df$y))^2)
    ss_residual <- sum((df$y - fitted_y)^2)
    r2 <- 1 - (ss_residual / ss_total)
    paste("R²: ", round(r2, 2))
  })
}

shinyApp(ui = ui, server = server)
