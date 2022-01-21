library(shiny)
# runExample("01_hello")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Holi Mundi"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "rectan",
                  label = "Número de bins:",
                  min = 1,
                  max = 30,
                  value = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    rectan <- seq(min(x), max(x), length.out = input$rectan + 1)
    
    hist(x, breaks = rectan, col = "#02519c", border = "grey",
         xlab = "Tiempos entre erupciones (en minutos)",
         main = "Histograma de tiempos de espera")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
