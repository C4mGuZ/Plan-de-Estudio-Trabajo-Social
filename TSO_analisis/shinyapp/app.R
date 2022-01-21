library(shiny)
library(shinydashboard)
library(igraph)
library(visNetwork)

# ui <- dashboardPage(
#   dashboardHeader(),
#   dashboardSidebar(),
#   dashboardBody()
# )
# 
# server <- function(input, output) { }
# 
# shinyApp(ui, server)



# UI
    # dashboardPage ----
ui <- dashboardPage(skin="purple", 
  
  # dashboarHeader ----
  dashboardHeader(title = "Subred de contenidos",
                  titleWidth = 500),
  
  # dashboardSidebar ----
  dashboardSidebar(sidebarMenu(
    menuItem("Información",
             tabName = "leer",
             badgeLabel = "leer"),
    menuItem("Visualización de Subredes",
             tabName = "redes",
             badgeLabel = "red"))
    ),

  # dashboardBody ----
  dashboardBody(
    fluidRow(
      box(DT::dataTableOutput("tabla")),
      box(title = "n_inter",
          sliderInput("slider", 
                      "Número de palabras en común",
                      1,20,1)
    )
  )
)
)


# SERVER
server <- function(input,output){
  output$tabla <- DT::renderDataTable({
    aristas_2 %>% filter(n_inter>=input$slider)},
    options = list(dom = "Bfrtip",
                   buttons = c("csv", "excel"),
                   scrolly = 400,
                   scroller = TRUE,
                   Scrollx = TRUE,
                   autoWidth = TRUE),
    extensions = c("Buttons", "Scroller"))
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)

