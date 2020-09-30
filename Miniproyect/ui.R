library(shiny)
library(shinydashboard)
library(plotly)
############################################# Control ##############################################
library(qcc)
####################################################################################################
# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(title = 'Miniproject 2020-30'),
  dashboardSidebar(
    br(),
    h2("Caso de estudio empresa Herrajes Andina S.A.S", align = "center"),
    br(),
    HTML('<center><img src="logo.png" width="150"></center>'),
    br(),
    HTML('<center><img src="abrazadera.png" width="120"></center>'),
    br(),
    h5("Camilo Alvarez", align = "center"),
    h5("Jeffey Villa", align = "center"),
    h5("Mary Jane Rivero", align ="center"),
    h5("Laura Soto",align ="center"),
    h5("Cristiam Bustos",align ="center"),
    h5("Natalia Jimenez",align ="center"),
    br()
  ),
  dashboardBody(
    tabsetPanel(
     tabPanel(title = "Control de calidad", icon = icon("chart-bar",class="fad fa-chart-bar"),
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("cr"), plotlyOutput("cm")),
                splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("comp"), plotlyOutput("pareto"))
              )

    ),
    tabPanel(title = "Transporte", icon = icon("truck", class="fas fa-truck"),
             fluidRow(
               plotOutput("plot1"),
               box(
                 title = "Desea descartar un barrio",
                 selectInput(inputId = "b", label = "Barrio a descartar:",
                             choices = c("Consevar todos",
                                         "Santo.Domingo",
                                         "Olaya",
                                         "El.Recreo",
                                         "Buena.Esperanza",
                                         "El.prado",
                                         "Barrio.Abajo",
                                         "Rebolo",
                                         "San.Isidro")),
                 submitButton())
             )
    ))
    )

  )
)
