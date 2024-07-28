library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  
  titlePanel("Monty Hall Problem Interactive Quiz"),
  
  tags$style(HTML("
    .instructions {
      font-size: 24px;
      font-weight: bold;
    }
    .result {
      font-size: 24px;
      font-weight: bold;
      color: red;
    }
    .btn-primary {
      font-size: 18px;
      font-weight: bold;
    }
    .game-stats {
      font-size: 18px;
      margin-top: 20px;
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      textOutput("welcome"),
      actionButton("start_button", "Start the Game", class = "btn-primary"),
      uiOutput("door_choice"),
      uiOutput("switch_choice"),
      uiOutput("submit_button"),
      uiOutput("result"),
      div(uiOutput("score"), class = "game-stats")
    ),
    mainPanel(
      div(uiOutput("instructions"), class = "instructions")
    )
  )
))
