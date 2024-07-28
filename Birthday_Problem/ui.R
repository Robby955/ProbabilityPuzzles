library(shiny)

shinyUI(fluidPage(
  titlePanel("Birthday Paradox Simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_people", "Number of People:", 23, min = 1, max = 100),
      actionButton("generate", "Generate Birthdays")
    ),
    mainPanel(
      textOutput("birthday_results"),
      verbatimTextOutput("birthdays_text"),
      plotOutput("birthday_plot")
    )
  )
))
