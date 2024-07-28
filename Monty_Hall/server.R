library(shiny)
library(ggplot2)
library(shinythemes)

# Generate doors with one car and two goats
generate_doors <- function() {
  doors <- c("goat", "goat", "car")
  sample(doors)
}

# Define server logic required to run the Monty Hall game
shinyServer(function(input, output, session) {
  # Initial game state
  game_state <- reactiveValues(
    doors = NULL,
    choice = NULL,
    revealed_door = NULL,
    final_choice = NULL,
    result = NULL,
    stage = "welcome",
    switch = NULL,
    runs = 0,
    wins = 0,
    switch_wins = 0,
    switch_runs = 0,
    stay_wins = 0,
    stay_runs = 0
  )
  
  # Welcome message
  output$welcome <- renderText({
    if (game_state$stage == "welcome") {
      return("Welcome to the Monty Hall Problem Interactive Quiz! Are you ready to play?")
    }
    return(NULL)
  })
  
  # Instructions
  output$instructions <- renderUI({
    if (game_state$stage == "choose") {
      tags$p("Please choose a door (1, 2, or 3):")
    } else if (game_state$stage == "reveal") {
      tags$p(paste("You chose door", game_state$choice, ". We reveal a goat behind door", game_state$revealed_door, ". Do you want to switch your choice?"))
    } else if (game_state$stage == "final") {
      tags$p(paste("Your final choice is door", game_state$final_choice, "."))
    }
  })
  
  # Handle game start
  observeEvent(input$start_button, {
    if (game_state$stage == "welcome") {
      game_state$doors <- generate_doors()
      game_state$choice <- NULL
      game_state$revealed_door <- NULL
      game_state$final_choice <- NULL
      game_state$result <- NULL
      game_state$switch <- NULL
      game_state$stage <- "choose"
      updateActionButton(session, "start_button", label = "Submit Choice")
    } else if (game_state$stage == "choose") {
      game_state$choice <- as.numeric(input$door_choice)
      car_position <- which(game_state$doors == "car")
      available_doors <- setdiff(1:3, c(game_state$choice, car_position))
      
      if (length(available_doors) > 1) {
        game_state$revealed_door <- sample(available_doors, 1)
      } else {
        game_state$revealed_door <- available_doors
      }
      game_state$stage <- "reveal"
      updateActionButton(session, "start_button", label = "Submit Switch")
    } else if (game_state$stage == "reveal") {
      game_state$switch <- input$switch_choice == "Yes"
      
      if (game_state$switch) {
        game_state$final_choice <- setdiff(1:3, c(game_state$choice, game_state$revealed_door))[1]
        game_state$switch_runs <- game_state$switch_runs + 1
      } else {
        game_state$final_choice <- game_state$choice
        game_state$stay_runs <- game_state$stay_runs + 1
      }
      
      game_state$result <- game_state$doors[game_state$final_choice] == "car"
      game_state$runs <- game_state$runs + 1
      if (game_state$result) {
        game_state$wins <- game_state$wins + 1
        if (game_state$switch) {
          game_state$switch_wins <- game_state$switch_wins + 1
        } else {
          game_state$stay_wins <- game_state$stay_wins + 1
        }
      }
      game_state$stage <- "final"
      updateActionButton(session, "start_button", label = "Start Over")
    } else if (game_state$stage == "final") {
      game_state$doors <- NULL
      game_state$choice <- NULL
      game_state$revealed_door <- NULL
      game_state$final_choice <- NULL
      game_state$result <- NULL
      game_state$switch <- NULL
      game_state$stage <- "welcome"
      updateActionButton(session, "start_button", label = "Start the Game")
    }
  })
  
  # Door choice
  output$door_choice <- renderUI({
    if (game_state$stage == "choose") {
      return(radioButtons("door_choice", "Choose a door:", choices = c(1, 2, 3)))
    }
    return(NULL)
  })
  
  # Switch choice
  output$switch_choice <- renderUI({
    if (game_state$stage == "reveal") {
      return(radioButtons("switch_choice", "Do you want to switch?", choices = c("Yes", "No")))
    }
    return(NULL)
  })
  
  # Final result
  output$result <- renderUI({
    if (game_state$stage == "final") {
      if (isTRUE(game_state$result)) {
        return(h3("Congratulations! You won the car!", class = "result"))
      } else {
        return(h3("Sorry, you got a goat. Better luck next time!", class = "result"))
      }
    }
    return(NULL)
  })
  
  # Game statistics
  output$score <- renderUI({
    switch_percentage <- ifelse(game_state$switch_runs > 0, round(100 * game_state$switch_wins / game_state$switch_runs, 2), 0)
    stay_percentage <- ifelse(game_state$stay_runs > 0, round(100 * game_state$stay_wins / game_state$stay_runs, 2), 0)
    
    tagList(
      tags$p("Game Statistics:"),
      tags$p(paste("Total Runs:", game_state$runs)),
      tags$p(paste("Total Wins:", game_state$wins)),
      tags$p(paste("Win % when switching:", switch_percentage, "%")),
      tags$p(paste("Win % when not switching:", stay_percentage, "%"))
    )
  })
})
