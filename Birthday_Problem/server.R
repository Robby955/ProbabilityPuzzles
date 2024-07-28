library(shiny)
library(ggplot2)
library(ggthemes)

# Function to simulate the Birthday Paradox
simulate_birthday_paradox <- function(num_people) {
  birthdays <- sample(1:365, num_people, replace = TRUE)
  return(birthdays)
}

highlight_matches <- function(birthdays) {
  duplicated_indices <- which(duplicated(birthdays) | duplicated(birthdays, fromLast = TRUE))
  highlighted <- ifelse(seq_along(birthdays) %in% duplicated_indices, paste0("*", birthdays), as.character(birthdays))
  return(highlighted)
}

day_of_year_to_date <- function(day_of_year) {
  months <- c("January", "February", "March", "April", "May", "June", 
              "July", "August", "September", "October", "November", "December")
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  cum_days <- cumsum(month_days)
  month <- months[which(day_of_year <= cum_days)[1]]
  day <- ifelse(which(day_of_year <= cum_days)[1] == 1, day_of_year, day_of_year - cum_days[which(day_of_year <= cum_days)[1] - 1])
  return(paste(month, day))
}

shinyServer(function(input, output, session) {
  observeEvent(input$generate, {
    num_people <- input$num_people
    birthdays <- simulate_birthday_paradox(num_people)
    highlighted_birthdays <- highlight_matches(birthdays)
    
    output$birthday_results <- renderText({
      if (any(duplicated(birthdays))) {
        "Some birthdays match!"
      } else {
        "No matching birthdays."
      }
    })
    
    output$birthdays_text <- renderPrint({
      date_names <- sapply(birthdays, day_of_year_to_date)
      highlighted_dates <- sapply(highlighted_birthdays, function(x) {
        if (startsWith(x, "*")) {
          day_of_year_to_date(as.numeric(substring(x, 2)))
        } else {
          day_of_year_to_date(as.numeric(x))
        }
      })
      print(highlighted_dates)
    })
    
    output$birthday_plot <- renderPlot({
      df <- data.frame(Birthday = birthdays)
      ggplot(df, aes(x = Birthday)) +
        geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(title = "Distribution of Birthdays",
             x = "Day of the Year",
             y = "Frequency") +
        scale_y_continuous(breaks = seq(0, max(table(df$Birthday)), by = 1)) +
        theme_minimal(base_size = 15) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          panel.grid.major = element_line(color = "#add8e6"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#e6f2ff")
        )
    })
  })
})
