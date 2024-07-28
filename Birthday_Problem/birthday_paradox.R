# Load necessary libraries
library(ggplot2)
library(ggthemes)

# Function to simulate the Birthday Paradox
simulate_birthday_paradox <- function(num_people, num_simulations) {
  has_shared_birthday <- function(num_people) {
    birthdays <- sample(1:365, num_people, replace = TRUE)
    return(any(duplicated(birthdays)))
  }
  
  results <- replicate(num_simulations, has_shared_birthday(num_people))
  probability <- mean(results)
  
  return(probability)
}

# Function to generate random birthdates and highlight matches
generate_birthdates <- function(num_people) {
  months <- c("January", "February", "March", "April", "May", "June", 
              "July", "August", "September", "October", "November", "December")
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  birthdates <- sample(1:365, num_people, replace = TRUE)
  date_names <- sapply(birthdates, function(x) {
    cum_days <- cumsum(month_days)
    month <- months[which(x <= cum_days)[1]]
    day <- ifelse(which(x <= cum_days)[1] == 1, x, x - cum_days[which(x <= cum_days)[1] - 1])
    return(paste(month, day))
  })
  unique_dates <- unique(date_names)
  duplicated_dates <- unique(date_names[duplicated(date_names)])
  
  cat("Randomly generated birthdates:\n")
  for (i in 1:num_people) {
    if (date_names[i] %in% duplicated_dates) {
      cat("*", date_names[i], "(duplicate)\n")
    } else {
      cat(date_names[i], "\n")
    }
  }
}

# Function to plot the Birthday Paradox probability
plot_birthday_paradox <- function(max_people, num_simulations) {
  people <- 1:max_people
  probabilities <- sapply(people, function(p) simulate_birthday_paradox(p, num_simulations))
  
  df <- data.frame(People = people, Probability = probabilities * 100)
  
  ggplot(df, aes(x = People, y = Probability)) +
    geom_line(color = "blue", size = 1.5) +
    geom_point(color = "red", size = 3) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = 23, linetype = "dashed", color = "red", size = 1) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Probability of Shared Birthdays",
         subtitle = "Birthday Paradox Simulation",
         x = "Number of People",
         y = "Probability of a Shared Birthday (%)") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      panel.background = element_rect(fill = "#f0f0f0"),
      panel.grid.major = element_line(color = "#d0d0d0"),
      panel.grid.minor = element_blank()
    )
}

# Example usage
num_people <- 23
num_simulations <- 10000

# Print random birthdates with duplicates highlighted
generate_birthdates(num_people)

# Plot the probability curve
plot_birthday_paradox(100, num_simulations)
