# Monty Hall Simulation Function
monty_hall <- function(iterations, switch) {
  results <- replicate(iterations, {
    # Step 1: Generate doors with one car and two goats
    doors <- c("goat", "goat", "car")
    doors <- sample(doors)  # Randomly place the car behind one of the doors
    
    # Step 2: Player makes a choice
    choice <- sample(1:3, 1)  # Player's initial choice
    
    # Step 3: Host reveals a goat
    car_position <- which(doors == "car")
    available_doors <- setdiff(1:3, c(choice, car_position))
    if (length(available_doors) > 1) {
      reveal <- sample(available_doors, 1)
    } else {
      reveal <- available_doors
    }
    
    # Step 4: Player decides whether to switch
    if (switch) {
      choice <- setdiff(1:3, c(choice, reveal))[1]
    }
    
    # Step 5: Check if the final choice has the car
    result <- doors[choice] == "car"
    return(as.numeric(result))
  })
  
  win_rate <- mean(results)
  loss_rate <- 1 - win_rate
  return(list(win_rate = win_rate, loss_rate = loss_rate))  # Return the win and loss rates
}

# Function to compare both strategies and plot densities
compare_strategies <- function(max_iterations) {
  iteration_points <- c(10, 100, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)
  win_rates_switch <- numeric(length(iteration_points))
  loss_rates_switch <- numeric(length(iteration_points))
  win_rates_stay <- numeric(length(iteration_points))
  loss_rates_stay <- numeric(length(iteration_points))
  
  for (i in seq_along(iteration_points)) {
    iter <- iteration_points[i]
    result_switch <- monty_hall(iter, TRUE)
    result_stay <- monty_hall(iter, FALSE)
    win_rates_switch[i] <- result_switch$win_rate
    loss_rates_switch[i] <- result_switch$loss_rate
    win_rates_stay[i] <- result_stay$win_rate
    loss_rates_stay[i] <- result_stay$loss_rate
  }
  
  df <- data.frame(
    Iterations = rep(iteration_points, 4),
    Rate = c(win_rates_switch, loss_rates_switch, win_rates_stay, loss_rates_stay),
    Strategy = rep(c("Switch (Win)", "Switch (Loss)", "Stay (Win)", "Stay (Loss)"), each = length(iteration_points))
  )
  
  ggplot(df, aes(x = Iterations, y = Rate * 100, color = Strategy)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    geom_hline(yintercept = 66.67, linetype = "dashed", color = "blue", size = 0.75) +
    geom_hline(yintercept = 33.33, linetype = "dashed", color = "red", size = 0.75) +
    annotate("text", x = max(iteration_points), y = 68, label = "66.67%", color = "blue", hjust = 1) +
    annotate("text", x = max(iteration_points), y = 35, label = "33.33%", color = "red", hjust = 1) +
    scale_x_log10(breaks = iteration_points, labels = scales::comma) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = "Win and Loss Rates Over Iterations for Monty Hall Problem",
         subtitle = "Comparison of Switching vs Staying Strategies",
         x = "Iterations (log scale)",
         y = "Rate (%)",
         color = "Strategy") +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  for (i in seq_along(iteration_points)) {
    iter <- iteration_points[i]
    cat(sprintf("Iterations: %d\n", iter))
    cat(sprintf("  Switching - Win rate: %.2f%%, Loss rate: %.2f%%\n", win_rates_switch[i] * 100, loss_rates_switch[i] * 100))
  }
}

# Example Usage
compare_strategies(100000)
