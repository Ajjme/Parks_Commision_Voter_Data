library(ggplot2)
library(dplyr)
library(purrr)

# Define the function to plot bar graphs for each data frame in the list
plot_summary_list <- function(summary_list) {
  # Iterate over the list and create plots
  plots <- map(names(summary_list), function(name) {
    data <- summary_list[[name]]
    
    # Ensure the data frame has at least two columns and is not empty
    if (ncol(data) >= 2 && nrow(data) > 0) {
      # Extract the columns for plotting
      x_col <- data[[1]]
      y_col <- data[[2]]
      
      # Create the bar plot
      ggplot(data, aes(x = reorder(as.character(x_col), y_col), y = y_col)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(
          title = paste("Bar Plot of", gsub("_", " ", name)),
          x = gsub("_", " ", colnames(data)[1]),
          y = colnames(data)[2]
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_flip()
    } else {
      NULL # Return NULL for plots that can't be generated
    }
  })
  
  # Filter out NULL plots
  plots <- compact(plots)
  
  # Print all valid plots
  walk(plots, print)
}

# Example usage:
plot_summary_list(summary_list)