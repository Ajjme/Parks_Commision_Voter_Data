library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(scales)

# Function to convert text to title case
to_title_case <- function(text) {
  str_to_title(text)
}

# Define the function to plot bar graphs for each data frame in the list
plot_summary_list <- function(summary_list, output_dir = "plots") {
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Iterate over the list and create plots
  map2(names(summary_list), summary_list, function(name, data) {
    # Check if data is a data frame and has at least two columns
    if (is.data.frame(data) && ncol(data) >= 2) {
      # Check if data frame is not empty and does not contain only NA values
      if (nrow(data) > 0 && !all(is.na(data[[1]])) && !all(is.na(data[[2]]))) {
        # Extract the columns for plotting
        x_col <- as.character(data[[1]])
        y_col <- data[[2]]
        
        # Create the bar plot with numbers
        plot <- ggplot(data, aes(x = reorder(x_col, y_col), y = y_col)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          geom_text(aes(label = y_col), vjust = -0.5, color = "black") + # Add text labels above bars
          labs(
            title = to_title_case(paste("Bar Plot of", gsub("_", " ", name))),
            x = to_title_case(gsub("_", " ", colnames(data)[1])),
            y = to_title_case(colnames(data)[2])
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          coord_flip()
        
        # Define the filename
        filename <- file.path(output_dir, paste0(gsub(" ", "_", to_title_case(name)), ".png"))
        
        # Save the plot as a PNG file
        ggsave(filename, plot, width = 10, height = 6, dpi = 300)
      }
    }
  })
}

# Example usage:
plot_summary_list(summary_list, output_dir = "my_plots")

### ---------------
library(ggplot2)
library(dplyr)
library(purrr)
library(stringr)
library(scales)

# Function to convert text to title case
to_title_case <- function(text) {
  str_to_title(text)
}

# Define the function to plot bar graphs for each data frame in the list
plot_summary_list <- function(summary_list) {
  # Iterate over the list and create plots
  plots <- map(names(summary_list), function(name) {
    data <- summary_list[[name]]
    
    # Check if data is a data frame and has at least two columns
    if (is.data.frame(data) && ncol(data) >= 2) {
      # Check if data frame is not empty and does not contain only NA values
      if (nrow(data) > 0 && !all(is.na(data[[1]])) && !all(is.na(data[[2]]))) {
        # Extract the columns for plotting
        x_col <- as.character(data[[1]])
        y_col <- data[[2]]
        
        # Create the bar plot with numbers
        ggplot(data, aes(x = reorder(x_col, y_col), y = y_col)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          geom_text(aes(label = y_col), vjust = -0.5, color = "black") + # Add text labels above bars
          labs(
            title = to_title_case(paste("Bar Plot of", gsub("_", " ", name))),
            x = to_title_case(gsub("_", " ", colnames(data)[1])),
            y = to_title_case(colnames(data)[2])
          ) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          coord_flip()
      } else {
        NULL # Return NULL if the data frame is empty or invalid
      }
    } else {
      NULL # Return NULL if the element is not a data frame or has fewer than two columns
    }
  })
  
  # Filter out NULL plots
  plots <- compact(plots)
  
  # Print all valid plots
  walk(plots, print)
}

# Example usage:
 plot_summary_list(summary_list)