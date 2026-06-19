## Utility Script ##
# Download the most recent publication tables from https://www.gov.scot/collections/health-and-care-experience-survey/ #

#Load required packages#
library(readxl)
library(purrr)
library(dplyr)
library(tidyverse)
library(sgplot)
library(ggplot2)

# File path to the most recent years data #
# TO BE UPDATED WHEN STORAGE LOCATION DECIDED #
file_path_demographics <- "Setup documents - to be deleted once storage location decided/HACE+2025+-+2026+-+All+results+by+Demographic+Characteristic.xlsx"
file_path_geographies <- "Setup documents - to be deleted once storage location decided/HACE+2025+-+2026+-+All+results+by+Geography.xlsx"

# Function that will create folders for the plots and save them to the working directory
save_plot_with_script_name <- function(plot) {
  
  script_path <- rstudioapi::getActiveDocumentContext()$path
  script_name <- tools::file_path_sans_ext(basename(script_path))
  
  folder_name <- paste0(script_name, " Plots")
  dir.create(folder_name, showWarnings = FALSE)
  
  plot_title <- plot$labels$title
  
  geom_classes <- unlist(lapply(plot$layers, function(x) class(x$geom)))
  
  plot_type <- if (any(grepl("GeomBoxplot", geom_classes))) {
    "Box plot of"
  } else if (any(grepl("GeomBar|GeomRect", geom_classes))) {
    "Histogram of"
  } else if (any(grepl("GeomPoint", geom_classes))) {
    "Scatter plot of"
  } else if (any(grepl("GeomLine", geom_classes))) {
    "Line plot of"
  } else if (any(grepl("GeomDensity", geom_classes))) {
    "Density plot of"
  } else {
    "Plot of"
  }
  
  full_title <- paste(plot_type, plot_title)
  safe_title <- gsub("[^A-Za-z0-9]", " ", full_title)
  
  ggsave(
    filename = file.path(folder_name, paste0(safe_title, ".svg")),
    plot = plot,
    width = 6.26,
    height = 3.94
  )
}

# Function for creating histograms
make_histogram <- function(data, 
                           x_var, 
                           title,
                           x_lab = x_lab,
                           y_lab = ylab) {
  
  ggplot(data, aes(x = {{x_var}})) +
    geom_histogram(
      binwidth = 3,
      boundary = 0,
      colour = "white",
      linewidth = 0.4
    ) +
    geom_density(alpha = 0.2) +
    scale_x_continuous(limits = c(0, 100)) +
    labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    theme(
      axis.title.y = element_text(angle = 90),
      plot.title = element_text(hjust = 0.5)
    )
}

make_boxplot <- function(data, 
   x_var, 
   title,
   x_lab = x_lab,
   y_lab = y_lab) 
  {
  ggplot(data, aes(x = {{x_var}})) +
    geom_boxplot() +
    scale_x_continuous(limits = c(0, 100)) +
    labs(
      title = title,
      x = x_lab,
      y = y_lab
    )+
    theme(
      plot.title = element_text(hjust = 0.5) # centering the main title
    )
}

make_scatter <- function(data, x_var, y_var, title,
                              x_lab = x_lab,
                              y_lab = y_lab) {
  
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_point(size = 2.5) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    theme(
      axis.title.y = element_text(angle = 90),
      plot.title = element_text(hjust = 0.5)
    )
}


