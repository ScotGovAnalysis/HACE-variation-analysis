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
file_path_geographies_2025 <- "Setup documents - to be deleted once storage location decided/HACE+2025+-+2026+-+All+results+by+Geography.xlsx"
file_path_geographies_2023 <- "Setup documents - to be deleted once storage location decided/Health+and+Care+Experience+Survey+2023+to+2024+-tables+of+results+by+geography.xlsx"
file_path_geographies_2021 <- "Setup documents - to be deleted once storage location decided/combined-pnn-info-questions-updated-25-10-22 (1).xlsx"

SG_Practice_lookup <- readRDS("~/HSCA/HACE-variation-analysis/Setup documents - to be deleted once storage location decided/SG_Practice_lookup.rds")

within_2_days_responses <- c(
  "I saw or spoke to a doctor or nurse on the same day",
  "I saw or spoke to a doctor or nurse within 1 or 2 working days"
)

# Function that will create folders for the plots and save them to the working directory
save_plot_with_script_name <- function(plot) {
  
  script_path <- rstudioapi::getActiveDocumentContext()$path
  script_name <- tools::file_path_sans_ext(basename(script_path))
  
  folder_name <- paste0(script_name, " Plots")
  dir.create(folder_name, showWarnings = FALSE)
  
  plot_title <- plot$labels$title
  
  geom_classes <- sapply(plot$layers, function(x) class(x$geom)[1])
  
  plot_type <- if (any(grepl("GeomBoxplot", geom_classes))) {
    "Box plot of"
  } else if ("GeomViolin" %in% geom_classes) {
    "Violin plot of"
  } else if (any(grepl("GeomCol|GeomBar", geom_classes))) {
    "Bar chart of"
  } else if (any(grepl("GeomLine", geom_classes))) {
    "Line plot of"
  } else if (any(grepl("GeomDensity", geom_classes))) {
    "Density plot of"
  } else if (any(grepl("GeomPoint", geom_classes))) {
    "Scatter plot of"
  } else {
    "Plot of"
  }
  
  full_title <- paste(plot_type, plot_title)
  safe_title <- gsub("[^A-Za-z0-9]", " ", full_title)
  
  ggsave(
    filename = file.path(folder_name, paste0(safe_title, ".svg")),
    plot = plot,
    width = 10,
    height = 6
  )
  
  print(geom_classes)
}

# Function for creating histograms
make_histogram <- function(data, 
                           x_var, 
                           title,
                           x_lab = x_lab,
                           y_lab = ylab) {
  
  ggplot(data, aes(x = {{x_var}})) +
    geom_histogram(
      binwidth = 5,
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

make_boxplot_multiple_groups <- function(data,
                                         x_var,
                                         y_var,
                                         title,
                                         x_lab = NULL,
                                         y_lab = NULL) {
  
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_boxplot() +
    labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    theme(
      plot.title = element_text(hjust = 0.5)
    )
}

make_boxplot_single_group <- function(data, 
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
      plot.title = element_text(hjust = 0.5), # centering the main title
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
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

make_barchart_multiple_groups <- function(data,
                                          x_var,
                                          y_var,
                                          title,
                                          x_lab = NULL,
                                          y_lab = NULL) {
  
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_col() +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_text(angle = 90),
      legend.position = "none"
    )
}

