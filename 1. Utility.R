#==============================================================================
## Utility Script ##
#==============================================================================
SCRIPT_NAME <- "1. Utility"

## Each year run the updates script first to include the new years data. Once you
## have run that, you can run the rest of the code.

#Load required packages#
library(readxl)
library(purrr)
library(dplyr)
library(tidyverse)
library(sgplot)
library(ggplot2)
library(stringr)


#If you have not run the Updates script manually set survey year
survey_year <- "2025-26"
current_survey_year <- survey_year

#Read in master data set
master_data_all <- readRDS("Clean data/master_data_all.rds")
# Read in lookup data set to determine what practices sit in each cluster in which hscp etc
SG_Practice_lookup <- readRDS("~/HSCA/HACE/HACE-variation-analysis/Clean data/SG_Practice_lookup.rds")

# SG Core value colors for slide pack
access_colour <- "#F46A25"
quality_colour <- "#19AB19"
person_centered_colour <- "#D071A7"

#Bands for binned barcharts
bands <- paste0(seq(0, 90, 10), "-", seq(10, 100, 10))

## Question lookup function
question_lookup_year <- readRDS(
  "Clean data/question_lookup_year.rds"
)

question_easy_contact <- question_lookup_year %>%
  filter(
    question_type == "easy_contact",
    survey_year == current_survey_year
  ) %>%
  pull(question_number)

question_overall_care <- question_lookup_year %>%
  filter(
    question_type == "overall_care",
    survey_year == current_survey_year
  ) %>%
  pull(question_number)

question_informed_choice <- question_lookup_year %>%
  filter(
    question_type == "informed_choice",
    survey_year == current_survey_year
  ) %>%
  pull(question_number)

question_OOH_care <- question_lookup_year %>%
  filter(
    question_type == "OOH_care",
    survey_year == current_survey_year
  ) %>%
  pull(question_number)

question_OOH_overall_care <- question_lookup_year %>%
  filter(
    question_type == "OOH_overall_care",
    survey_year == current_survey_year
  ) %>%
  pull(question_number)

question_lookup <- list(
  easy_contact = question_lookup_year %>%
    filter(question_type == "easy_contact") %>%
    pull(question_number) %>%
    unique(),
  
  overall_care = question_lookup_year %>%
    filter(question_type == "overall_care") %>%
    pull(question_number) %>%
    unique(),
  
  informed_choice = question_lookup_year %>%
    filter(question_type == "informed_choice") %>%
    pull(question_number) %>%
    unique(),
  
  OOH_care = question_lookup_year %>%
    filter(question_type == "OOH_care") %>%
    pull(question_number) %>%
    unique(),
  
  OOH_overall_care = question_lookup_year %>%
    filter(question_type == "OOH_overall_care") %>%
    pull(question_number) %>%
    unique()
)

master_total <- master_data_all %>%
  filter(
    Year == current_survey_year,
    Sex == "Total",
    `Age Band` == "Total",
    SIMD == "Total",
    `Urban-Rural 8` == "Total",
    `Long-Term Condition` == "Total",
    `Chronic Pain` == "Total",
    `Sexual Orientation` == "Total",
    Ethnicity == "Total"
  )

master_demographics <- master_data_all %>%
  filter(
    Year == current_survey_year,
    Area == "Scotland"
  )

variation_data <- master_total %>%
  filter(
    Year == current_survey_year,
    `Area Type` == "GP Practice") %>%
  left_join(
    SG_Practice_lookup,
    by = c("Area" = "gp_name_letter")
  ) %>%
  select(
    Year,`Question Number`,Topic,`Question Text`,`Response Option`,`Area Type`,
    hb_code,hb_name,ca_code,hscp_gpcl_name,hscp_code,hscp_name,gp_prac_no,
    gp_name_official,practicelistsize,Area,`Number of Responses`,Percentage,
    `Lower 95% Confidence Interval`,`Upper 95% Confidence Interval`
  )

numeric_cols <- c(
  "Number of Responses",
  "Percentage",
  "Lower 95% Confidence Interval",
  "Upper 95% Confidence Interval"
)

#add year to title function
add_year <- function(title_text) {
  paste0(title_text, ", ", current_survey_year)
}

# Function that will create folders for the plots using each individual script name 
# and save them to the working directory. It then saves the plot using the title of
# the plot as the name, and any specified width and height measurements in cm.  
save_plot_with_script_name <- function(
    plot,
    width = 20,
    height = 12,
    show_title = TRUE
) {
  
  script_name <- get("SCRIPT_NAME", envir = .GlobalEnv)
  # script_path <- rstudioapi::getActiveDocumentContext()$path
  # script_name <- tools::file_path_sans_ext(basename(script_path))
  
  folder_name <- paste0(script_name, " Plots")
  
  dir.create(folder_name,showWarnings = FALSE)
  
  plot_title <- plot$labels$title
  
  geom_classes <- sapply(plot$layers,function(x) class(x$geom)[1]
                         )
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
  
  plot_to_save <- if (show_title) {
    
    plot +
      labs(
        title = stringr::str_wrap(
          plot$labels$title,
          width = 120
        )
      ) +
      theme(
        plot.title.position = "plot",
        plot.title = element_text(
          hjust = 0.15
        )
      )
    
  } else {
    plot + labs(title = NULL)
  }
  
  ggsave(
    filename = file.path(folder_name, paste0(safe_title, ".svg")
    ),
    plot = plot_to_save,
    width = width,
    height = height,
    units = "cm"
  )
  
}


# Functions for creating various chart types.
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
    )+
    theme_minimal(base_size = 12)
}

make_boxplot_multiple_groups <- function(data,
                                         x_var,
                                         y_var,
                                         title,
                                         x_lab = NULL,
                                         y_lab = NULL,
                                         bar_width = 0.9,
                                         bar_colour = NULL) {
  
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_boxplot() +
    geom_col(
      fill = bar_colour,
      width = bar_width
    ) +
    
    labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    theme(
      plot.title = element_text(hjust = 0.5)
    )+
    theme_minimal(base_size = 12)
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
      )+
    theme_minimal(base_size = 12)
}

make_scatter <- function(data, x_var, y_var, title,
                              x_lab = x_lab,
                              y_lab = y_lab) {
  
  title <- paste0(
    title,
    ", ",
    current_survey_year
  )
  
  title <- stringr::str_wrap(
    title,
    width = 120
  )
  
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
    )+
    theme_minimal(base_size = 12)
}

make_barchart_multiple_groups <- function(
    data,
    x_var,
    y_var,
    title,
    x_lab = NULL,
    y_lab = NULL,
    bar_width = 0.75,
    bar_colour = "#0b4c0b"
) {
  
  title <- paste0(
    title,
    ", ",
    current_survey_year
  )
  
  title <- stringr::str_wrap(
    title,
    width = 80
  )
  
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_col(
      fill = bar_colour,
      width = bar_width
    ) +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.margin = margin(
        t = 20,
        r = 20,
        b = 10,
        l = 10
      ),
      axis.title.y = element_text(angle = 90),
      legend.position = "none"
    )
}

make_jitter_plot <- function(
    data,
    x_var,
    y_var,
    title,
    x_lab = NULL,
    y_lab = NULL,
    colour_var = NULL,
    shape_var = NULL
) {
  
  title <- paste0(
    title,
    ", ",
    current_survey_year
  )
  
  ggplot(
    data,
    aes(
      x = {{x_var}},
      y = {{y_var}},
      colour = {{colour_var}},
      shape = {{shape_var}}
    )
  ) +
    geom_jitter(
      width = 0.25,
      height = 0,
      size = 3
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        hjust = 0,
        margin = margin(b = 10)
      ),
      plot.margin = margin(
        t = 20,
        r = 20,
        b = 10,
        l = 10
      ),
      legend.position = "none"
    )
}
