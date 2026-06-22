### Analysis for HACE Q16
# June 2026

## HSC vision portfolio indicators 
# General Practice – informed choice

## What it measures 
#Percentage of people responding, 'I felt able to make an informed choice about 
# my treatment and care'.

## HACE Question (Q16)
# Q16 Thinking about that healthcare professional, how much do you agree or disagree 
# with the following statements? 


## Core value
# Person-centered – delivering outcomes that matter to people

#------------------------------------------------------------------------------#
# Set SGplot for default chart colours
sgplot::use_sgplot()
#Source function to save plots from utility script
source("1. Utility.R")
#Load clean data from rds scripts
data_list_demographics <- readRDS("Clean data/data_list_demographics_clean.rds")
data_list_geographies <- readRDS("Clean data/data_list_geographies_clean.rds")

# Summary table showing the Percentage of people responding positively to, 'I felt able to 
# make an informed choice about my treatment and care
informed_choice_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` == "positive") %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`GP Practice name`) %>%
  # For each GP practice, sum the percentages of the selected response options
  summarise(
    percentage_informed_choice_GP = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_informed_choice_GP) %>%
  # Order practices by % respondents positive, lowest to highest
  mutate(order = row_number())

#Scatteplot by GP practice
informed_choice_GP_scatterplot <- make_scatter(data = informed_choice_GP,
             x_var = order, 
             y_var = percentage_informed_choice_GP,
             title = str_wrap(
               "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care by GP practice", 
               width = 60
             ), 
             x_lab = "GP practice",
             y_lab = "Percentage (%) responding positively"
             )
# Saves plot to working directory
save_plot_with_script_name(informed_choice_GP_scatterplot)

# Box plot by GP practice
informed_choice_GP_boxplot <- make_boxplot_single_group(data = informed_choice_GP,
             x_var = percentage_informed_choice_GP, 
             title = str_wrap(
               "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care by GP practice", 
               width = 60
             ),
             x_lab = "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care",
             y_lab = "")
informed_choice_GP_boxplot
# Save plot to working directory
save_plot_with_script_name(informed_choice_GP_boxplot)

## Histogram by GP Practice ##
informed_choice_GP_histogram <- make_histogram(
   data = informed_choice_GP, 
   x_var = percentage_informed_choice_GP, 
   title = str_wrap(
     "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care by GP practice", 
     width = 60
   ), 
   x_lab = "Percentage (%) responding positively",
   y_lab = "Number of GP Practices")
informed_choice_GP_histogram
# Save plot to working directory
save_plot_with_script_name(informed_choice_GP_histogram)

#------------------------------------------------------------------------------#

## By GP Cluster 
informed_choice_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive") %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`Area`) %>%
  # For each GP practice, sum the percentages of the selected response options
  summarise(
    percentage_informed_choice_cluster = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_informed_choice_cluster) %>%
  # Order practices by % urgent respondants positive, lowest to highest
  mutate(order = row_number())

#Scatterplot by GP cluster
informed_choice_cluster_scatterplot <- make_scatter(
   data = informed_choice_cluster,
   x_var = order, 
   y_var = percentage_informed_choice_cluster,
   title = str_wrap(
     "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care by GP cluster", 
     width = 60
   ), 
   x_lab = "GP cluster",
   y_lab = "Percentage (%) responding positively"
)
informed_choice_cluster_scatterplot
# Saves plot to working directory
save_plot_with_script_name(informed_choice_cluster_scatterplot)

#Boxplot by GP cluster
informed_choice_cluster_boxplot <- make_boxplot_single_group(
  data = informed_choice_cluster,
  x_var = order,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care by GP cluster", 
    width = 60
  ),
  x_lab = "Percentage",
  y_lab = "")
informed_choice_cluster_boxplot
# Saves plot to working directory
save_plot_with_script_name(informed_choice_cluster_boxplot)

## Histogram by GP Cluster ##
informed_choice_cluster_histogram <- make_histogram(
  data = informed_choice_cluster, 
  x_var = percentage_informed_choice_cluster, 
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care by GP cluster", 
    width = 60
  ), 
  x_lab = "Percentage (%) responding positively",
  y_lab = "Number of GP clusters")
informed_choice_cluster_histogram
# Save plot to working directory
save_plot_with_script_name(informed_choice_cluster_histogram)


