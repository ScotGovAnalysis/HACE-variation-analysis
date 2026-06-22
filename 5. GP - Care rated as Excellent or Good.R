### Analysis for HACE Q13
# June 2026

## HSC vision portfolio indicators 
# General Practices - Care rated as Excellent or Good

## What it measures 
# Percentage of people rating the overall care from their General Practice as 
# excellent or good.

## HACE Question (Q13)
# Q13 Overall, how would you rate the care provided by your GP Practice?


## Core value
# Quality – We get the care we need

#------------------------------------------------------------------------------#

# Set SGplot for default chart colours
sgplot::use_sgplot()

#Source function to save plots from utility script
source("1. Utility.R")
#Load clean data from rds scripts
data_list_demographics <- readRDS("Clean data/data_list_demographics_clean.rds")
data_list_geographies <- readRDS("Clean data/data_list_geographies_clean.rds")

# Summary table showing the percentage of respondents who rated the overall care 
# from their General Practice as positive (“Excellent” or “Good”)
overall_care_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive") %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`GP Practice name`) %>%
  summarise(percentage_overall_care_GP = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  # For each GP practice, sum the percentages of the selected response options
  arrange(percentage_overall_care_GP) %>%
  # Order practices by % respondents positive, lowest to highest
  mutate(order = row_number())

#Scatterplot by GP practice
overall_care_GP_scatterplot <- make_scatter(
  data = overall_care_GP,
  x_var = order, 
  y_var = percentage_overall_care_GP, 
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by GP practice",
    width = 60
  ),
  x_lab = "GP practice",
  y_lab = "Percentage (%)"
)
overall_care_GP_scatterplot
# Save plot to working directory
save_plot_with_script_name(overall_care_GP_scatterplot)

# Boxplot by GP practice
overall_care_GP_boxplot <- make_boxplot_single_group(
  data = overall_care_GP,
  x_var = percentage_overall_care_GP,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by GP practice",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = ""
)
overall_care_GP_boxplot
# Save plot to working directory
save_plot_with_script_name(overall_care_GP_boxplot)

## Histogram ##
overall_care_GP_histogram <- make_histogram(
  data = overall_care_GP,
  x_var = percentage_overall_care_GP,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by GP practice",
    width = 60
  ),
  x_lab = "Percentage (%) of respondents who rated the overall care from their General Practice as positive",
  y_lab = "Number of GP practices")
overall_care_GP_histogram

# Saves plot to working directory
save_plot_with_script_name(overall_care_GP_histogram)

#------------------------------------------------------------------------------#

## By GP cluster 
overall_care_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive") %>%
  # Group the data by GP cluster so calculations are done per practice
  group_by(`Area`) %>%
  summarise(percentage_overall_care_cluster = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  # For each GP practice, sum the percentages of the selected response options
  arrange(percentage_overall_care_cluster) %>%
  # Order practices by % respondents positive, lowest to highest
  mutate(order = row_number())


# Boxplot by GP cluster
overall_care_cluster_boxplot <- make_boxplot_single_group(
  data = overall_care_cluster,
  x_var = percentage_overall_care_cluster,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by GP cluster",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = ""
)
overall_care_cluster_boxplot
# Save to working directory
save_plot_with_script_name(overall_care_cluster_boxplot)

## Histogram by cluster ##
overall_care_cluster_histogram <- make_histogram(
  data = overall_care_cluster,
  x_var = percentage_overall_care_cluster,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by GP cluster",
    width = 60
  ),
  x_lab = "Percentage (%) of respondents who rated the overall care from their General Practice as positive",
  y_lab = "Number of GP clusters")
overall_care_cluster_histogram
# Save plot to working directory
save_plot_with_script_name(overall_care_cluster_histogram)

#Scatterplot by GP cluster
overall_care_cluster_scatterplot <- make_scatter(
  data = overall_care_cluster,
  x_var = order, 
  y_var = percentage_overall_care_cluster, 
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by GP cluster",
    width = 60
  ),
  x_lab = "GP cluster",
  y_lab = "Percentage (%)"
)
overall_care_cluster_scatterplot
# Save plot to working directory
save_plot_with_script_name(overall_care_cluster_scatterplot)
