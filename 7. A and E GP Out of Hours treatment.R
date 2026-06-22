### Analysis for HACE Q24
# June 2026

## HSC vision portfolio indicators 
# A&E/GP Out of Hours – treatment

## What it measures 
#Percentage of people responding 'I was treated with compassion and understanding'
# during A&E or GP Out of Hours care.

## HACE Question (Q24)
# Q24 How much would you agree or disagree with the following statements about 
# your experience?


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

#Calculating the number of redacted responses at each geography level
`GP Practice` %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive") |> 
  summarise(
    na_n = sum(is.na(Percentage)),
    total_n = n(),
    na_pct = (na_n / total_n) * 100
  )
`GP Cluster` %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive") |> 
  summarise(
    na_n = sum(is.na(Percentage)),
    total_n = n(),
    na_pct = (na_n / total_n) * 100
  )
`Health Board` %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive") |> 
  summarise(
    na_n = sum(is.na(Percentage)),
    total_n = n(),
    na_pct = (na_n / total_n) * 100
  )

#------------------------------------------------------------------------------#
## By Health board
OOO_care_HB <- `Health Board` %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive") %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`Area`) %>%
  # For each GP practice, sum the percentages of the selected response options
  summarise(
    percentage_OOO_care_HB = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_OOO_care_HB) %>%
  # Order practices by % urgent respondants positive, lowest to highest
  mutate(order = row_number())

#Scatterplot by Health Board
OOO_care_HB_scatterplot <- make_scatter(
  data = OOO_care_HB,
  x_var = order, 
  y_var = percentage_OOO_care_HB,
  title = str_wrap(
    "The percentage of respondents responding positively to 'I was treated with compassion and understanding during A&E or GP Out of Hours care' by Health Board", 
    width = 60
  ), 
  x_lab = "Health board",
  y_lab = "Percentage (%) responding positively"
)
OOO_care_HB_scatterplot
# Saves plot to working directory
save_plot_with_script_name(OOO_care_HB_scatterplot)

#Boxplot by GP cluster
OOO_care_HB_boxplot <- make_boxplot_single_group(
  data = OOO_care_HB,
  x_var = percentage_OOO_care_HB,
  title = str_wrap(
    "The percentage of respondents responding positively to 'I was treated with compassion and understanding during A&E or GP Out of Hours care' by Health Board", 
    width = 60
  ),
  x_lab = "Percentage",
  y_lab = "")
OOO_care_HB_boxplot
# Saves plot to working directory
save_plot_with_script_name(OOO_care_HB_boxplot)

## Histogram by GP Cluster ##
OOO_care_HB_histogram <- make_histogram(
  data = OOO_care_HB, 
  x_var = percentage_OOO_care_HB, 
  title = str_wrap(
    "The percentage of respondents responding positively to 'I was treated with compassion and understanding during A&E or GP Out of Hours care' by Health Board", 
    width = 60
  ),
  x_lab = "Percentage (%) responding positively",
  y_lab = "Number of Health boards")
OOO_care_HB_histogram
# Save plot to working directory
save_plot_with_script_name(OOO_care_HB_histogram)

#------------------------------------------------------------------------------#
## By GP Cluster 
OOO_care_cluster <- `GP Cluster` %>%
  {na_n <- sum(is.na(.$Percentage))
    
    if (na_n > 0) {
      message(
      "#############################################
## NOTE: Redacted responses (NAs) removed ###
#############################################")
    }
    
    .
  } %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` == "positive",
    !is.na(Percentage)
  ) %>%
  group_by(`Area`) %>%
  summarise(
    percentage_OOO_care_cluster = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_OOO_care_cluster) %>%
  mutate(order = row_number())

#Scatterplot by GP cluster
OOO_care_cluster_scatterplot <- make_scatter(
  data = OOO_care_cluster,
  x_var = order, 
  y_var = percentage_OOO_care_cluster,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A and E or GP Out of Hours care.' by GP cluster", 
    width = 60
  ), 
  x_lab = "GP cluster",
  y_lab = "Percentage (%) responding positively"
)
OOO_care_cluster_scatterplot
# Saves plot to working directory
save_plot_with_script_name(OOO_care_cluster_scatterplot)

#Boxplot by GP cluster
OOO_care_cluster_boxplot <- make_boxplot_single_group(
  data = OOO_care_cluster,
  x_var = percentage_OOO_care_cluster,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A and E or GP Out of Hours care' by GP cluster", 
    width = 60
  ),
  x_lab = "Percentage",
  y_lab = "")
OOO_care_cluster_boxplot
# Saves plot to working directory
save_plot_with_script_name(OOO_care_cluster_boxplot)

## Histogram by GP Cluster ##
OOO_care_cluster_histogram <- make_histogram(
  data = OOO_care_cluster, 
  x_var = percentage_OOO_care_cluster, 
  title = str_wrap(
    "The percentage of respondents responding positively to 'I was treated with compassion and understanding' during A and E or GP Out of Hours care' by GP cluster", 
    width = 60
  ),
  x_lab = "Percentage (%) responding positively",
  y_lab = "Number of GP clusters")
OOO_care_cluster_histogram
# Save plot to working directory
save_plot_with_script_name(OOO_care_cluster_histogram)


#----- BY GP Practice - too many redacted answers to be insightful yet --------#
# Summary table showing the Percentage of people responding 'I was treated with 
# compassion and understanding' during A&E or GP Out of Hours care.
# OOO_care_GP <- `GP Practice` %>%
#   filter(
#     `Question Number` == "q24c",
#     `Response Option` == "positive") %>%
#   # Group the data by GP Practice so calculations are done per practice
#   group_by(`GP Practice name`) %>%
#   # For each GP practice, sum the percentages of the selected response options
#   summarise(
#     percentage_OOO_care_GP = sum(Percentage, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   arrange(percentage_OOO_care_GP) %>%
#   # Order practices by % respondents positive, lowest to highest
#   mutate(order = row_number())
# 
# #Scatteplot by GP practice
# OOO_care_GP_scatterplot <- make_scatter(
#   data = OOO_care_GP,
#   x_var = order, 
#   y_var = percentage_OOO_care_GP,
#   title = str_wrap(
#     "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A&E or GP Out of Hours care.' by GP practice", 
#     width = 60
#   ), 
#   x_lab = "GP practice",
#   y_lab = "Percentage (%) responding positively"
# )
# OOO_care_GP_scatterplot
# # Saves plot to working directory
# save_plot_with_script_name(OOO_care_GP_scatterplot)
# 
# # Box plot by GP practice
# OOO_care_GP_boxplot <- make_boxplot_single_group(
#   data = OOO_care_GP,
#   x_var = percentage_OOO_care_GP, 
#   title = str_wrap(
#     "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A&E or GP Out of Hours care.' by GP practice", 
#     width = 60
#   ),
#   x_lab = "Percentage (%)",
#   y_lab = "")
# OOO_care_GP_boxplot
# # Save plot to working directory
# save_plot_with_script_name(OOO_care_GP_boxplot)
# 
# ## Histogram by GP Practice ##
# OOO_care_GP_histogram <- make_histogram(
#   data = OOO_care_GP, 
#   x_var = percentage_OOO_care_GP, 
#   title = str_wrap(
#     "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A&E or GP Out of Hours care.' by GP practice", 
#     width = 60
#   ), 
#   x_lab = "Percentage (%) responding positively",
#   y_lab = "Number of GP Practices")
# OOO_care_GP_histogram
# # Save plot to working directory
# save_plot_with_script_name(OOO_care_GP_histogram)

#------------------------------------------------------------------------------#