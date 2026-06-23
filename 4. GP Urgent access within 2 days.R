### Analysis for HACE Q10 
# June 2026

## HSC vision portfolio indicators 
# General Practice – urgent access within 2 days

## What it measures 
# Percentage of people who needed to see or speak to a doctor or nurse from their 
# General Practice quite urgently and were able to do so within 2 days.

## HACE Question (Q10)
# The last time you needed to see or speak to a doctor or a nurse from your GP
# Practice quite urgently, how long did you wait? ##


## Core value
# Access – Care at the right time and in the right place#

#------------------------------------------------------------------------------#

# Set SGplot for default chart colours
sgplot::use_sgplot()

#Source function to save plots from utility script
source("1. Utility.R")
#Load clean data from rds scripts
data_list_demographics <- readRDS("Clean data/data_list_demographics_clean.rds")
data_list_geographies <- readRDS("Clean data/data_list_geographies_clean.rds")


# Summary table showing the percentage of respondents who saw or spoke to a doctor
# or nurse within 2 working days
within_2_days_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% c(
      "I saw or spoke to a doctor or nurse on the same day",
      "I saw or spoke to a doctor or nurse within 1 or 2 working days"
    )
  ) %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`GP Practice name`) %>%
  # For each GP practice, sum the percentages of the selected response options
  summarise(
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_within_2_days) %>%
  # Order practices by % urgent respondents seen within 2 days, lowest to highest
  mutate(order = row_number())

#Scatterplot
within_2_days_GP_scatterplot <- make_scatter(
  data = within_2_days_GP, 
  x_var = order, 
  y_var = percentage_within_2_days, 
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by GP practice",
    width = 60
  ),
  x_lab = "GP practice (ordered from lowest to highest)",
  y_lab = "Percentage (%)")
within_2_days_GP_scatterplot
save_plot_with_script_name(within_2_days_GP_scatterplot)

#Boxplot
within_2_days_GP_boxplot <- make_boxplot_single_group(
    data = within_2_days_GP, 
    x_var = percentage_within_2_days, 
    title= str_wrap(
      "The percentage of respondents needing urgent care seen within 2 working days by GP practice",
     width = 60
    ),
    x_lab = "Percentage (%) of respondents needing urgent care seen within 2 working days",
    y_lab = "")
within_2_days_GP_boxplot
save_plot_with_script_name(within_2_days_GP_boxplot)

## Histogram using function on utility page ##
within_2_days_GP_histogram <- make_histogram(
  data = within_2_days_GP, 
  x_var = percentage_within_2_days, 
  title = str_wrap(
   "The percentage of respondents needing urgent care seen within 2 working days by GP practice",
   width = 60
  ),
  x_lab = "Percentage (%) of respondents needing urgent care seen within 2 working days",
  y_lab = "Number of GP Practices")
within_2_days_GP_histogram

# Saves plot to working directory
save_plot_with_script_name(within_2_days_GP_histogram)

#------------------------------------------------------------------------------#
## By GP Cluster 

within_2_days_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% c(
      "I saw or spoke to a doctor or nurse on the same day",
      "I saw or spoke to a doctor or nurse within 1 or 2 working days"
    )
  ) %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`Area`) %>%
  # For each GP practice, sum the percentages of the selected response options
  summarise(
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_within_2_days) %>%
  # Order practices by % urgent respondents seen within 2 days, lowest to highest
  mutate(order = row_number())


within_2_days_cluster_scatterplot <- make_scatter(
   data = within_2_days_cluster, 
   x_var = order, 
   y_var = percentage_within_2_days, 
   title = str_wrap(
     "The percentage of respondents needing urgent care seen within 2 working days by GP cluster",
     width = 60
   ),
   x_lab = "GP Clusters (ordered from lowest to highest)",
   y_lab = "Percentage (%)")
within_2_days_cluster_scatterplot
save_plot_with_script_name(within_2_days_cluster_scatterplot)

#Boxplot
within_2_days_cluster_boxplot <- make_boxplot_single_group(
  data = within_2_days_cluster, 
  x_var = percentage_within_2_days, 
  title= str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by GP cluster",
    width = 60
  ),
  x_lab = "Percentage (%) of respondents needing urgent care seen within 2 working days",
  y_lab = "")
within_2_days_cluster_boxplot
save_plot_with_script_name(within_2_days_cluster_boxplot)

## Histogram using function on utility page ##
within_2_days_cluster_histogram <- make_histogram(
  data = within_2_days_cluster, 
  x_var = percentage_within_2_days, 
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by GP cluster",
    width = 60
  ),
  x_lab = "Percentage (%) of respondents needing urgent care seen within 2 working days",
  y_lab = "Number of GP Clusters")

within_2_days_cluster_histogram

save_plot_with_script_name(within_2_days_cluster_histogram)

##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
within_2_days_responses <- c(
  "I saw or spoke to a doctor or nurse on the same day",
  "I saw or spoke to a doctor or nurse within 1 or 2 working days"
)

within_2_days_by_sex <- Sex %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% c(
      "I saw or spoke to a doctor or nurse on the same day",
      "I saw or spoke to a doctor or nurse within 1 or 2 working days"
    )
  ) %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`Sex`) %>%
  # For each GP practice, sum the percentages of the selected response options
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


within_2_days_scotland <- Scotland |> 
  filter(
    `Question Number` == "q10",
    `Response Option` %in% c(
      "I saw or spoke to a doctor or nurse on the same day",
      "I saw or spoke to a doctor or nurse within 1 or 2 working days"
    )
  ) |> 
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(Sex = "Scotland Total")
  

within_2_days_scotland_by_sex <- bind_rows(
  within_2_days_by_sex %>%
    select(`Question Number`, `Question Text`, `Response Option`, Sex, percentage_within_2_days),
  within_2_days_scotland %>%
    select(`Question Number`, `Question Text`, `Response Option`, Sex, percentage_within_2_days)
)

within_2_days_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_sex,
  x_var = Sex,
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by Sex",
    width = 60
  ), 
  x_lab = "Sex", 
  y_lab = "Percentage (%)"
)
within_2_days_scotland_by_sex_barchart
save_plot_with_script_name(within_2_days_scotland_by_sex_barchart)

##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by Age band #
within_2_days_responses <- c(
  "I saw or spoke to a doctor or nurse on the same day",
  "I saw or spoke to a doctor or nurse within 1 or 2 working days"
)

within_2_days_by_Age <- `Age Band` %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`Age Band`) %>%
  # For each GP practice, sum the percentages of the selected response options
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


within_2_days_scotland <- Scotland |> 
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) |> 
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(`Age Band` = "Scotland Total")


within_2_days_scotland_by_age <- bind_rows(
  within_2_days_by_Age %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Age Band`, percentage_within_2_days),
  within_2_days_scotland %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Age Band`, percentage_within_2_days)
) %>%
  filter(`Age Band` != "Scotland Total")

scotland_total <- within_2_days_scotland$percentage_within_2_days

within_2_days_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_age,
  x_var = `Age Band`,
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by age band",
    width = 60
  ), 
  x_lab = "Age", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = scotland_total,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = scotland_total,
    label = paste0("Scottish average: ", round(scotland_total, 1), "%"),
    hjust = 1,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


within_2_days_scotland_by_age_barchart
save_plot_with_script_name(within_2_days_scotland_by_age_barchart)
