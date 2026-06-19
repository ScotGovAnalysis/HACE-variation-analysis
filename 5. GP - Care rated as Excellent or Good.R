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


# Summary table showing the percentage of respondents who rated the overall care 
# from their General Practice as positive (“Excellent” or “Good”)
overall_care_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive") %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`GP Practice name`) %>%
  summarise(Percentage = sum(Percentage, na.rm = TRUE), .groups = "drop") %>%
  # For each GP practice, sum the percentages of the selected response options
  arrange(Percentage) %>%
  # Order practices by % respondents positive, lowest to highest
  mutate(order = row_number())

# Boxplot
overall_care_GP_boxplot <- ggplot(overall_care_GP, aes(x = Percentage)) +
  geom_boxplot() +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    title = str_wrap("The percentage of respondents who rated the overall care from their General Practice as positive by GP practice",
                     width = 60
                     ),
    x = "Percentage (%)",
    y = ""
  )
overall_care_GP_boxplot
save_plot_with_script_name(overall_care_GP_boxplot)

## Histogram ##
overall_care_GP_histogram <- ggplot(overall_care_GP, aes(x = Percentage)) +
  geom_histogram(
    binwidth = 3,
    boundary = 0,
    colour = "white",   #outlines for bins
    linewidth = 0.4
  ) +
  geom_density(
    alpha = 0.2
  ) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    title = str_wrap(
      "The percentage of respondents who rated the overall care from their General Practice as positive by GP practice",
      width = 60
    ),
    x = "Percentage (%) of respondents who rated the overall care from their General Practice as positive",
    y = "Number of GP Practices"
  )+
  theme(axis.title.y = element_text(angle = 90), #rotating Y axis title
        plot.title = element_text(hjust = 0.5) # centering the main title
  )

overall_care_GP_histogram

# Saves plot to working directory
save_plot_with_script_name(overall_care_GP_histogram)

overall_care_GP_scattlerplot <- ggplot(overall_care_GP,
  aes(x = order, y = Percentage)) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = str_wrap(
      "The percentage of respondents needing urgent care seen within 2 working days by GP Practice",
      width = 60
    ),
    x = "GP Practice (ordered from lowest to highest)",
    y = "Percentage (%)"
  )+
  theme(axis.title.y = element_text(angle = 90), #rotating Y axis title
        plot.title = element_text(hjust = 0.5) # centering the main title
  )
overall_care_GP_scattlerplot
save_plot_with_script_name(overall_care_GP_scattlerplot)

#------------------------------------------------------------------------------#
## By GP Cluster 

overall_care_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` == "positive"
  ) %>%
  arrange(Percentage) %>%
  mutate(order = row_number())

# Boxplot
overall_care_cluster_boxplot <- ggplot(overall_care_cluster, aes(x = Percentage)) +
  geom_boxplot() +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    title = str_wrap("The percentage of respondents who rated the overall care from their General Practice as positive by GP cluster",
                     width = 60
    ),
    x = "Percentage (%)",
    y = ""
  )
overall_care_cluster_boxplot
save_plot_with_script_name(overall_care_cluster_boxplot)

## Histogram ##
overall_care_cluster_histogram <- ggplot(overall_care_cluster, aes(x = Percentage)) +
  geom_histogram(
    binwidth = 3,
    boundary = 0,
    colour = "white",   #outlines for bins
    linewidth = 0.4
  ) +
  geom_density(
    alpha = 0.2
  ) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    title = str_wrap(
      "The percentage of respondents who rated the overall care from their General Practice as positive by GP cluster",
      width = 60
    ),
    x = "Percentage (%) of respondents who rated the overall care from their General Practice as positive",
    y = "Number of GP Practices"
  )+
  theme(axis.title.y = element_text(angle = 90), #rotating Y axis title
        plot.title = element_text(hjust = 0.5) # centering the main title
  )

overall_care_cluster_histogram

# Saves plot to working directory
save_plot_with_script_name(overall_care_cluster_histogram)

overall_care_cluster_scattlerplot <- ggplot(overall_care_cluster,
       aes(x = order, y = Percentage)) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = str_wrap(
      "The percentage of respondents needing urgent care seen within 2 working days by GP cluster",
      width = 60
    ),
    x = "GP Clusters (ordered from lowest to highest)",
    y = "Percentage (%)"
  )+
  theme(axis.title.y = element_text(angle = 90), #rotating Y axis title
        plot.title = element_text(hjust = 0.5) # centering the main title
  )
overall_care_cluster_scattlerplot
save_plot_with_script_name(overall_care_cluster_scattlerplot)
