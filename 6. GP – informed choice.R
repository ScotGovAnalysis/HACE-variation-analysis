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


# Summary table showing the Percentage of people responding positively to, 'I felt able to 
# make an informed choice about my treatment and care
informed_choice_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` == "positive") %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`GP Practice name`) %>%
  # For each GP practice, sum the percentages of the selected response options
  arrange(Percentage) %>%
  # Order practices by % respondents positive, lowest to highest
  mutate(order = row_number())

# Box plot by GP practice
ggplot(informed_choice_GP, aes(x = Percentage)) +
  geom_boxplot() +
  scale_x_continuous(limits = c(0, 100)) +
  labs(
    title = "Distribution of the % of people responding positively to, 'I felt able to  make an informed choice about my treatment and care",
    x = "Percentage (%)",
    y = ""
  )


## Histogram by GP Practice ##
ggplot(informed_choice_GP, aes(x = Percentage)) +
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
    title = "Histogram of the percentage (%) of people responding positively to, 'I felt able to  make an informed choice about my treatment and care",
    y = "Number of GP Practices"
  )+
  theme(axis.title.y = element_text(angle = 90))

#------------------------------------------------------------------------------#

## By GP Cluster 

informed_choice_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive") %>%
  # Group the data by GP Practice so calculations are done per practice
  group_by(`Area`) %>%
  # For each GP practice, sum the percentages of the selected response options
  arrange(Percentage) %>%
  # Order practices by % urgent respondants positive, lowest to highest
  mutate(order = row_number())

#Boxplot by GP cluster
ggplot(informed_choice_cluster,
       aes(x = Area, y = Percentage)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Variation in percentage (%) of people responding positively to, 'I felt able to  make an informed choice about my treatment and care",
    x = "GP Cluster",
    y = "Percentage (%)"
  )
