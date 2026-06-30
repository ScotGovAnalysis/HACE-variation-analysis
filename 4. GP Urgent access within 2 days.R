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

# Summary table showing the percentage of respondents who saw or spoke to a doctor
# or nurse within 2 working days
within_2_days_scotland <- Scotland %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>% 
  summarise(
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pull(percentage_within_2_days)

within_2_days_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(`GP Practice name`) %>%
  summarise(
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_within_2_days) %>%
  mutate(order = row_number())

within_2_days_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% c(
      "I saw or spoke to a doctor or nurse on the same day",
      "I saw or spoke to a doctor or nurse within 1 or 2 working days"
    )
  ) %>%
  group_by(`Area`) %>%
  summarise(
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_within_2_days) %>%
  mutate(order = row_number())

within_2_days_HSCP <- `HSCP` %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(`Area`) %>%
  summarise(
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_within_2_days) %>%
  mutate(order = row_number())

within_2_days_HB <- `Health Board` %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(`Area`) %>%
  summarise(
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(percentage_within_2_days) %>%
  mutate(order = row_number())


#------------------------------------------------------------------------------#

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

## Barchart of urgent access national level ------------------------------------

bands <- paste0(seq(0, 90, 10), "-", seq(10, 100, 10))

scotland_band <- cut(
  within_2_days_scotland,
  breaks = seq(0, 100, by = 10),
  labels = bands,
  include.lowest = TRUE,
  right = FALSE
)

within_2_days_GP_binned <- within_2_days_GP %>%
  mutate(
    pct_band = cut(
      percentage_within_2_days,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- within_2_days_GP_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)

within_2_days_GP_barchart <- make_barchart_multiple_groups(
  data = within_2_days_GP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by GP practice",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Number of GP practices")+
  geom_point(
    data = data.frame(
      pct_band = scotland_band,
      n_practices = 1
    ),
    aes(x = pct_band, y = scotland_y),
    colour = "red",
    size = 4
  )+
  annotate(
    "text",
    x = scotland_band,
    y = scotland_y,
    label = paste0("Scottish average ", round(within_2_days_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )
within_2_days_GP_barchart

# Saves plot to working directory
save_plot_with_script_name(within_2_days_GP_barchart)

## Barchart of urgent access cluster level ------------------------------------
within_2_days_cluster_binned <- within_2_days_cluster %>%
  mutate(
    pct_band = cut(
      percentage_within_2_days,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- within_2_days_cluster_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


within_2_days_cluster_barchart <- make_barchart_multiple_groups(
  data = within_2_days_cluster_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by GP cluster",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Number of GP practices")+
  geom_point(
    data = data.frame(
      pct_band = scotland_band,
      n_practices = 1
    ),
    aes(x = pct_band, y = scotland_y),
    colour = "red",
    size = 4
  )+
  annotate(
    "text",
    x = scotland_band,
    y = scotland_y,
    label = paste0("Scottish average ", round(within_2_days_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )

within_2_days_cluster_barchart

# Saves plot to working directory
save_plot_with_script_name(within_2_days_cluster_barchart)

## Barchart of urgent access HSCP level ------------------------------------
within_2_days_HSCP_binned <- within_2_days_HSCP %>%
  mutate(
    pct_band = cut(
      percentage_within_2_days,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- within_2_days_HSCP_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


within_2_days_HSCP_barchart <- make_barchart_multiple_groups(
  data = within_2_days_HSCP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by HSCP",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Number of GP practices")+
  geom_point(
    data = data.frame(
      pct_band = scotland_band,
      n_practices = 1
    ),
    aes(x = pct_band, y = scotland_y),
    colour = "red",
    size = 4
  )+
  annotate(
    "text",
    x = scotland_band,
    y = scotland_y,
    label = paste0("Scottish average ", round(within_2_days_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )

within_2_days_HSCP_barchart
# Saves plot to working directory
save_plot_with_script_name(within_2_days_HSCP_barchart)

## Barchart of urgent access Health board level ------------------------------------
within_2_days_HB_binned <- within_2_days_HB %>%
  mutate(
    pct_band = cut(
      percentage_within_2_days,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- within_2_days_HB_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


within_2_days_HB_barchart <- make_barchart_multiple_groups(
  data = within_2_days_HB_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by Health Board",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Number of GP practices")+
  geom_point(
    data = data.frame(
      pct_band = scotland_band,
      n_practices = 1
    ),
    aes(x = pct_band, y = scotland_y),
    colour = "red",
    size = 4
  )+
  annotate(
    "text",
    x = scotland_band,
    y = scotland_y,
    label = paste0("Scottish average ", round(within_2_days_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )

within_2_days_HB_barchart
# Saves plot to working directory
save_plot_with_script_name(within_2_days_HB_barchart)

#------------------------------------------------------------------------------#
## By GP Cluster 
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

within_2_days_scotland_by_sex <- Sex_joined %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
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

within_2_days_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_sex %>% 
    filter(Sex != "Scotland Total"),
  x_var = Sex,
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by Sex",
    width = 60
  ), 
  x_lab = "Sex", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = within_2_days_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = within_2_days_scotland,
    label = paste0("Scottish average: ", round(within_2_days_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
within_2_days_scotland_by_sex_barchart
save_plot_with_script_name(within_2_days_scotland_by_sex_barchart)



##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by Age band #
within_2_days_scotland_by_age <- Age_band_joined %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses # Detailed in the utility script
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

within_2_days_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_age %>% 
    filter(`Age Band` != "Scotland Total"),
  x_var = `Age Band`,
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by age band",
    width = 60
  ), 
  x_lab = "Age Band", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = within_2_days_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = within_2_days_scotland,
    label = paste0("Scottish average: ", round(within_2_days_scotland, 1), "%"),
    hjust = 1,
    vjust = -3, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


within_2_days_scotland_by_age_barchart
save_plot_with_script_name(within_2_days_scotland_by_age_barchart)

##----------------------------------------------------------------------------#
## Barchart of Access within two days by SIMD ##
within_2_days_scotland_by_SIMD <-  SIMD_joined %>% 
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(`Scottish Index of Multiple Deprivation Decile`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

within_2_days_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_SIMD %>% 
    filter(`Scottish Index of Multiple Deprivation Decile`!= "Scotland Total"),
  x_var = reorder(
      `Scottish Index of Multiple Deprivation Decile`,
      as.numeric(sub("^([0-9]+).*", "\\1",
                     `Scottish Index of Multiple Deprivation Decile`))
  ),
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by SIMD decile",
    width = 60
  ), 
  x_lab = "SIMD", 
  y_lab = "Percentage (%)"
  )+
  geom_hline(
    yintercept = within_2_days_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = within_2_days_scotland,
    label = paste0("Scottish average: ", round(within_2_days_scotland, 1), "%"),
    hjust = 1,
    vjust = -1,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


within_2_days_scotland_by_SIMD_barchart
save_plot_with_script_name(within_2_days_scotland_by_SIMD_barchart)

##----------------------------------------------------------------------------#
## Barchart of Access within two days by Urban 8 ##
within_2_days_scotland_by_urban <- Urban_Rural_8_joined %>% 
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(`Urban-Rural 8-fold classification`) %>%
  mutate(
    `Urban-Rural 8-fold classification` =
      ifelse(
        grepl("^[2-7] ", `Urban-Rural 8-fold classification`),
        sub("^([2-7]).*", "\\1", `Urban-Rural 8-fold classification`),
        `Urban-Rural 8-fold classification`
      )) %>% 
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

within_2_days_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_urban %>% 
    filter(`Urban-Rural 8-fold classification`!= "Scotland Total"),
  x_var = `Urban-Rural 8-fold classification`,
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by Urban-Rural 8-fold classification",
    width = 60
  ), 
  x_lab = "Urban-Rural 8-fold classification", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = within_2_days_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = within_2_days_scotland,
    label = paste0("Scottish average: ", round(within_2_days_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


within_2_days_scotland_by_urban_barchart
save_plot_with_script_name(within_2_days_scotland_by_urban_barchart)

##----------------------------------------------------------------------------#
##Barchart of Access within two days by Chronic Pain ##
within_2_days_scotland_by_chronic_pain <- `Chronic Pain` %>% 
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


within_2_days_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_chronic_pain %>% 
    mutate(
      `By Question Response Option` = factor(
      `By Question Response Option`,
      levels = c("Yes", "No", "Skipped Q42")
    )),
  x_var = `By Question Response Option`,
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by Chronic pain",
    width = 60
  ), 
  x_lab = "Do you suffer from chronic or persistent pain, that is pain that carries on for longer than 3 months despite medication or treatment?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = within_2_days_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = within_2_days_scotland,
    label = paste0("Scottish average: ", round(within_2_days_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


within_2_days_scotland_by_chronic_pain_barchart
save_plot_with_script_name(within_2_days_scotland_by_chronic_pain_barchart)

## Access within two days by Long term condition ##
# Barchart #
within_2_days_scotland_by_long_term <- `Long-Term Condition` %>% 
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


within_2_days_scotland_by_long_term_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_long_term %>% 
    mutate(
      `By Question Response Option` = factor(
      `By Question Response Option`,
      levels = c("Yes", "No", "Skipped Question")
    )),
  x_var = `By Question Response Option`,
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by long term condition",
    width = 60
  ), 
  x_lab = "Do you have any physical or mental health conditions or illnesses lasting or expected to last 12 months or more?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = within_2_days_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = within_2_days_scotland,
    label = paste0("Scottish average: ", round(within_2_days_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


within_2_days_scotland_by_long_term_barchart
save_plot_with_script_name(within_2_days_scotland_by_long_term_barchart)

## Access within two days by Sexual Orientation ##
# Barchart #
within_2_days_scotland_by_sexual_orientation <- `Sexual Orientation` %>% 
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


within_2_days_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_sexual_orientation %>% 
    mutate(
      `By Question Response Option` = 
       forcats::fct_reorder(
         `By Question Response Option`,
          percentage_within_2_days,
          .desc = TRUE) %>%
       forcats::fct_relevel(
         "Skipped Q43", 
         after = Inf)
    ),
  x_var = `By Question Response Option`,
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by sexual orientation",
    width = 60
  ), 
  x_lab = "Which of the following best describes your sexual orientation?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = within_2_days_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = within_2_days_scotland,
    label = paste0("Scottish average: ", round(within_2_days_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")

within_2_days_scotland_by_sexual_orientation_barchart
save_plot_with_script_name(within_2_days_scotland_by_sexual_orientation_barchart)

## Access within two days by Ethnicity ##
# Barchart #
within_2_days_scotland_by_ethnicity <- Ethnicity %>% 
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "Had urgent access within 2 days",
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

within_2_days_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = within_2_days_scotland_by_ethnicity %>% 
    mutate(
      `By Question Response Option` = 
             forcats::fct_reorder(`By Question Response Option`,
                                  percentage_within_2_days,
                                  .desc = TRUE) %>%
             forcats::fct_relevel("Skipped Q44", after = Inf)
    ),
  x_var = `By Question Response Option`,
  y_var = percentage_within_2_days,
  title = str_wrap(
    "The percentage of respondents needing urgent care seen within 2 working days by ethnicity",
    width = 60
  ), 
  x_lab = "What is your ethnic group?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = within_2_days_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = within_2_days_scotland,
    label = paste0("Scottish average: ", round(within_2_days_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")

within_2_days_scotland_by_ethnicity_barchart
save_plot_with_script_name(within_2_days_scotland_by_ethnicity_barchart)

###############################################################################
## Comparing to the last surveys results at Scotland level ##
## Cleaning 2021 results
within_2_days_scotland_2021 <- `Scotland - PNN Questions` %>% 
  filter(
    `Question Number` == "5"
  )%>%
  select(-c("Questionnaire Section", "Scotland"))%>% 
  pivot_longer(
    cols = starts_with("%"),
    names_to = "Response Option",
    values_to = "percentage_within_2_days"
  ) %>%
  mutate(
    `Response Option` = gsub("% ", "", `Response Option`),
    `Response Option` = tolower(`Response Option`),
    "Year"= "2021",
    percentage_within_2_days = as.numeric(as.character(percentage_within_2_days))
  ) %>% 
  filter(
    `Response Option` == "positive"
  )%>%
  select(c("percentage_within_2_days","Year"))

## Cleaning 2023 results
within_2_days_scotland_2023 <- `Information Questions` %>%
  filter(
    `Geography Type` == "Scotland",
    `Question Number` == "q10",
    `Response Option Text` %in% within_2_days_responses
  ) %>%
  mutate(
    `Percentage selecting this response option` =
      as.numeric(as.character(`Percentage selecting this response option`))
  ) %>%
  summarise(
    percentage_within_2_days = sum(`Percentage selecting this response option`, na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(
        percentage_within_2_days = percentage_within_2_days*100,
        "Year"="2023"
      )

within_2_days_scotland_2025 <- Scotland %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>% 
  summarise(
    percentage_within_2_days = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    "Year"="2025"
  )

within_2_days_scotland_timeseries <- bind_rows(
  within_2_days_scotland_2021,
  within_2_days_scotland_2023,
  within_2_days_scotland_2025,
  # 2019 & 2017 row
  tibble(
    percentage_within_2_days = c(
      66, #2019
      66), # 2017
    `Year` = c("2019","2017")
     )
   )



glimpse(within_2_days_scotland_timeseries)

within_2_days_scotland_timeseries_barchart <- ggplot(
  within_2_days_scotland_timeseries,
  aes(x = percentage_within_2_days,
      y = Year)) +
  geom_col(width = 0.6) +
  labs(
    title = "Timeseries of the percentage of respondents needing urgent care seen within 2 working days",
    x = "Percentage (%)",
    y = "Year",
    caption = "Note: 2021 was asked as a PNN question - this shows % responding positively, not necessarily within 2 days."
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1))+
  theme_minimal()+
  geom_text(
    aes(label = paste0(round(percentage_within_2_days, 0), "%")),
    hjust = 2,
    size = 3,
    colour = "white"
  )

within_2_days_scotland_timeseries_barchart
save_plot_with_script_name(within_2_days_scotland_timeseries_barchart)

##----------------------------------------------------------------------------#
#------------------## Within 2 days variation analysis ##---------------------#
within_2_days_variation_by_GP <- variation_data_2025 %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(hscp_name, `GP Practice name`) %>%
  summarise(
    within_2_days_percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

within_2_days_variation_by_hscp <- within_2_days_variation_by_GP %>%
  group_by(hscp_name) %>%
  summarise(
    num_practices = n(),
    sd_pct = sd(within_2_days_percentage, na.rm = TRUE),
    min_pct = min(within_2_days_percentage, na.rm = TRUE),
    max_pct = max(within_2_days_percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))


within_2_days_variation_tails <- bind_rows(
  Top5 = within_2_days_variation_by_hscp %>% slice_head(n = 5),
  Bottom5 = within_2_days_variation_by_hscp %>% slice_tail(n = 5),
  .id = "Group"
  ) %>%
  pull(hscp_name)


within_2_days_variation_tails_by_hscp_plot <- ggplot(
  within_2_days_variation_by_GP %>%
    filter(hscp_name %in% within_2_days_variation_tails),
  aes(x = reorder(hscp_name, within_2_days_percentage),
      y = within_2_days_percentage)
) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  coord_flip() +
  labs(
    title = "Urgent access: Top 5 and Bottom 5 HSCPs by variation",
    x = "HSCP",
    y = "% within 2 days"
  ) +
  theme_minimal()
within_2_days_variation_tails_by_hscp_plot
save_plot_with_script_name(within_2_days_variation_tails_by_hscp_plot)

chosen_within_2_days_variation_by_hscp <- within_2_days_variation_by_GP %>%
  filter(
    hscp_name %in% c(
      "Glasgow City","North Lanarkshire","West Dunbartonshire","Scottish Borders")
  )



chosen_within_2_days_variation_by_hscp_plot <- ggplot(
  chosen_within_2_days_variation_by_hscp, 
  aes(x = hscp_name, y = within_2_days_percentage)) +
  geom_jitter(
    aes(colour = hscp_name),
    width = 0.4, height = 0,
    size = 3, alpha = 0.75
  ) +
  scale_y_continuous(limits = c(0,100))+
  labs(
    title = "Urgent access by HSCP",
    x = "HSCP",
    y = "% seen within 2 days"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0)
  )
chosen_within_2_days_variation_by_hscp_plot
save_plot_with_script_name(chosen_within_2_days_variation_by_hscp_plot)
