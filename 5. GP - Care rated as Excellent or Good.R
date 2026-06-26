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
# For scotland
overall_care_scotland <- Scotland %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive") %>%
  summarise(
    percentage_overall_care_scotland = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
    ) %>%
  pull(percentage_overall_care_scotland)

# For individual GP practices
overall_care_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive") %>%
  group_by(`GP Practice name`) %>%
  summarise(
    percentage_overall_care_GP = sum(Percentage, na.rm = TRUE), 
    .groups = "drop"
    ) %>%
  arrange(percentage_overall_care_GP) %>%
  mutate(order = row_number())

# For GP Clusters
overall_care_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_overall_care_cluster = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_overall_care_cluster) %>%
  mutate(order = row_number())

# For Health and Social Care partnerships
overall_care_HSCP <- HSCP %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_overall_care_HSCP = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_overall_care_HSCP) %>%
  mutate(order = row_number())

# For Health Board
overall_care_HB <- `Health Board` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_overall_care_HB = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_overall_care_HB) %>%
  mutate(order = row_number())
################################################################################
## Barchart of urgent access national level ------------------------------------
bands <- paste0(seq(0, 90, 10), "-", seq(10, 100, 10))

scotland_band <- cut(
  overall_care_scotland,
  breaks = seq(0, 100, by = 10),
  labels = bands,
  include.lowest = TRUE,
  right = FALSE
)

overall_care_GP_binned <- overall_care_GP %>%
  mutate(
    pct_band = cut(
      percentage_overall_care_GP,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- overall_care_GP_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


overall_care_GP_barchart <- make_barchart_multiple_groups(
  data = overall_care_GP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
        "The percentage of respondents who rated the overall care from their General Practice as positive by GP practice",
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
    label = paste0("Scottish average ", round(overall_care_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )
overall_care_GP_barchart
save_plot_with_script_name(overall_care_GP_barchart)

# By Cluster
overall_care_cluster_binned <- overall_care_cluster %>%
  mutate(
    pct_band = cut(
      percentage_overall_care_cluster,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- overall_care_cluster_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


overall_care_cluster_barchart <- make_barchart_multiple_groups(
  data = overall_care_cluster_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by GP cluster",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Number of GP clusters")+
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
    label = paste0("Scottish average ", round(overall_care_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )
overall_care_cluster_barchart
save_plot_with_script_name(overall_care_cluster_barchart)

# By HSCP
overall_care_HSCP_binned <- overall_care_HSCP %>%
  mutate(
    pct_band = cut(
      percentage_overall_care_HSCP,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- overall_care_HSCP_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


overall_care_HSCP_barchart <- make_barchart_multiple_groups(
  data = overall_care_HSCP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by GP HSCP",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Number of GP HSCPs")+
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
    label = paste0("Scottish average ", round(overall_care_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )
overall_care_HSCP_barchart
save_plot_with_script_name(overall_care_HSCP_barchart)

################################################################################
##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
overall_care_scotland_by_sex <- Sex_joined %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`Sex`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_overall_care = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

overall_care_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_sex %>% 
    filter(Sex != "Scotland Total"),
  x_var = Sex,
  y_var = percentage_overall_care,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by sex",
    width = 60
  ), 
  x_lab = "Sex", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = overall_care_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = overall_care_scotland,
    label = paste0("Scottish average: ", round(overall_care_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
overall_care_scotland_by_sex_barchart
save_plot_with_script_name(overall_care_scotland_by_sex_barchart)

## Barchart of Scotland average answer by Age band #
overall_care_scotland_by_age <- Age_band_joined %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`Age Band`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_overall_care = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

overall_care_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_age %>% 
    filter(`Age Band` != "Scotland Total"),
  x_var = `Age Band`,
  y_var = percentage_overall_care,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by age",
    width = 60
  ), 
  x_lab = "Age", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = overall_care_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = 2,
    y = overall_care_scotland,
    label = paste0("Scottish average: ", round(overall_care_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
overall_care_scotland_by_age_barchart
save_plot_with_script_name(overall_care_scotland_by_age_barchart)

# Barchart of Scotland average answer by SIMD #
overall_care_scotland_by_SIMD <- SIMD_joined %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`Scottish Index of Multiple Deprivation Decile`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_overall_care = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

overall_care_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_SIMD %>% 
    filter(`Scottish Index of Multiple Deprivation Decile` != "Scotland Total"),
  x_var = reorder(
    `Scottish Index of Multiple Deprivation Decile`,
    as.numeric(sub("^([0-9]+).*", "\\1",
                   `Scottish Index of Multiple Deprivation Decile`))
  ),
  y_var = percentage_overall_care,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by SIMD",
    width = 60
  ), 
  x_lab = "SIMD", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = overall_care_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = 3,
    y = overall_care_scotland,
    label = paste0("Scottish average: ", round(overall_care_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
overall_care_scotland_by_SIMD_barchart
save_plot_with_script_name(overall_care_scotland_by_SIMD_barchart)

# Barchart of Scotland average answer by Urban 8 #
overall_care_scotland_by_urban <- Urban_Rural_8_joined %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
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
    `Response Option` = "positive",
    percentage_overall_care = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

overall_care_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_urban %>% 
    filter(`Urban-Rural 8-fold classification` != "Scotland Total"),
  x_var = `Urban-Rural 8-fold classification`,
  y_var = percentage_overall_care,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by Urban-Rural 8",
    width = 60
  ), 
  x_lab = "Urban-Rural 8-fold classification", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = overall_care_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = 3,
    y = overall_care_scotland,
    label = paste0("Scottish average: ", round(overall_care_scotland, 0), "%"),
    hjust = 1,
    vjust = -1.5, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
overall_care_scotland_by_urban_barchart
save_plot_with_script_name(overall_care_scotland_by_urban_barchart)

##----------------------------------------------------------------------------#
##Barchart by Chronic Pain ##
overall_care_scotland_by_chronic_pain <- `Chronic Pain` %>% 
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_overall_care = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


overall_care_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_chronic_pain %>% 
    mutate(`By Question Response Option` = factor(
      `By Question Response Option`,
      levels = c("Yes", "No", "Skipped Q42")
    )),
  x_var = `By Question Response Option`,
  y_var = percentage_overall_care,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by Chronic pain",
    width = 60
  ), 
  x_lab = "Do you suffer from chronic or persistent pain, that is pain that carries on for longer than 3 months despite medication or treatment?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = overall_care_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = overall_care_scotland,
    label = paste0("Scottish average: ", round(overall_care_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


overall_care_scotland_by_chronic_pain_barchart
save_plot_with_script_name(overall_care_scotland_by_chronic_pain_barchart)

##  Barchart by Long term condition ##
overall_care_scotland_by_long_term <- `Long-Term Condition` %>% 
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_overall_care = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


overall_care_scotland_by_long_term_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_long_term %>% 
    mutate(`By Question Response Option` = factor(
      `By Question Response Option`,
      levels = c("Yes", "No", "Skipped Question")
    )),
  x_var = `By Question Response Option`,
  y_var = percentage_overall_care,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by long term condition",
    width = 60
  ), 
  x_lab = "Do you have any physical or mental health conditions or illnesses lasting or expected to last 12 months or more?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = overall_care_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = overall_care_scotland,
    label = paste0("Scottish average: ", round(overall_care_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


overall_care_scotland_by_long_term_barchart
save_plot_with_script_name(overall_care_scotland_by_long_term_barchart)

## Barchart  by Sexual Orientation ##
overall_care_scotland_by_sexual_orientation <- `Sexual Orientation` %>% 
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_overall_care = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


overall_care_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_sexual_orientation %>% 
    mutate(`By Question Response Option` = 
             forcats::fct_reorder(`By Question Response Option`,
                                  percentage_overall_care,
                                  .desc = TRUE) %>%
             forcats::fct_relevel("Skipped Q43", after = Inf)
    ),
  x_var = `By Question Response Option`,
  y_var = percentage_overall_care,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by sexual orientation",
    width = 60
  ), 
  x_lab = "Which of the following best describes your sexual orientation?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = overall_care_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = overall_care_scotland,
    label = paste0("Scottish average: ", round(overall_care_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")

overall_care_scotland_by_sexual_orientation_barchart
save_plot_with_script_name(overall_care_scotland_by_sexual_orientation_barchart)

## # Barchart by Ethnicity ##
overall_care_scotland_by_ethnicity <- Ethnicity %>% 
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_overall_care = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

overall_care_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_ethnicity %>% 
    mutate(
      `By Question Response Option` = 
        forcats::fct_reorder(`By Question Response Option`,
                             percentage_overall_care,
                             .desc = TRUE) %>%
        forcats::fct_relevel("Skipped Q44", after = Inf)
    ),
  x_var = `By Question Response Option`,
  y_var = percentage_overall_care,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by ethnicity",
    width = 60
  ), 
  x_lab = "What is your ethnic group?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = overall_care_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = overall_care_scotland,
    label = paste0("Scottish average: ", round(overall_care_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")

overall_care_scotland_by_ethnicity_barchart
save_plot_with_script_name(overall_care_scotland_by_ethnicity_barchart)

##----------------------------------------------------------------------------#
################################################################################
# Exploratory plots
# #Scatterplot by GP practice
# overall_care_GP_scatterplot <- make_scatter(
#   data = overall_care_GP,
#   x_var = order, 
#   y_var = percentage_overall_care_GP, 
#   title = str_wrap(
#     "The percentage of respondents who rated the overall care from their General Practice as positive by GP practice",
#     width = 60
#   ),
#   x_lab = "GP practice",
#   y_lab = "Percentage (%)"
# )
# overall_care_GP_scatterplot
# # Save plot to working directory
# save_plot_with_script_name(overall_care_GP_scatterplot)
# 
# # Boxplot by GP practice
# overall_care_GP_boxplot <- make_boxplot_single_group(
#   data = overall_care_GP,
#   x_var = percentage_overall_care_GP,
#   title = str_wrap(
#     "The percentage of respondents who rated the overall care from their General Practice as positive by GP practice",
#     width = 60
#   ),
#   x_lab = "Percentage (%)",
#   y_lab = ""
# )
# overall_care_GP_boxplot
# # Save plot to working directory
# save_plot_with_script_name(overall_care_GP_boxplot)
# 
# ## Histogram ##
# overall_care_GP_histogram <- make_histogram(
#   data = overall_care_GP,
#   x_var = percentage_overall_care_GP,
#   title = str_wrap(
#     "The percentage of respondents who rated the overall care from their General Practice as positive by GP practice",
#     width = 60
#   ),
#   x_lab = "Percentage (%) of respondents who rated the overall care from their General Practice as positive",
#   y_lab = "Number of GP practices")
# overall_care_GP_histogram
# 
# # Saves plot to working directory
# save_plot_with_script_name(overall_care_GP_histogram)
# 
# #------------------------------------------------------------------------------#
# 
# # Boxplot by GP cluster
# overall_care_cluster_boxplot <- make_boxplot_single_group(
#   data = overall_care_cluster,
#   x_var = percentage_overall_care_cluster,
#   title = str_wrap(
#     "The percentage of respondents who rated the overall care from their General Practice as positive by GP cluster",
#     width = 60
#   ),
#   x_lab = "Percentage (%)",
#   y_lab = ""
# )
# overall_care_cluster_boxplot
# # Save to working directory
# save_plot_with_script_name(overall_care_cluster_boxplot)
# 
# ## Histogram by cluster ##
# overall_care_cluster_histogram <- make_histogram(
#   data = overall_care_cluster,
#   x_var = percentage_overall_care_cluster,
#   title = str_wrap(
#     "The percentage of respondents who rated the overall care from their General Practice as positive by GP cluster",
#     width = 60
#   ),
#   x_lab = "Percentage (%) of respondents who rated the overall care from their General Practice as positive",
#   y_lab = "Number of GP clusters")
# overall_care_cluster_histogram
# # Save plot to working directory
# save_plot_with_script_name(overall_care_cluster_histogram)
# 
# #Scatterplot by GP cluster
# overall_care_cluster_scatterplot <- make_scatter(
#   data = overall_care_cluster,
#   x_var = order, 
#   y_var = percentage_overall_care_cluster, 
#   title = str_wrap(
#     "The percentage of respondents who rated the overall care from their General Practice as positive by GP cluster",
#     width = 60
#   ),
#   x_lab = "GP cluster",
#   y_lab = "Percentage (%)"
# )
# overall_care_cluster_scatterplot
# # Save plot to working directory
# save_plot_with_script_name(overall_care_cluster_scatterplot)
