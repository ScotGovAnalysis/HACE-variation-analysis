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

informed_choice_scotland <- Scotland %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive") %>%
  summarise(
    percentage_informed_choice_scotland = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pull(percentage_informed_choice_scotland)

# For individual GP practices
informed_choice_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive") %>%
  group_by(`GP Practice name`) %>%
  summarise(
    percentage_informed_choice_GP = sum(Percentage, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(percentage_informed_choice_GP) %>%
  mutate(order = row_number())

# For GP Clusters
informed_choice_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_informed_choice_cluster = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_informed_choice_cluster) %>%
  mutate(order = row_number())

# For Health and Social Care partnerships
informed_choice_HSCP <- HSCP %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_informed_choice_HSCP = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_informed_choice_HSCP) %>%
  mutate(order = row_number())

# For Health Board
informed_choice_HB <- `Health Board` %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_informed_choice_HB = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_informed_choice_HB) %>%
  mutate(order = row_number())
################################################################################
## Barchart of urgent access national level ------------------------------------
bands <- paste0(seq(0, 90, 10), "-", seq(10, 100, 10))

scotland_band <- cut(
  informed_choice_scotland,
  breaks = seq(0, 100, by = 10),
  labels = bands,
  include.lowest = TRUE,
  right = FALSE
)

informed_choice_GP_binned <- informed_choice_GP %>%
  mutate(
    pct_band = cut(
      percentage_informed_choice_GP,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- informed_choice_GP_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


informed_choice_GP_barchart <- make_barchart_multiple_groups(
  data = informed_choice_GP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by GP practice",
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
    label = paste0("Scottish average ", round(informed_choice_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )
informed_choice_GP_barchart
save_plot_with_script_name(informed_choice_GP_barchart)

# By Cluster
informed_choice_cluster_binned <- informed_choice_cluster %>%
  mutate(
    pct_band = cut(
      percentage_informed_choice_cluster,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- informed_choice_cluster_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


informed_choice_cluster_barchart <- make_barchart_multiple_groups(
  data = informed_choice_cluster_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by GP cluster",
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
    label = paste0("Scottish average ", round(informed_choice_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )
informed_choice_cluster_barchart
save_plot_with_script_name(informed_choice_cluster_barchart)

# By HSCP
informed_choice_HSCP_binned <- informed_choice_HSCP %>%
  mutate(
    pct_band = cut(
      percentage_informed_choice_HSCP,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- informed_choice_HSCP_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


informed_choice_HSCP_barchart <- make_barchart_multiple_groups(
  data = informed_choice_HSCP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by GP HSCP",
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
    label = paste0("Scottish average ", round(informed_choice_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )
informed_choice_HSCP_barchart
save_plot_with_script_name(informed_choice_HSCP_barchart)

################################################################################
##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
informed_choice_scotland_by_sex <- Sex_joined %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`Sex`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_informed_choice = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

informed_choice_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_sex %>% 
    filter(Sex != "Scotland Total"),
  x_var = Sex,
  y_var = percentage_informed_choice,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by sex",
    width = 60
  ), 
  x_lab = "Sex", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = informed_choice_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = informed_choice_scotland,
    label = paste0("Scottish average: ", round(informed_choice_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
informed_choice_scotland_by_sex_barchart
save_plot_with_script_name(informed_choice_scotland_by_sex_barchart)

## Barchart of Scotland average answer by Age band #
informed_choice_scotland_by_age <- Age_band_joined %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`Age Band`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_informed_choice = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

informed_choice_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_age %>% 
    filter(`Age Band` != "Scotland Total"),
  x_var = `Age Band`,
  y_var = percentage_informed_choice,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by age",
    width = 60
  ), 
  x_lab = "Age", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = informed_choice_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = 2,
    y = informed_choice_scotland,
    label = paste0("Scottish average: ", round(informed_choice_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
informed_choice_scotland_by_age_barchart
save_plot_with_script_name(informed_choice_scotland_by_age_barchart)

# Barchart of Scotland average answer by SIMD #
informed_choice_scotland_by_SIMD <- SIMD_joined %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`Scottish Index of Multiple Deprivation Decile`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_informed_choice = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

informed_choice_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_SIMD %>% 
    filter(`Scottish Index of Multiple Deprivation Decile` != "Scotland Total"),
  x_var = reorder(
    `Scottish Index of Multiple Deprivation Decile`,
    as.numeric(sub("^([0-9]+).*", "\\1",
                   `Scottish Index of Multiple Deprivation Decile`))
  ),
  y_var = percentage_informed_choice,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by SIMD",
    width = 60
  ), 
  x_lab = "SIMD", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = informed_choice_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = 3,
    y = informed_choice_scotland,
    label = paste0("Scottish average: ", round(informed_choice_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
informed_choice_scotland_by_SIMD_barchart
save_plot_with_script_name(informed_choice_scotland_by_SIMD_barchart)

# Barchart of Scotland average answer by Urban 8 #
informed_choice_scotland_by_urban <- Urban_Rural_8_joined %>%
  filter(
    `Question Number` == "q16m",
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
    percentage_informed_choice = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

informed_choice_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_urban %>% 
    filter(`Urban-Rural 8-fold classification` != "Scotland Total"),
  x_var = `Urban-Rural 8-fold classification`,
  y_var = percentage_informed_choice,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by Urban-Rural 8",
    width = 60
  ), 
  x_lab = "Urban-Rural 8-fold classification", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = informed_choice_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = 3,
    y = informed_choice_scotland,
    label = paste0("Scottish average: ", round(informed_choice_scotland, 0), "%"),
    hjust = 1,
    vjust = -1.5, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
informed_choice_scotland_by_urban_barchart
save_plot_with_script_name(informed_choice_scotland_by_urban_barchart)

##----------------------------------------------------------------------------#
##Barchart by Chronic Pain ##
informed_choice_scotland_by_chronic_pain <- `Chronic Pain` %>% 
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_informed_choice = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


informed_choice_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_chronic_pain %>% 
    mutate(`By Question Response Option` = factor(
      `By Question Response Option`,
      levels = c("Yes", "No", "Skipped Q42")
    )),
  x_var = `By Question Response Option`,
  y_var = percentage_informed_choice,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by Chronic pain",
    width = 60
  ), 
  x_lab = "Do you suffer from chronic or persistent pain, that is pain that carries on for longer than 3 months despite medication or treatment?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = informed_choice_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = informed_choice_scotland,
    label = paste0("Scottish average: ", round(informed_choice_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


informed_choice_scotland_by_chronic_pain_barchart
save_plot_with_script_name(informed_choice_scotland_by_chronic_pain_barchart)

##  Barchart by Long term condition ##
informed_choice_scotland_by_long_term <- `Long-Term Condition` %>% 
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_informed_choice = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


informed_choice_scotland_by_long_term_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_long_term %>% 
    mutate(`By Question Response Option` = factor(
      `By Question Response Option`,
      levels = c("Yes", "No", "Skipped Question")
    )),
  x_var = `By Question Response Option`,
  y_var = percentage_informed_choice,
  title = str_wrap(
    "The percentage responding positively to, 'I felt able to  make an informed choice about my treatment and care' by long term condition",
    width = 60
  ), 
  x_lab = "Do you have any physical or mental health conditions or illnesses lasting or expected to last 12 months or more?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = informed_choice_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = informed_choice_scotland,
    label = paste0("Scottish average: ", round(informed_choice_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
informed_choice_scotland_by_long_term_barchart
save_plot_with_script_name(informed_choice_scotland_by_long_term_barchart)

## Barchart  by Sexual Orientation ##
informed_choice_scotland_by_sexual_orientation <- `Sexual Orientation` %>% 
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_informed_choice = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


informed_choice_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_sexual_orientation %>% 
    mutate(`By Question Response Option` = 
             forcats::fct_reorder(`By Question Response Option`,
                                  percentage_informed_choice,
                                  .desc = TRUE) %>%
             forcats::fct_relevel("Skipped Q43", after = Inf)
    ),
  x_var = `By Question Response Option`,
  y_var = percentage_informed_choice,
  title = str_wrap(
    "The percentage responding positively to, 'I felt able to  make an informed choice about my treatment and care' by sexual orientation",
    width = 60
  ), 
  x_lab = "Which of the following best describes your sexual orientation?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = informed_choice_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = informed_choice_scotland,
    label = paste0("Scottish average: ", round(informed_choice_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")

informed_choice_scotland_by_sexual_orientation_barchart
save_plot_with_script_name(informed_choice_scotland_by_sexual_orientation_barchart)

## # Barchart by Ethnicity ##
informed_choice_scotland_by_ethnicity <- Ethnicity %>% 
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_informed_choice = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

informed_choice_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_ethnicity %>% 
    mutate(
      `By Question Response Option` = 
        forcats::fct_reorder(`By Question Response Option`,
                             percentage_informed_choice,
                             .desc = TRUE) %>%
        forcats::fct_relevel("Skipped Q44", after = Inf)
    ),
  x_var = `By Question Response Option`,
  y_var = percentage_informed_choice,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by ethnicity",
    width = 60
  ), 
  x_lab = "What is your ethnic group?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = informed_choice_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = informed_choice_scotland,
    label = paste0("Scottish average: ", round(informed_choice_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")

informed_choice_scotland_by_ethnicity_barchart
save_plot_with_script_name(informed_choice_scotland_by_ethnicity_barchart)
