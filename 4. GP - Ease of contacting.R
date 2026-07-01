### Analysis for HACE q03
# June 2026

## HSC vision portfolio indicators 
# General Practice – Ease of contacting GP

## What it measures 
#Percentage of people positive about how easy it was to contact their General 
#Practice in the way they wanted.

## HACE Question (q03)
# q03 How easy is it for you to contact your GP practice in the way that you want?


## Core value
# Access – Care at the right time and in the right place

#------------------------------------------------------------------------------#

# Set SGplot for default chart colours
sgplot::use_sgplot()

#Source function to save plots from utility script
source("1. Utility.R")

# Summary table showing the percentage of respondents who rated the Ease of contacting 
# their General Practice as positive for Scotland
easy_contact_scotland <- Scotland %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive") %>%
  summarise(
    percentage_easy_contact_scotland = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pull(percentage_easy_contact_scotland)

# For individual GP practices
easy_contact_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive") %>%
  group_by(`GP Practice name`) %>%
  summarise(
    percentage_easy_contact_GP = sum(Percentage, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(percentage_easy_contact_GP) %>%
  mutate(order = row_number())

# For GP Clusters
easy_contact_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_easy_contact_cluster = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_easy_contact_cluster) %>%
  mutate(order = row_number())

# For Health and Social Care partnerships
easy_contact_HSCP <- HSCP %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_easy_contact_HSCP = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_easy_contact_HSCP) %>%
  mutate(order = row_number())

# For Health Board
easy_contact_HB <- `Health Board` %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_easy_contact_HB = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_easy_contact_HB) %>%
  mutate(order = row_number())
################################################################################
## Barchart of urgent access national level ------------------------------------
bands <- paste0(seq(0, 90, 10), "-", seq(10, 100, 10))

scotland_band <- cut(
  easy_contact_scotland,
  breaks = seq(0, 100, by = 10),
  labels = bands,
  include.lowest = TRUE,
  right = FALSE
)

easy_contact_GP_binned <- easy_contact_GP %>%
  mutate(
    pct_band = cut(
      percentage_easy_contact_GP,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, 
           fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- easy_contact_GP_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


easy_contact_GP_barchart <- make_barchart_multiple_groups(
  data = easy_contact_GP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "The percentage of respondents who rated the ease of their General Practice as positive by GP practice",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Number of GP practices"
  )+
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
    label = paste0("Scottish average ", round(easy_contact_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )
easy_contact_GP_barchart
save_plot_with_script_name(easy_contact_GP_barchart)

# # By Cluster
# easy_contact_cluster_binned <- easy_contact_cluster %>%
#   mutate(
#     pct_band = cut(
#       percentage_easy_contact_cluster,
#       breaks = seq(0, 100, by = 10),
#       labels = bands,
#       include.lowest = TRUE,
#       right = FALSE
#     )
#   ) %>%
#   count(pct_band, name = "n_practices") %>%
#   complete(pct_band = bands, fill = list(n_practices = 0)) %>%
#   mutate(pct_band = factor(pct_band, levels = bands))
# 
# scotland_y <- easy_contact_cluster_binned %>%
#   filter(pct_band == scotland_band) %>%
#   pull(n_practices)
# 
# 
# easy_contact_cluster_barchart <- make_barchart_multiple_groups(
#   data = easy_contact_cluster_binned,
#   x_var = pct_band,
#   y_var = n_practices,
#   title = str_wrap(
#     "The percentage of respondents who rated the ease of their General Practice as positive by GP cluster",
#     width = 60
#   ),
#   x_lab = "Percentage (%)",
#   y_lab = "Number of GP clusters")+
#   geom_point(
#     data = data.frame(
#       pct_band = scotland_band,
#       n_practices = 1
#     ),
#     aes(x = pct_band, y = scotland_y),
#     colour = "red",
#     size = 4
#   )+
#   annotate(
#     "text",
#     x = scotland_band,
#     y = scotland_y,
#     label = paste0("Scottish average ", round(easy_contact_scotland, 0), "%"),
#     vjust = -0.8,
#     colour = "red",
#     fontface = "bold"
#   )
# easy_contact_cluster_barchart
# save_plot_with_script_name(easy_contact_cluster_barchart)

# # By HSCP
# easy_contact_HSCP_binned <- easy_contact_HSCP %>%
#   mutate(
#     pct_band = cut(
#       percentage_easy_contact_HSCP,
#       breaks = seq(0, 100, by = 10),
#       labels = bands,
#       include.lowest = TRUE,
#       right = FALSE
#     )
#   ) %>%
#   count(pct_band, name = "n_practices") %>%
#   complete(pct_band = bands, fill = list(n_practices = 0)) %>%
#   mutate(pct_band = factor(pct_band, levels = bands))
# 
# scotland_y <- easy_contact_HSCP_binned %>%
#   filter(pct_band == scotland_band) %>%
#   pull(n_practices)
# 
# 
# easy_contact_HSCP_barchart <- make_barchart_multiple_groups(
#   data = easy_contact_HSCP_binned,
#   x_var = pct_band,
#   y_var = n_practices,
#   title = str_wrap(
#     "The percentage of respondents who rated the ease of their General Practice as positive by HSCP",
#     width = 60
#   ),
#   x_lab = "Percentage (%)",
#   y_lab = "Number of HSCPs")+
#   geom_point(
#     data = data.frame(
#       pct_band = scotland_band,
#       n_practices = 1
#     ),
#     aes(x = pct_band, y = scotland_y),
#     colour = "red",
#     size = 4
#   )+
#   annotate(
#     "text",
#     x = scotland_band,
#     y = scotland_y,
#     label = paste0("Scottish average ", round(easy_contact_scotland, 0), "%"),
#     vjust = -0.8,
#     colour = "red",
#     fontface = "bold"
#   )
# easy_contact_HSCP_barchart
# save_plot_with_script_name(easy_contact_HSCP_barchart)

################################################################################
##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
easy_contact_scotland_by_sex <- Sex_joined %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`Sex`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_easy_contact = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

easy_contact_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_sex %>% 
    filter(Sex != "Scotland Total"),
  x_var = Sex,
  y_var = percentage_easy_contact,
  title = str_wrap(
    "The percentage of respondents who rated the ease of their General Practice as positive by sex",
    width = 60
  ), 
  x_lab = "Sex", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = easy_contact_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = easy_contact_scotland,
    label = paste0("Scottish average: ", round(easy_contact_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
easy_contact_scotland_by_sex_barchart
save_plot_with_script_name(easy_contact_scotland_by_sex_barchart)

## Barchart of Scotland average answer by Age band #
easy_contact_scotland_by_age <- Age_band_joined %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`Age Band`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_easy_contact = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

easy_contact_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_age %>% 
    filter(`Age Band` != "Scotland Total"),
  x_var = `Age Band`,
  y_var = percentage_easy_contact,
  title = str_wrap(
    "The percentage of respondents who rated the ease of their General Practice as positive by age",
    width = 60
  ), 
  x_lab = "Age", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = easy_contact_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = 2,
    y = easy_contact_scotland,
    label = paste0("Scottish average: ", round(easy_contact_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
easy_contact_scotland_by_age_barchart
save_plot_with_script_name(easy_contact_scotland_by_age_barchart)

# Barchart of Scotland average answer by SIMD #
easy_contact_scotland_by_SIMD <- SIMD_joined %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`Scottish Index of Multiple Deprivation Decile`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_easy_contact = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

easy_contact_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_SIMD %>% 
    filter(`Scottish Index of Multiple Deprivation Decile` != "Scotland Total"),
  x_var = reorder(
    `Scottish Index of Multiple Deprivation Decile`,
    as.numeric(sub("^([0-9]+).*", "\\1",
                   `Scottish Index of Multiple Deprivation Decile`))
  ),
  y_var = percentage_easy_contact,
  title = str_wrap(
    "The percentage of respondents who rated the ease of their General Practice as positive by SIMD",
    width = 60
  ), 
  x_lab = "SIMD", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = easy_contact_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = 3,
    y = easy_contact_scotland,
    label = paste0("Scottish average: ", round(easy_contact_scotland, 0), "%"),
    hjust = 1,
    vjust = -1, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
easy_contact_scotland_by_SIMD_barchart
save_plot_with_script_name(easy_contact_scotland_by_SIMD_barchart)

# Barchart of Scotland average answer by Urban 8 #
easy_contact_scotland_by_urban <- Urban_Rural_8_joined %>%
  filter(
    `Question Number` == "q03",
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
    percentage_easy_contact = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

easy_contact_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_urban %>% 
    filter(`Urban-Rural 8-fold classification` != "Scotland Total"),
  x_var = `Urban-Rural 8-fold classification`,
  y_var = percentage_easy_contact,
  title = str_wrap(
    "The percentage of respondents who rated the ease of their General Practice as positive by Urban-Rural 8",
    width = 60
  ), 
  x_lab = "Urban-Rural 8-fold classification", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = easy_contact_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = 3,
    y = easy_contact_scotland,
    label = paste0("Scottish average: ", round(easy_contact_scotland, 0), "%"),
    hjust = 1,
    vjust = -1.5, 
    colour = "red"
  ) +
  coord_cartesian(clip = "off")
easy_contact_scotland_by_urban_barchart
save_plot_with_script_name(easy_contact_scotland_by_urban_barchart)

##----------------------------------------------------------------------------#
##Barchart by Chronic Pain ##
easy_contact_scotland_by_chronic_pain <- `Chronic Pain` %>% 
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_easy_contact = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


easy_contact_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_chronic_pain %>% 
    mutate(`By Question Response Option` = factor(
      `By Question Response Option`,
      levels = c("Yes", "No", "Skipped Q42")
    )),
  x_var = `By Question Response Option`,
  y_var = percentage_easy_contact,
  title = str_wrap(
    "The percentage of respondents who rated the ease of their General Practice as positive by Chronic pain",
    width = 60
  ), 
  x_lab = "Do you suffer from chronic or persistent pain, that is pain that carries on for longer than 3 months despite medication or treatment?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = easy_contact_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = easy_contact_scotland,
    label = paste0("Scottish average: ", round(easy_contact_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


easy_contact_scotland_by_chronic_pain_barchart
save_plot_with_script_name(easy_contact_scotland_by_chronic_pain_barchart)

##  Barchart by Long term condition ##
easy_contact_scotland_by_long_term <- `Long-Term Condition` %>% 
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_easy_contact = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


easy_contact_scotland_by_long_term_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_long_term %>% 
    mutate(`By Question Response Option` = factor(
      `By Question Response Option`,
      levels = c("Yes", "No", "Skipped Question")
    )),
  x_var = `By Question Response Option`,
  y_var = percentage_easy_contact,
  title = str_wrap(
    "The percentage of respondents who rated the ease of their General Practice as positive by long term condition",
    width = 60
  ), 
  x_lab = "Do you have any physical or mental health conditions or illnesses lasting or expected to last 12 months or more?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = easy_contact_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = easy_contact_scotland,
    label = paste0("Scottish average: ", round(easy_contact_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")


easy_contact_scotland_by_long_term_barchart
save_plot_with_script_name(easy_contact_scotland_by_long_term_barchart)

## Barchart  by Sexual Orientation ##
easy_contact_scotland_by_sexual_orientation <- `Sexual Orientation` %>% 
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_easy_contact = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )


easy_contact_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_sexual_orientation %>% 
    mutate(`By Question Response Option` = 
             forcats::fct_reorder(`By Question Response Option`,
                                  percentage_easy_contact,
                                  .desc = TRUE) %>%
             forcats::fct_relevel("Skipped Q43", after = Inf)
    ),
  x_var = `By Question Response Option`,
  y_var = percentage_easy_contact,
  title = str_wrap(
    "The percentage of respondents who rated the ease of their General Practice as positive by sexual orientation",
    width = 60
  ), 
  x_lab = "Which of the following best describes your sexual orientation?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = easy_contact_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = easy_contact_scotland,
    label = paste0("Scottish average: ", round(easy_contact_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")

easy_contact_scotland_by_sexual_orientation_barchart
save_plot_with_script_name(easy_contact_scotland_by_sexual_orientation_barchart)

## # Barchart by Ethnicity ##
easy_contact_scotland_by_ethnicity <- Ethnicity %>% 
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`By Question Response Option`) %>%
  summarise(
    `Question Number` = first(`Question Number`),
    `Question Text` = first(`Question Text`),
    `Response Option` = "positive",
    percentage_easy_contact = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

easy_contact_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_ethnicity %>% 
    mutate(
      `By Question Response Option` = 
        forcats::fct_reorder(`By Question Response Option`,
                             percentage_easy_contact,
                             .desc = TRUE) %>%
        forcats::fct_relevel("Skipped Q44", after = Inf)
    ),
  x_var = `By Question Response Option`,
  y_var = percentage_easy_contact,
  title = str_wrap(
    "The percentage of respondents who rated the ease of their General Practice as positive by ethnicity",
    width = 60
  ), 
  x_lab = "What is your ethnic group?", 
  y_lab = "Percentage (%)"
)+
  geom_hline(
    yintercept = easy_contact_scotland,
    linetype = "dashed",
    colour = "red"
  )+
  annotate(
    "text",
    x = Inf,
    y = easy_contact_scotland,
    label = paste0("Scottish average: ", round(easy_contact_scotland, 1), "%"),
    hjust = 1,
    vjust = -2,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")

easy_contact_scotland_by_ethnicity_barchart
save_plot_with_script_name(easy_contact_scotland_by_ethnicity_barchart)

###############################################################################
## Comparing to the last surveys results at Scotland level ##
## Cleaning 2021 results
easy_contact_scotland_2021 <- `Scotland - PNN Questions` %>% 
  filter(
    `Question Number` == "3"
  )%>%
  select(-c("Questionnaire Section", "Scotland", "% Neutral", "% Negative"))%>% 
  pivot_longer(
    cols = starts_with("%"),
    names_to = "Response Option",
    values_to = "Percentage"
  ) %>%
  mutate(
    `Response Option` = gsub("% ", "", `Response Option`),
    `Response Option` = tolower(`Response Option`),
    "Year"= "2021",
    Percentage = as.numeric(as.character(Percentage))
  )

## Cleaning 2023 results
easy_contact_scotland_2023 <- `Positive, Neutral or Negative` %>% 
  filter(
    `Geography Type` == "Scotland",
    `Question Number` == "q03", 
  )%>%
  select(-c("...11","Geography Type","Area", "Area Name", "Survey Section",
            "Percentage Neutral", "Percentage Negative",
            "Lower 95% Confidence Interval - Percentage Positive", 
            "Upper 95% Confidence Interval - Percentage Positive")
  ) %>% 
  pivot_longer(
    cols = starts_with("Percentage"),
    names_to = "Response Option",
    values_to = "Percentage"
  ) %>%
  mutate(
    `Response Option` = gsub("Percentage ", "", `Response Option`),
    `Response Option` = tolower(`Response Option`),
    "Year"= "2023",
    Percentage = as.numeric(as.character(Percentage))
  ) %>% 
  mutate(Percentage = Percentage*100)

easy_contact_scotland_2025 <- Scotland %>%
  filter(
    `Question Number` == "q03",
    `Response Option`=="positive"
  ) %>% 
  mutate(
    "Year"="2025"
  )%>% 
  select(-c("Topic", "Lower 95% Confidence Interval", "Upper 95% Confidence Interval"))

easy_contact_scotland_timeseries <- bind_rows(
  easy_contact_scotland_2021,
  easy_contact_scotland_2023,
  easy_contact_scotland_2025,
  # 2019 & 2017 row
  tibble(
    `Question Number` = rep("03",2),
    `Question Text` = rep("How easy is it for you to contact your GP practice in the way that you want?",2),
    `Number of Responses` = c(140969,118064),
    `Response Option` = rep("positive",2),
    `Percentage` = c(85, #2019
                     87), #2017
    `Year` = c("2019","2017")
  )) %>%
  mutate(`Response Option` = as.factor(`Response Option`))



glimpse(easy_contact_scotland_timeseries)

easy_contact_scotland_timeseries_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_timeseries,
  x_var = Year,
  y_var = Percentage,
  title = "Timeseries of percentage of respondents positive about the ease of contacting their GP",
  y_lab = "Percentage (%)",
  x_lab = "Year"
  )+
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  )+
  geom_text(
    aes(label = paste0(round(Percentage, 0), "%")),
    vjust = 2,
    size = 3,
    colour = "white"
  )
easy_contact_scotland_timeseries_barchart
save_plot_with_script_name(easy_contact_scotland_timeseries_barchart)

easy_contact_scotland_timeseries_scatter <- make_scatter(
  data = easy_contact_scotland_timeseries,
  x_var = Year,
  y_var = Percentage,
  title = "Timeseries of percentage of respondents positive about the ease of contacting their GP",
  y_lab = "Percentage (%)",
  x_lab = "Year"
  )+
  geom_line(aes(group = 1), linewidth = 1) +
  geom_text(
  aes(
    label = paste0(round(Percentage, 0), "%")),
    vjust = -0.9,
    size = 3
  )
easy_contact_scotland_timeseries_scatter
save_plot_with_script_name(easy_contact_scotland_timeseries_scatter)
#------------------------## Ease of contacting GP variation analysis ##--------------------------#
easy_contact_variation_by_GP <- variation_data_2025 %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(hscp_name, `GP Practice name`) %>%
  summarise(
    easy_contact_percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

easy_contact_variation_by_hscp <- easy_contact_variation_by_GP %>%
  group_by(hscp_name) %>%
  summarise(
    num_practices = n(),
    sd_pct = sd(easy_contact_percentage, na.rm = TRUE),
    min_pct = min(easy_contact_percentage, na.rm = TRUE),
    max_pct = max(easy_contact_percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))


easy_contact_variation_tails <- bind_rows(
  Top5 = easy_contact_variation_by_hscp %>%
    filter(num_practices > 15) %>%
    slice_head(n = 2),
  Bottom5 = easy_contact_variation_by_hscp %>%
    filter(num_practices > 15) %>%
    slice_tail(n = 2),
  ) %>%
  pull(hscp_name)


easy_contact_variation_tails_boxplot<- ggplot(
  easy_contact_variation_by_GP %>%
    filter(hscp_name %in% easy_contact_variation_tails),
  aes(x = reorder(hscp_name, easy_contact_percentage),
      y = easy_contact_percentage)
) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  coord_flip() +
  labs(
    title = "Ease of contacting GP: Top 2 and Bottom 2 HSCPs by variation",
    x = "HSCP",
    y = "% positive"
  ) +
  theme_minimal()
easy_contact_variation_tails_boxplot
save_plot_with_script_name(easy_contact_variation_tails_boxplot)

chosen_easy_contact_variation_by_hscp <- easy_contact_variation_by_GP %>%
  filter(
    hscp_name %in% easy_contact_variation_tails
  )

chosen_easy_contact_variation_by_hscp_plot <- ggplot(
  data = chosen_easy_contact_variation_by_hscp, 
  aes(x = reorder(hscp_name, easy_contact_percentage), 
      y = easy_contact_percentage)
  ) +
  geom_jitter(
    aes(colour = hscp_name, shape = hscp_name),
    width = 0.4, height = 0,
    size = 3, alpha = 0.75
  ) +
  geom_hline(
    yintercept = easy_contact_scotland,
    linetype = "solid",
    colour = "black"
  ) +
  annotate(
    "text",
    x = 3.9,
    y = easy_contact_scotland,
    label = paste0("Scotland average ", round(easy_contact_scotland,0),"%"),
    hjust = 0,
    vjust = 1.5,
    colour = "black",
    size = 4
  )+
  scale_y_continuous(limits = c(0,100))+
  labs(
    title = "% positive about the ease of contacting their GP by GP Practice, grouped by HSCP",
    x = "HSCP",
    y = "% positive"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

chosen_easy_contact_variation_by_hscp_plot
save_plot_with_script_name(chosen_easy_contact_variation_by_hscp_plot)

#Violin plot
chosen_easy_contact_variation_by_hscp_violin <- ggplot(
  chosen_easy_contact_variation_by_hscp,
  aes(x = reorder(hscp_name, easy_contact_percentage), y = easy_contact_percentage)
  )+
  geom_violin(
    fill = "grey80",
    alpha = 0.6,
  )+
  geom_hline(
    yintercept = easy_contact_scotland,
    colour = "black"
  )+
  annotate(
    "text",
    x = 3.9,
    y = easy_contact_scotland,
    label = paste0("Scotland average ", round(easy_contact_scotland,0),"%"),
    hjust = 0,
    vjust = 1.5,
    colour = "black",
    size = 4
  )+
  geom_jitter(
    aes(colour = hscp_name, shape = hscp_name),
    width = 0.15,
    size = 2,
    alpha = 0.7,
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Percentage of respondents positive about the ease of contacting their GP,by GP Practice (grouped by HSCP)",
    x = "HSCP (anonymised)",
    y = "% positive"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )
chosen_easy_contact_variation_by_hscp_violin
save_plot_with_script_name(chosen_easy_contact_variation_by_hscp_violin)


