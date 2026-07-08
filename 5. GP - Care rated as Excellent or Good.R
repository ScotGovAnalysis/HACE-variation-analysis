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
scale_colour_sg(palette = "main-extended")

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
  group_by(`Area`) 

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
  geom_col(fill = "#19AB19")+
  geom_text(
    aes(
      label = round(n_practices, 0),
      colour = n_practices < 25,
      vjust = ifelse(n_practices < 25, -0.5, 2)
    ),
    size = 4,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("FALSE" = "white",
               "TRUE" = "black")
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
    "The percentage of respondents who rated the overall care from their General Practice as positive by HSCP",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Number of HSCPs")+
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
#Geographical variation 

HSCP_barchart <- make_barchart_multiple_groups(
  data = overall_care_HSCP ,
  x_var = Percentage,
  y_var = reorder(Area, Percentage),
  title = "Percentage by HSCP",
  x_lab = "Percentage (%)",
  y_lab = ""
  )+
  geom_col(fill = "#19AB19")+
  geom_errorbar(
    aes(
      xmin = `Lower 95% Confidence Interval`,
      xmax = `Upper 95% Confidence Interval`
    ),
    width = 0.2,
    linewidth = 0.5,
    colour = "black"
  )+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0,100),
    expand = expansion(mult = c(0, 0.05))
  )
HSCP_barchart
save_plot_with_script_name(HSCP_barchart,
                           width = 15.68,
                           height = 11.85)


################################################################################
##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
overall_care_scotland_by_sex <- Sex %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`Sex`)


overall_care_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_sex,
  x_var = Percentage,
  y_var = reorder(Sex, Percentage),
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by sex",
    width = 60
  ),   
  x_lab = "Percentage (%)",
  y_lab = "Sex" 
  )+
  geom_col(fill = "#19AB19" )+
  geom_errorbar(
    aes(
      xmin = `Lower 95% Confidence Interval`,
      xmax = `Upper 95% Confidence Interval`
    ),
    width = 0.2,
    linewidth = 0.5,
    colour = "black"
  ) +
  geom_text(
    aes(label = paste0(round(Percentage, 0), "%")),
    hjust = 1.5,
    size = 4,
    colour = "white"
  )+
  scale_x_continuous(limits = c(0,100))+
  scale_y_discrete(
    labels = stringr::str_to_title
  )
overall_care_scotland_by_sex_barchart
save_plot_with_script_name(overall_care_scotland_by_sex_barchart)

## Barchart of Scotland average answer by Age band #
overall_care_scotland_by_age <- `Age Band` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`Age Band`) 

# %>%
#   summarise(
#     `Question Number` = first(`Question Number`),
#     `Question Text` = first(`Question Text`),
#     `Response Option` = "positive",
#     percentage_overall_care = sum(Percentage, na.rm = TRUE),
#     .groups = "drop"
#   )

overall_care_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_age, 
  x_var = Percentage,
  y_var = `Age Band`,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by age",
    width = 60
  ), 
  x_lab = "Percentage (%)",
  y_lab = "Age band", 
  bar_width = 0.75,
  bar_colour = "19AB19"
  )+
  geom_errorbar(
    aes(
      xmin = `Lower 95% Confidence Interval`,
      xmax = `Upper 95% Confidence Interval`
    ),
    width = 0.2,
    linewidth = 0.5,
    colour = "black"
  ) +
  scale_x_continuous(limits = c(0,100))+
  scale_y_discrete(limits = rev)

overall_care_scotland_by_age_barchart
save_plot_with_script_name(overall_care_scotland_by_age_barchart)

# Barchart of Scotland average answer by SIMD #
overall_care_scotland_by_SIMD <- SIMD %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`Scottish Index of Multiple Deprivation Decile`)

overall_care_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_SIMD %>% 
    filter(`Scottish Index of Multiple Deprivation Decile` != "Scotland Total"),
  x_var = Percentage,
  y_var = reorder(
    `Scottish Index of Multiple Deprivation Decile`,
    11-as.numeric(sub("^([0-9]+).*", "\\1",
                   `Scottish Index of Multiple Deprivation Decile`))
  ),
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by SIMD",
    width = 60
  ), 
  x_lab = "Percentage (%)",
  y_lab = "SIMD decile",
  bar_colour = "#19AB19"
  )+
  geom_errorbar(
    aes(
      xmin = `Lower 95% Confidence Interval`,
      xmax = `Upper 95% Confidence Interval`
    ),
    width = 0.2,
    linewidth = 0.5,
    colour = "black"
  ) +
  scale_x_continuous(limits = c(0,100))
overall_care_scotland_by_SIMD_barchart
save_plot_with_script_name(overall_care_scotland_by_SIMD_barchart)

# Barchart of Scotland average answer by Urban 8 #
overall_care_scotland_by_urban <- `Urban-Rural 8` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`Urban-Rural 8-fold classification`)

overall_care_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_urban,
  y_var = factor(
    `Urban-Rural 8-fold classification`,
    levels = rev(sort(unique(`Urban-Rural 8-fold classification`)))
  ),
  x_var = Percentage,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by Urban-Rural 8",
    width = 60
  ), 
  y_lab = "", 
  x_lab = "Percentage (%)",
  bar_colour = "#19AB19"
  )+
  geom_errorbar(
    aes(
      xmin = `Lower 95% Confidence Interval`,
      xmax = `Upper 95% Confidence Interval`
    ),
    width = 0.2,
    linewidth = 0.5,
    colour = "black"
  )+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0,100),
    expand = expansion(mult = c(0, 0.05))
    )

overall_care_scotland_by_urban_barchart
save_plot_with_script_name(overall_care_scotland_by_urban_barchart)

##----------------------------------------------------------------------------#
##Barchart by Chronic Pain ##
overall_care_scotland_by_chronic_pain <- `Chronic Pain` %>% 
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive",
    `By Question Response Option` != "Skipped Q42"
  ) %>%
  group_by(`By Question Response Option`)

overall_care_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_chronic_pain,
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by Chronic pain",
    width = 60
  ), 
  x_lab = "Percentage (%)",
  y_lab = "Experienced chronic pain?", 
  bar_colour = "#19AB19"
  )+
  geom_errorbar(
    aes(
      xmin = `Lower 95% Confidence Interval`,
      xmax = `Upper 95% Confidence Interval`
    ),
    width = 0.2,
    linewidth = 0.5,
    colour = "black"
  )+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0,100),
    expand = expansion(mult = c(0, 0.05))
  )

overall_care_scotland_by_chronic_pain_barchart
save_plot_with_script_name(overall_care_scotland_by_chronic_pain_barchart,width = 15,height = 3)

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
    `Response Option` == "positive",
    `By Question Response Option` != "Skipped Q43"
  ) %>%
  mutate(
    `By Question Response Option` =
      forcats::fct_reorder(
        `By Question Response Option`,
        Percentage,
        .desc = TRUE
      )
  )

overall_care_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_sexual_orientation ,
  x_var = Percentage,
  y_var = reorder(`By Question Response Option`,Percentage),
  title = str_wrap(
    "The percentage of respondents who rated the overall care from their General Practice as positive by sexual orientation",
    width = 60
  ), 
  x_lab = "Percentage (%)",
  y_lab = "Sexual orientation", 
  bar_colour = "#19AB19"
  )+
  geom_errorbar(
    aes(
      xmin = `Lower 95% Confidence Interval`,
      xmax = `Upper 95% Confidence Interval`
    ),
    width = 0.2,
    linewidth = 0.5,
    colour = "black"
  )+
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0,100),
    expand = expansion(mult = c(0, 0.05))
  )
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
    vjust = -1.5,
    colour = "red"
  ) +
  coord_cartesian(clip = "off")

overall_care_scotland_by_ethnicity_barchart
save_plot_with_script_name(overall_care_scotland_by_ethnicity_barchart)

###############################################################################
## Comparing to the last surveys results at Scotland level ##
## Cleaning 2021 results
overall_care_scotland_2022 <- `Scotland - PNN Questions` %>% 
  filter(
    `Question Number` == "10"
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
    "Year"= "2021-22",
    Percentage = as.numeric(as.character(Percentage))
  )

## Cleaning 2023 results
overall_care_scotland_2024 <- `Positive, Neutral or Negative` %>% 
  filter(
    `Geography Type` == "Scotland",
    `Question Number` == "q13"
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
    "Year"= "2023-24",
    Percentage = as.numeric(as.character(Percentage))
  ) %>% 
  mutate(Percentage = Percentage*100)

overall_care_scotland_2026 <- Scotland %>%
  filter(
    `Question Number` == "q13",
    `Response Option`=="positive"
    ) %>% 
  mutate(
    "Year"="2025-26"
  )%>% 
  select(-c("Topic", "Lower 95% Confidence Interval", "Upper 95% Confidence Interval"))

overall_care_scotland_timeseries <- bind_rows(
  overall_care_scotland_2022,
  overall_care_scotland_2024,
  overall_care_scotland_2026,
    # 2019 & 2017 row
    tibble(
      `Question Number` = rep("10",2),
      `Question Text` = rep("Overall, how would you rate the care provided by your GP Practice?",2),
      `Number of Responses` = c(137249,115006),
      `Response Option` = rep(c("positive"),2),
      `Percentage` = c(79, #2019
                       83), #2017
      `Year` = c("2019-20","2017-18")
    )) %>%
    mutate(`Response Option` = as.factor(`Response Option`))



glimpse(overall_care_scotland_timeseries)

overall_care_scotland_timeseries_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_timeseries, 
  x_var = Year, 
  y_var = Percentage,
  title = "Timeseries of overall care rated positive",
  x_lab = "Year",
  y_lab = "Percentage (%)",
  )+
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  )+
  geom_col(fill = "#19AB19")+
  geom_text(
    aes(label = paste0(round(Percentage, 0), "%")),
    vjust = 2,
    size = 4,
    colour = "white"
  )

overall_care_scotland_timeseries_barchart  
save_plot_with_script_name(overall_care_scotland_timeseries_barchart)

overall_care_scotland_timeseries_line <- make_scatter(
  data = overall_care_scotland_timeseries,
  x_var = Year,
  y_var = Percentage,
  title = "Timeseries of overall care rated positive",
  y_lab = "Percentage (%)",
  x_lab = "Year"
  )+
  geom_line(colour = "#19AB19",
            aes(group = 2), 
            linewidth = 2, ) +
  geom_point(size = 4, colour = "#19AB19")+
  geom_text(
    aes(
      label = paste0(round(Percentage, 0), "%")),
    vjust = -0.9,
    size = 4
  )
overall_care_scotland_timeseries_line
save_plot_with_script_name(overall_care_scotland_timeseries_line)

#------------## Overall care variation by HSCP  analysis ##--------------------#
overall_care_variation_by_GP <- variation_data_2025 %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(hscp_name, `GP Practice name`) %>%
  summarise(
    overall_care_percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

overall_care_variation_by_hscp <- overall_care_variation_by_GP %>%
  group_by(hscp_name) %>%
  summarise(
    num_practices = n(),
    sd_pct = sd(overall_care_percentage, na.rm = TRUE),
    min_pct = min(overall_care_percentage, na.rm = TRUE),
    max_pct = max(overall_care_percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))


overall_care_variation_tails <- bind_rows(
  Top5 = overall_care_variation_by_hscp %>%
    filter(num_practices > 15) %>%
    slice_head(n = 2),
  Bottom5 = overall_care_variation_by_hscp %>% 
    filter(num_practices > 15) %>%
    slice_tail(n = 2),
  .id = "Group"
  ) %>%
  pull(hscp_name)


ggplot(
  overall_care_variation_by_GP %>%
    filter(hscp_name %in% overall_care_variation_tails),
  aes(x = reorder(hscp_name, overall_care_percentage),
      y = overall_care_percentage)
) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  coord_flip() +
  labs(
    title = "overall care: Top 5 and Bottom 5 HSCPs by variation",
    x = "HSCP",
    y = "% positive"
  ) +
  theme_minimal()

chosen_overall_care_variation_by_hscp <- overall_care_variation_by_GP %>%
  filter(
    hscp_name %in% overall_care_variation_tails
  )


chosen_overall_care_variation_by_hscp_plot <- ggplot(
  chosen_overall_care_variation_by_hscp, 
  aes(x = reorder(hscp_name, overall_care_percentage), y = overall_care_percentage)) +
  geom_jitter(
    aes(colour = hscp_name),
    width = 0.4, height = 0,
    size = 3, alpha = 0.75
  ) +
  scale_y_continuous(limits = c(0,100))+
  labs(
    title = "% rating overall care positive by GP Practice, grouped by HSCP",
    x = "HSCP",
    y = "% positive"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0)
  )
chosen_overall_care_variation_by_hscp_plot <- ggplot(
  data = chosen_overall_care_variation_by_hscp, 
  aes(x = reorder(hscp_name, overall_care_percentage), 
      y = overall_care_percentage)
) +
  geom_jitter(
    aes(colour = hscp_name),
    width = 0.4, height = 0,
    size = 3, alpha = 0.75
  ) +
scale_y_continuous(limits = c(0,100))+
  labs(
    title = "% positive about the overall care provided by their GP by GP Practice, grouped by HSCP",
    x = "HSCP",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

chosen_overall_care_variation_by_hscp_plot
save_plot_with_script_name(chosen_overall_care_variation_by_hscp_plot)

#------------## Overall care variation by GP Cluster  analysis ##--------------------#
overall_care_variation_by_GP <- variation_data_2025 %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(hscp_gpcl_name, `GP Practice name`) %>%
  summarise(
    overall_care_percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

overall_care_variation_by_cluster <- overall_care_variation_by_GP %>%
  group_by(hscp_gpcl_name) %>%
  summarise(
    num_practices = n(),
    sd_pct = sd(overall_care_percentage, na.rm = TRUE),
    min_pct = min(overall_care_percentage, na.rm = TRUE),
    max_pct = max(overall_care_percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))


overall_care_cluster_variation_tails <- bind_rows(
  Top2 = overall_care_variation_by_cluster %>% 
    filter(num_practices > 5) %>% 
    slice_head(n = 2),
  Bottom2 = overall_care_variation_by_cluster %>% 
    filter(num_practices > 5) %>% 
    slice_tail(n = 2),
  .id = "Group"
) %>%
  pull(hscp_gpcl_name)


ggplot(
  overall_care_variation_by_GP %>%
    filter(hscp_gpcl_name %in% overall_care_cluster_variation_tails),
  aes(x = reorder(hscp_gpcl_name, overall_care_percentage),
      y = overall_care_percentage)
) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  coord_flip() +
  labs(
    title = "overall care: Top 2 and Bottom 2 GP Clusters by variation",
    x = "GP Cluster",
    y = "% positive"
  ) +
  theme_minimal()

chosen_overall_care_variation_by_cluster <- overall_care_variation_by_GP %>%
  filter(
    hscp_gpcl_name %in% overall_care_cluster_variation_tails
  )


chosen_overall_care_variation_by_cluster_plot <- ggplot(
  data = chosen_overall_care_variation_by_cluster, 
  aes(x = reorder(hscp_gpcl_name, overall_care_percentage), 
      y = overall_care_percentage)
  )+
  geom_jitter(
    aes(colour = hscp_gpcl_name, shape = hscp_gpcl_name),
    width = 0.4, height = 0,
    size = 3, alpha = 0.75
  )+
  scale_colour_discrete_sg(palette = "main-extended")+
  scale_y_continuous(limits = c(0,100))+
  labs(
    title = "% positive about the ease of contacting their GP by GP Practice, grouped by GP Cluser",
    x = "HSCP",
    y = "% positive"
  )+
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

chosen_overall_care_variation_by_cluster_plot
save_plot_with_script_name(chosen_overall_care_variation_by_cluster_plot)

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
