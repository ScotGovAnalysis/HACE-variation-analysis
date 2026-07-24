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
  pull(Percentage)

# For individual GP practices
overall_care_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive")

# For GP Clusters
overall_care_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive")

# For Health and Social Care partnerships
overall_care_HSCP <- HSCP %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive")

# For Health Board
overall_care_HB <- `Health Board` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive") 

################################################################################
## Barchart of overall care for all practices across Scotland ------------------
bands <- paste0(seq(0, 90, 10), "-", seq(10, 100, 10))

overall_care_scotland_band <- cut(
  overall_care_scotland,
  breaks = seq(0, 100, by = 10),
  labels = bands,
  include.lowest = TRUE,
  right = FALSE
)

overall_care_GP_binned <- overall_care_GP %>%
  mutate(
    pct_band = cut(
      Percentage,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))


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
  geom_col(fill = quality_colour)+
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
# #save_plot_with_script_name(overall_care_GP_barchart)

################################################################################
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
  geom_col(fill = quality_colour)+
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
# #save_plot_with_script_name(HSCP_barchart, height = 11.85)


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
  geom_col(fill = quality_colour )+
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
# #save_plot_with_script_name(overall_care_scotland_by_sex_barchart)

## Barchart of Scotland average answer by Age band #
overall_care_scotland_by_age <- `Age Band` %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(`Age Band`) 

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
  bar_colour = quality_colour
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
# #save_plot_with_script_name(overall_care_scotland_by_age_barchart, width = 15,height = 8.16,show_title = FALSE)

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
  bar_colour = quality_colour
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
# #save_plot_with_script_name(overall_care_scotland_by_SIMD_barchart)

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
  bar_colour = quality_colour
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
#save_plot_with_script_name(overall_care_scotland_by_urban_barchart)

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
  title = "The percentage of respondents who rated the overall care from their General Practice as positive by Chronic pain",
  x_lab = "Percentage (%)",
  y_lab = "Chronic pain?", 
  bar_colour = quality_colour
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
#save_plot_with_script_name(overall_care_scotland_by_chronic_pain_barchart,width = 15,height = 4.5, show_title = FALSE)

##  Barchart by Long term condition ##
overall_care_scotland_by_long_term <- `Long-Term Condition` %>% 
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive",
    `By Question Response Option` !="Skipped Question"
  ) %>%
  group_by(`By Question Response Option`)



overall_care_scotland_by_long_term_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_long_term,
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = "The percentage of respondents who rated the overall care from their General Practice as positive by long term condition",
  x_lab = "Percentage (%)",
  y_lab = "Do you have any physical or mental health conditions or illnesses lasting or expected to last 12 months or more?", 
  bar_colour = quality_colour
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

overall_care_scotland_by_long_term_barchart
#save_plot_with_script_name(overall_care_scotland_by_long_term_barchart)

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
  title = "The percentage of respondents who rated the overall care from their General Practice as positive by sexual orientation",
  x_lab = "Percentage (%)",
  y_lab = "Sexual orientation", 
  bar_colour = quality_colour
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
#save_plot_with_script_name(overall_care_scotland_by_sexual_orientation_barchart)

## # Barchart by Ethnicity ##
overall_care_scotland_by_ethnicity <- Ethnicity %>% 
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive",
    `By Question Response Option`!="Skipped Q44"
  ) %>%
  group_by(`By Question Response Option`)

overall_care_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_ethnicity ,
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = "The percentage of respondents who rated the overall care from their General Practice as positive by ethnicity",
  x_lab = "Percentage (%)",
  y_lab = "What is your ethnic group?", 
  bar_colour = quality_colour
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

overall_care_scotland_by_ethnicity_barchart
#save_plot_with_script_name(overall_care_scotland_by_ethnicity_barchart)

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

overall_care_scotland_timeseries_line <- make_scatter(
  data = overall_care_scotland_timeseries,
  x_var = Year,
  y_var = Percentage,
  title = "Timeseries of overall care rated positive",
  y_lab = "Percentage (%)",
  x_lab = "Year"
  )+
  geom_line(colour = quality_colour,
            aes(group = 2), 
            linewidth = 2, ) +
  geom_point(size = 4, colour = quality_colour)+
  geom_text(
    aes(
      label = paste0(round(Percentage, 0), "%")),
    vjust = -0.9,
    size = 4
  )
overall_care_scotland_timeseries_line
#save_plot_with_script_name(overall_care_scotland_timeseries_line)

#------------## Overall care variation by HSCP  analysis ##--------------------#
#This creates a list of the overall care % positive broken down by HSCP, cluster and practice name. 
overall_care_variation_by_GP <- variation_data_2025 %>%
  filter(
    `Question Number` == "q13",
    `Response Option` =="positive"
  ) %>%
  group_by(hscp_name, hscp_gpcl_name, `GP Practice name`) %>%
  summarise(
    overall_care_percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

## Summary by HSCP to chose a HSCP with large enough number of clusters to analyse
overall_care_variation_by_hscp <- overall_care_variation_by_GP %>%
  group_by(hscp_name) %>%
  summarise(
    num_practices = n(),
    num_clusters = n_distinct(hscp_gpcl_name),
    sd_pct = sd(overall_care_percentage, na.rm = TRUE),
    min_pct = min(overall_care_percentage, na.rm = TRUE),
    max_pct = max(overall_care_percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))

overall_care_largest_hscp_by_clusters <- overall_care_variation_by_hscp %>%
  arrange(desc(num_clusters), desc(num_practices)) %>%
  slice_head(n = 5)
## This will extract the chosen HSCP based on highest number of clusters and individual practices
overall_care_chosen_hscp <- overall_care_largest_hscp_by_clusters %>% 
  slice_head(n = 1) %>% 
  pull(hscp_name)

# Get summary stats for the chosen HSCP
overall_care_variation_by_cluster <- overall_care_variation_by_GP %>%
  filter(hscp_name == overall_care_chosen_hscp) %>% 
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

# What clusters in the HSCP have the largest SD
top_sd <- overall_care_variation_by_cluster %>%
  arrange(desc(sd_pct)) %>%
  slice_head(n = 3)
# What clusters in the HSCP have the largest range
top_range <- overall_care_variation_by_cluster %>%
  filter(!hscp_gpcl_name %in% top_sd$hscp_gpcl_name) %>%
  arrange(desc(range_pct)) %>%
  slice_head(n = 3)

overall_care_cluster_variation_tails <- bind_rows(
  sd5 = top_sd,
  range5 = top_range,
  .id = "Group"
) %>%
  pull(hscp_gpcl_name)
overall_care_cluster_variation_tails

#Plot a combinition of the most varied clusters 
overall_care_variation_by_GP_Cluster_and_HSCP_plot_data <- overall_care_variation_by_GP %>%
  filter(
    hscp_name == overall_care_chosen_hscp,
    hscp_gpcl_name %in% overall_care_cluster_variation_tails
  ) %>%
  mutate(
    cluster_label = paste(
      "GP Cluster",
      as.integer(factor(hscp_gpcl_name))
    )
  )

overall_care_variation_by_GP_Cluster_and_HSCP_plot <- ggplot(
  overall_care_variation_by_GP_Cluster_and_HSCP_plot_data,
  aes(
    x = cluster_label,
    y = overall_care_percentage,
    colour = cluster_label,
    shape = cluster_label
  )
) +
  geom_jitter(
    width = 0.25,
    height = 0,
    size = 3
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_shape_manual(
    values = c(16, 17, 15, 18, 3, 4, 8, 7, 9, 10)
  ) +
  labs(
    title = "Variation of the positive overall care ratings for individual GP Practices within a HSCP, grouped by GP Clusters",
    x = "",
    y = "Percentage of respondents rating their overall care positively (%)",
    caption = paste(
      "Note: Each point represents an individual GP practice.",
      "Different colours and shapes are used to distinguish GP clusters.",
      "All GP clusters included in this chart belong to the same HSCP."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    plot.caption = element_text(hjust = 0, size = 12)
  )
overall_care_variation_by_GP_Cluster_and_HSCP_plot
#save_plot_with_script_name(plot, width = 29,height =15 ,show_title = TRUE)

################################################################################

# Save plots
save_plot_with_script_name(overall_care_GP_barchart)
save_plot_with_script_name(HSCP_barchart, height = 12)
save_plot_with_script_name(overall_care_scotland_by_sex_barchart)
save_plot_with_script_name(overall_care_scotland_by_age_barchart)
save_plot_with_script_name(overall_care_scotland_by_SIMD_barchart)
save_plot_with_script_name(overall_care_scotland_by_urban_barchart)
save_plot_with_script_name(overall_care_scotland_by_chronic_pain_barchart)
save_plot_with_script_name(overall_care_scotland_by_long_term_barchart)
save_plot_with_script_name(overall_care_scotland_by_sexual_orientation_barchart)
save_plot_with_script_name(overall_care_scotland_by_ethnicity_barchart)
save_plot_with_script_name(overall_care_scotland_timeseries_scatter)
save_plot_with_script_name(overall_care_variation_by_GP_Cluster_and_HSCP_plot, width = 29, height =15 ,show_title = TRUE)

