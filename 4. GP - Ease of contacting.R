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

#The percentage of respondents who rated the Ease of contacting their General 
# Practice as positive for Scotland for current survey year
easy_contact_scotland <- Scotland %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive") %>%
  pull(Percentage)

# For individual GP practices
easy_contact_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive")

# For GP Clusters
easy_contact_cluster <- `GP Cluster` %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive") 

# For Health and Social Care partnerships
easy_contact_HSCP <- HSCP %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive")

# For Health Board
easy_contact_HB <- `Health Board` %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive") 

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
  ungroup() %>%
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

easy_contact_GP_barchart <- make_barchart_multiple_groups(
  data = easy_contact_GP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = "The number of GP practices scoring in each percentage band of positive ratings for ease of contact",
  x_lab = "Percentage (%)",
  y_lab = "Number of GP practices",
  bar_colour = access_colour
  )+
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
easy_contact_GP_barchart
#save_plot_with_script_name(easy_contact_GP_barchart)


################################################################################
################################################################################
#Geographical variation 

HSCP_barchart <- make_barchart_multiple_groups(
  data = easy_contact_HSCP ,
  x_var = Percentage,
  y_var = reorder(Area, Percentage),
  title = "Percentage of positive ratings for ease of contact, by HSCP",
  x_lab = "Percentage (%)",
  y_lab = ""
  )+
  geom_col(fill = access_colour)+
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
#save_plot_with_script_name(HSCP_barchart, height = 12)

################################################################################
##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
easy_contact_scotland_by_sex <- Sex %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`Sex`)

easy_contact_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_sex %>% 
    filter(Sex != "Scotland Total"),
  x_var = Percentage,
  y_var = reorder(Sex,Percentage),
  title = "Percentage of positive ratings for ease of contact, by sex",
  x_lab = "Percentage (%)",
  y_lab = "Sex", 
  bar_colour = access_colour
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
easy_contact_scotland_by_sex_barchart
#save_plot_with_script_name(easy_contact_scotland_by_sex_barchart)

## Barchart of Scotland average answer by Age band #
easy_contact_scotland_by_age <- `Age Band` %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`Age Band`)

easy_contact_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_age,
  x_var = Percentage,
  y_var = `Age Band`,
  title = "Percentage of positive ratings for ease of contact, by age",
  x_lab = "Percentage (%)", 
  y_lab = "Age Band",
  bar_colour = access_colour
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

  
easy_contact_scotland_by_age_barchart
#save_plot_with_script_name(easy_contact_scotland_by_age_barchart)


# Barchart of Scotland average answer by SIMD #
easy_contact_scotland_by_SIMD <- SIMD %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive",
  ) %>%
  group_by(`Scottish Index of Multiple Deprivation Decile`)

easy_contact_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_SIMD,
  x_var = Percentage,
  y_var = reorder(
    `Scottish Index of Multiple Deprivation Decile`,
    11-as.numeric(sub("^([0-9]+).*", "\\1",
                   `Scottish Index of Multiple Deprivation Decile`))
  ),
  title = "Percentage of positive ratings for ease of contact, by SIMD",
  x_lab = "Percentage (%)", 
  y_lab = "SIMD decile",
  bar_colour = access_colour
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

easy_contact_scotland_by_SIMD_barchart
#save_plot_with_script_name(easy_contact_scotland_by_SIMD_barchart)


# Barchart of Scotland average answer by Urban 8 #
easy_contact_scotland_by_urban <- `Urban-Rural 8` %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  ) %>%
  group_by(`Urban-Rural 8-fold classification`)

easy_contact_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_urban,
  x_var = Percentage,
  y_var = factor(
    `Urban-Rural 8-fold classification`,
    levels = rev(sort(unique(`Urban-Rural 8-fold classification`)))
  ),
  title = "Percentage of positive ratings for ease of contact, by Urban-Rural 8",
  x_lab = "Percentage (%)", 
  y_lab = "",
  bar_colour = access_colour
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
easy_contact_scotland_by_urban_barchart
#save_plot_with_script_name(easy_contact_scotland_by_urban_barchart)


##----------------------------------------------------------------------------#
##Barchart by Chronic Pain ##
easy_contact_scotland_by_chronic_pain <- `Chronic Pain` %>% 
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive",
    `By Question Response Option`!= "Skipped Q42"
  ) %>%
  group_by(`By Question Response Option`)

easy_contact_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_chronic_pain, 
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = "Percentage of positive ratings for ease of contact, by Chronic pain",
  x_lab = "Percentage (%)",
  y_lab = "Experienced chronic pain?", 
  bar_colour = access_colour
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

easy_contact_scotland_by_chronic_pain_barchart
#save_plot_with_script_name(easy_contact_scotland_by_chronic_pain_barchart)


##  Barchart by Long term condition ##
easy_contact_scotland_by_long_term <- `Long-Term Condition` %>% 
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive",
    `By Question Response Option`!= "Skipped Question"
  ) %>%
  group_by(`By Question Response Option`)


easy_contact_scotland_by_long_term_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_long_term,
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = "Percentage of positive ratings for ease of contact, by long term condition",
  x_lab = "Percentage (%)",
  y_lab = "Long term condition?", 
  bar_colour = access_colour,
  bar_width = 0.75
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
    limits = c(0,100)
  )+
  theme(
    axis.title.y = element_text(
      hjust = 0.9,
      size = 11
    ))

easy_contact_scotland_by_long_term_barchart
#save_plot_with_script_name(easy_contact_scotland_by_long_term_barchart)

## Barchart  by Sexual Orientation ##
easy_contact_scotland_by_sexual_orientation <- `Sexual Orientation` %>% 
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive",
    `By Question Response Option` != "Skipped Q43"
  )%>%
  group_by(`By Question Response Option`)

easy_contact_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_sexual_orientation,
  x_var = Percentage,
  y_var = reorder(`By Question Response Option`, Percentage),
  title = "Percentage of positive ratings for ease of contact, by sexual orientation",
  x_lab = "Percentage (%)", 
  y_lab = "Sexual Orientation",
  bar_colour = access_colour
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

easy_contact_scotland_by_sexual_orientation_barchart
#save_plot_with_script_name(easy_contact_scotland_by_sexual_orientation_barchart)

## # Barchart by Ethnicity ##
easy_contact_scotland_by_ethnicity <- Ethnicity %>% 
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive",
    `By Question Response Option`!= "Skipped Q44"
  ) %>%
  group_by(`By Question Response Option`)

easy_contact_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_ethnicity,
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = "Percentage of positive ratings for ease of contact, by ethnicity",
  x_lab = "What is your ethnic group?", 
  y_lab = "Percentage (%)",
  bar_colour = access_colour
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

easy_contact_scotland_by_ethnicity_barchart
#save_plot_with_script_name(easy_contact_scotland_by_ethnicity_barchart)

###############################################################################
## Comparing to the last surveys results at Scotland level ##
## Cleaning 2022 results
easy_contact_scotland_2022 <- `Scotland - PNN Questions` %>% 
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
    "Year"= "2021-22",
    Percentage = as.numeric(as.character(Percentage))
  )

## Cleaning 2024 results
easy_contact_scotland_2024 <- `Positive, Neutral or Negative` %>% 
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
    "Year"= "2023-24",
    Percentage = as.numeric(as.character(Percentage))
  ) %>% 
  mutate(Percentage = Percentage*100)

easy_contact_scotland_2026 <- Scotland %>%
  filter(
    `Question Number` == "q03",
    `Response Option`=="positive"
  ) %>% 
  mutate(
    "Year"="2025-26"
  )%>% 
  select(-c("Topic", "Lower 95% Confidence Interval", "Upper 95% Confidence Interval"))

easy_contact_scotland_timeseries <- bind_rows(
  easy_contact_scotland_2022,
  easy_contact_scotland_2024,
  easy_contact_scotland_2026,
  # 2019 & 2017 row
  tibble(
    `Question Number` = rep("03",2),
    `Question Text` = rep("How easy is it for you to contact your GP practice in the way that you want?",2),
    `Number of Responses` = c(140969,118064),
    `Response Option` = rep("positive",2),
    `Percentage` = c(85, #2019
                     87), #2017
    `Year` = c("2019-20","2017-18")
  )) %>%
  mutate(`Response Option` = as.factor(`Response Option`))

glimpse(easy_contact_scotland_timeseries)

easy_contact_scotland_timeseries_scatter <- make_scatter(
  data = easy_contact_scotland_timeseries,
  x_var = Year,
  y_var = Percentage,
  title = "Timeseries of the percentage of positive ratings for ease of contacting GP",
  y_lab = "Percentage (%)",
  x_lab = "Year"
  )+
  geom_line(colour = access_colour,
            aes(group = 2), 
            linewidth = 2, ) +
  geom_point(size = 4, colour = access_colour)+
  geom_text(
    aes(
      label = paste0(round(Percentage, 0), "%")),
    vjust = -0.9,
    size = 4
  )+
  scale_y_continuous(limits = c(0,100))
easy_contact_scotland_timeseries_scatter
#save_plot_with_script_name(easy_contact_scotland_timeseries_scatter)

#------------------------## Ease of contacting GP variation analysis ##--------------------------#
easy_contact_variation_by_GP <- variation_data_2025 %>%
  filter(
    `Question Number` == "q03",
    `Response Option` =="positive"
  )

easy_contact_variation_by_hscp <- easy_contact_variation_by_GP %>%
  group_by(hscp_name) %>%
  summarise(
    num_practices = n(),
    num_clusters = n_distinct(hscp_gpcl_name),
    sd_pct = sd(Percentage, na.rm = TRUE),
    min_pct = min(Percentage, na.rm = TRUE),
    max_pct = max(Percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))

easy_contact_largest_hscp_by_clusters <- easy_contact_variation_by_hscp %>%
  arrange(desc(num_clusters), desc(num_practices)) %>%
  slice_head(n = 5)
## This will extract the chosen HSCP based on highest number of clusters and individual practices
easy_contact_chosen_hscp <- easy_contact_largest_hscp_by_clusters %>% 
  slice_head(n = 1) %>% 
  pull(hscp_name)

# Get summary stats for the chosen HSCP
easy_contact_variation_by_cluster <- easy_contact_variation_by_GP %>%
  filter(hscp_name == easy_contact_chosen_hscp) %>% 
  group_by(hscp_gpcl_name) %>% 
  summarise(
    num_practices = n(),
    sd_pct = sd(Percentage, na.rm = TRUE),
    min_pct = min(Percentage, na.rm = TRUE),
    max_pct = max(Percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))

# What clusters in the HSCP have the largest SD
top_sd <- easy_contact_variation_by_cluster %>%
  arrange(desc(sd_pct)) %>%
  slice_head(n = 4)
# What clusters in the HSCP have the largest range
top_range <- easy_contact_variation_by_cluster %>%
  filter(!hscp_gpcl_name %in% top_sd$hscp_gpcl_name) %>%
  arrange(desc(range_pct)) %>%
  slice_head(n = 2)

easy_contact_cluster_variation_tails <- bind_rows(
  sd5 = top_sd,
  range5 = top_range,
  .id = "Group"
  ) %>%
  pull(hscp_gpcl_name)
easy_contact_cluster_variation_tails

#Plot a combinition of the most varied clusters 
easy_contact_plot_data <- easy_contact_variation_by_GP %>%
  ungroup() %>% 
  filter(
    hscp_name == easy_contact_chosen_hscp,
    hscp_gpcl_name %in% easy_contact_cluster_variation_tails
  ) %>%
  mutate(
    cluster_label = paste(
      "GP Cluster",
      as.integer(factor(hscp_gpcl_name))
    )
  )

easy_contact_variation_by_GP_Cluster_and_HSCP_plot <- ggplot(
  easy_contact_variation_by_GP %>%
    filter(
      hscp_name == easy_contact_chosen_hscp,
      hscp_gpcl_name %in% easy_contact_cluster_variation_tails
    ) %>%
    mutate(
      cluster_label = paste(
        "GP Cluster",
        as.integer(factor(hscp_gpcl_name))
      )
    ),
  aes(
    x = cluster_label,
    y = Percentage,
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
    title = "Variation of the positive ease of contact ratings for individual GP Practices within a HSCP, grouped by GP Clusters",
    x = "",
    y = "Percentage of respondents rating their ease of contact positively (%)",
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
easy_contact_variation_by_GP_Cluster_and_HSCP_plot
#save_plot_with_script_name(plot, width = 29, height =15 ,show_title = TRUE)

#Save plots
save_plot_with_script_name(easy_contact_GP_barchart)
save_plot_with_script_name(HSCP_barchart, height = 12)
save_plot_with_script_name(easy_contact_scotland_by_sex_barchart)
save_plot_with_script_name(easy_contact_scotland_by_age_barchart)
save_plot_with_script_name(easy_contact_scotland_by_SIMD_barchart)
save_plot_with_script_name(easy_contact_scotland_by_urban_barchart)
save_plot_with_script_name(easy_contact_scotland_by_chronic_pain_barchart)
save_plot_with_script_name(easy_contact_scotland_by_long_term_barchart)
save_plot_with_script_name(easy_contact_scotland_by_sexual_orientation_barchart)
save_plot_with_script_name(easy_contact_scotland_by_ethnicity_barchart)
save_plot_with_script_name(easy_contact_scotland_timeseries_scatter)
save_plot_with_script_name(easy_contact_variation_by_GP_Cluster_and_HSCP_plot, width = 29, height =15 ,show_title = TRUE)
