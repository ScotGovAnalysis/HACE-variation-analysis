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
SCRIPT_NAME <- "GP - Overall care"
#------------------------------------------------------------------------------#
# Summary table showing the percentage of respondents who rated the overall care 
# from their General Practice as positive (“Excellent” or “Good”)
# For scotland
overall_care_scotland <- master_total %>%
  filter(
    Area == "Scotland",
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive"
  ) %>%
  pull(Percentage)
overall_care_scotland

# For individual GP practices
overall_care_GP <- master_total %>%
  filter(
    `Area Type` == "GP Practice",
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive"
  )

# For GP Clusters
overall_care_cluster <- master_total %>%
  filter(
    `Area Type` == "GP Cluster",
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive"
  )

# For Health and Social Care partnerships
overall_care_HSCP <- master_total %>%
  filter(
    `Area Type` == "Health and Social Care Partnership",
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive"
  )


# For Health Boards
overall_care_HB <- master_total %>%
  filter(
    `Area Type` == "Health Board",
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive"
  )

################################################################################
## Barchart of overall care for all practices across Scotland ------------------
overall_care_GP_binned <- overall_care_GP %>%
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

overall_care_GP_barchart <- make_barchart_multiple_groups(
  data = overall_care_GP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = "The number of GP practices scoring in each percentage band of positive ratings for overall care",
  x_lab = "Percentage (%)",
  y_lab = "Number of GP practices",
  bar_colour = quality_colour
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

overall_care_GP_barchart
# save_plot_with_script_name(overall_care_GP_barchart)

################################################################################
#Geographical variation 
overall_care_HSCP_barchart <- make_barchart_multiple_groups(
  data = overall_care_HSCP ,
  x_var = Percentage,
  y_var = reorder(Area, Percentage),
  title = "Percentage of positive ratings for overall care, by HSCP",
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
overall_care_HSCP_barchart
#save_plot_with_script_name(overall_care_HSCP_barchart, height = 12)


################################################################################
##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
overall_care_scotland_by_sex <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` =="positive",
    Sex != "Total"
  )

overall_care_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_sex,
  x_var = Percentage,
  y_var = reorder(Sex,Percentage),
  title = "Percentage of positive ratings for overall care, by sex",
  x_lab = "Percentage (%)",
  y_lab = "Sex", 
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
#save_plot_with_script_name(overall_care_scotland_by_sex_barchart)

## Barchart of Scotland average answer by Age band #
overall_care_scotland_by_age <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` =="positive",
    `Age Band`!= "Total"
  )

overall_care_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_age,
  x_var = Percentage,
  y_var = `Age Band`,
  title = "Percentage of positive ratings for overall care, by age",
  x_lab = "Percentage (%)", 
  y_lab = "Age Band",
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
#save_plot_with_script_name(overall_care_scotland_by_age_barchart)


# Barchart of Scotland average answer by SIMD #
overall_care_scotland_by_SIMD <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` =="positive",
    `SIMD`!= "Total"
  )

overall_care_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_SIMD,
  x_var = Percentage,
  y_var = reorder(
    `SIMD`,
    11-as.numeric(sub("^([0-9]+).*", "\\1",
                      `SIMD`))
  ),
  title = "Percentage of positive ratings for overall care, by SIMD",
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
#save_plot_with_script_name(overall_care_scotland_by_SIMD_barchart)


# Barchart of Scotland average answer by Urban 8 #
overall_care_scotland_by_urban <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` =="positive",
    `Urban-Rural 8`!= "Total"
  )

overall_care_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_urban,
  x_var = Percentage,
  y_var = factor(
    `Urban-Rural 8`,
    levels = rev(sort(unique(`Urban-Rural 8`)))
  ),
  title = "Percentage of positive ratings for overall care, by Urban-Rural 8",
  x_lab = "Percentage (%)", 
  y_lab = "",
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
overall_care_scotland_by_chronic_pain <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive",
    !(`Chronic Pain` %in% c("Total", "Skipped Q42"))
  )


overall_care_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_chronic_pain, 
  x_var = Percentage,
  y_var = `Chronic Pain`,
  title = "Percentage of positive ratings for overall care, by Chronic pain",
  x_lab = "Percentage (%)",
  y_lab = "Experienced chronic pain?", 
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
#save_plot_with_script_name(overall_care_scotland_by_chronic_pain_barchart)


##  Barchart by Long term condition ##
overall_care_scotland_by_long_term <- master_demographics %>% 
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive",
    !(`Long-Term Condition` %in% c("Total", "Skipped Question"))
  )



overall_care_scotland_by_long_term_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_long_term,
  x_var = Percentage,
  y_var = `Long-Term Condition`,
  title = "Percentage of positive ratings for overall care, by long term condition",
  x_lab = "Percentage (%)",
  y_lab = "Long term condition?", 
  bar_colour = quality_colour,
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
  )

overall_care_scotland_by_long_term_barchart
#save_plot_with_script_name(overall_care_scotland_by_long_term_barchart)

## Barchart  by Sexual Orientation ##
overall_care_scotland_by_sexual_orientation <- master_demographics %>% 
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive",
    !(`Sexual Orientation` %in% c("Total", "Skipped Q43"))
  )

overall_care_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_sexual_orientation,
  x_var = Percentage,
  y_var = reorder(`Sexual Orientation`, Percentage),
  title = "Percentage of positive ratings for overall care, by sexual orientation",
  x_lab = "Percentage (%)", 
  y_lab = "Sexual Orientation",
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
overall_care_scotland_by_ethnicity <- master_demographics %>% 
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive",
    !(Ethnicity %in% c("Total", "Skipped Q44"))
  )


overall_care_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = overall_care_scotland_by_ethnicity,
  x_var = Percentage,
  y_var = Ethnicity,
  title = "Percentage of positive ratings for overall care, by ethnicity",
  x_lab = "What is your ethnic group?", 
  y_lab = "Percentage (%)",
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
overall_care_scotland_timeseries <- master_data_all %>% 
  filter(
    Area == "Scotland",
    `Question Number` %in% question_lookup$overall_care,
    `Response Option` == "positive",
    Sex == "Total",
    `Age Band` == "Total",
    SIMD == "Total",
    `Urban-Rural 8` == "Total",
    `Long-Term Condition` == "Total",
    `Chronic Pain` == "Total",
    `Sexual Orientation` == "Total",
    Ethnicity == "Total"
  )


overall_care_scotland_timeseries_scatter <- make_scatter(
  data = overall_care_scotland_timeseries %>%
    distinct(Year, .keep_all = TRUE) %>%
    arrange(Year) %>%
    slice_tail(n = 5),
  x_var = Year,
  y_var = Percentage,
  title = "Timeseries of the percentage of positive ratings for overall care",
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
  )+
  scale_y_continuous(limits = c(0,100))

overall_care_scotland_timeseries_scatter
#save_plot_with_script_name(overall_care_scotland_timeseries_scatter)

#------------## Overall care variation by HSCP  analysis ##--------------------#
#This creates a list of the overall care % positive broken down by HSCP, cluster and practice name. 
overall_care_variation_by_GP <- variation_data %>%
  filter(
    `Question Number` == question_lookup$overall_care,
    `Response Option` =="positive"
  )

overall_care_variation_by_hscp <- overall_care_variation_by_GP %>%
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

## This will extract the chosen HSCP based on highest number of clusters and individual practices
overall_care_chosen_hscp <- overall_care_variation_by_hscp %>%
  arrange(desc(num_clusters), desc(num_practices)) %>%
  slice_head(n = 1) %>% 
  pull(hscp_name)
overall_care_chosen_hscp

# Get summary stats for the chosen HSCP
overall_care_variation_by_cluster <- overall_care_variation_by_GP %>%
  filter(hscp_name == overall_care_chosen_hscp) %>% 
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

# What clusters in the HSCP have the largest range
top_range <- overall_care_variation_by_cluster %>%
  arrange(desc(range_pct)) %>%
  slice_head(n = 5)

overall_care_cluster_variation_tails <- top_range %>% 
  pull(hscp_gpcl_name)
overall_care_cluster_variation_tails

#Plot a combinition of the most varied clusters 
overall_care_variation_by_GP_Cluster_and_HSCP_plot_data <- overall_care_variation_by_GP %>%
  ungroup() %>% 
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

overall_care_variation_by_GP_Cluster_and_HSCP_plot <-
  make_jitter_plot(
    data = overall_care_variation_by_GP_Cluster_and_HSCP_plot_data %>%
      filter(
        hscp_name == overall_care_chosen_hscp,
        hscp_gpcl_name %in% overall_care_cluster_variation_tails
      ) %>%
      mutate(
        cluster_label = paste(
          "GP Cluster",
          as.integer(factor(hscp_gpcl_name))
        )
      ),
    x_var = cluster_label,
    y_var = Percentage,
    colour_var = cluster_label,
    shape_var = cluster_label,
    title = "Variation of the positive overall care rating for practices within a HSCP, grouped by GP Clusters",
    x_lab = "",
    y_lab = "Percentage of respondents rating their overall care positively (%)"
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 18, 3, 4, 8, 7, 9, 10)
  ) +
  labs(
    caption = paste(
      "Note: Each point represents an individual GP practice.",
      "Different colours and shapes are used to distinguish GP clusters.",
      "All GP clusters included in this chart belong to the same HSCP."
    )
  ) +
  theme(
    plot.caption = element_text(
      hjust = 0,
      size = 12
    )
  )
overall_care_variation_by_GP_Cluster_and_HSCP_plot
#save_plot_with_script_name(overall_care_variation_by_GP_Cluster_and_HSCP_plot, width = 29,height =15 ,show_title = TRUE)

################################################################################
SCRIPT_NAME <- "GP - Overall care"
# Save plots
save_plot_with_script_name(overall_care_GP_barchart, width = 26, height =13)
save_plot_with_script_name(overall_care_HSCP_barchart)
save_plot_with_script_name(overall_care_scotland_by_sex_barchart)
save_plot_with_script_name(overall_care_scotland_by_age_barchart)
save_plot_with_script_name(overall_care_scotland_by_SIMD_barchart)
save_plot_with_script_name(overall_care_scotland_by_urban_barchart)
save_plot_with_script_name(overall_care_scotland_by_chronic_pain_barchart)
save_plot_with_script_name(overall_care_scotland_by_long_term_barchart)
save_plot_with_script_name(overall_care_scotland_by_sexual_orientation_barchart)
save_plot_with_script_name(overall_care_scotland_by_ethnicity_barchart)
save_plot_with_script_name(overall_care_scotland_timeseries_scatter)
save_plot_with_script_name(overall_care_variation_by_GP_Cluster_and_HSCP_plot, width = 29, height =15)

