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
SCRIPT_NAME <- "GP - Ease of contacting"
#------------------------------------------------------------------------------#
#The percentage of respondents who rated the Ease of contacting their General 
# Practice as positive for Scotland for current survey year
easy_contact_scotland <- master_total %>%
  filter(
    Area == "Scotland",
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` == "positive"
  ) %>%
  pull(Percentage)
easy_contact_scotland

# For individual GP practices
easy_contact_GP <- master_total %>%
  filter(
    `Area Type` == "GP Practice",
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` == "positive"
  )

# For GP Clusters
easy_contact_cluster <- master_total %>%
  filter(
    `Area Type` == "GP Cluster",
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` == "positive"
  )

# For Health and Social Care partnerships
easy_contact_HSCP <- master_total %>%
  filter(
    `Area Type` == "Health and Social Care Partnership",
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` == "positive"
  )


# For Health Boards
easy_contact_HB <- master_total %>%
  filter(
    `Area Type` == "Health Board",
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` == "positive"
  )

################################################################################
## Barchart of urgent access national level ------------------------------------
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
# save_plot_with_script_name(easy_contact_GP_barchart)


################################################################################
################################################################################
#Geographical variation 
easy_contact_HSCP_barchart <- make_barchart_multiple_groups(
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
easy_contact_HSCP_barchart
#save_plot_with_script_name(easy_contact_HSCP_barchart, height = 12)

################################################################################
##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
easy_contact_scotland_by_sex <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` =="positive",
    Sex != "Total"
  )

easy_contact_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_sex,
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
easy_contact_scotland_by_age <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` =="positive",
    `Age Band`!= "Total"
  )

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
easy_contact_scotland_by_SIMD <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` =="positive",
    `SIMD`!= "Total"
  )

easy_contact_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_SIMD,
  x_var = Percentage,
  y_var = reorder(
    `SIMD`,
    11-as.numeric(sub("^([0-9]+).*", "\\1",
                   `SIMD`))
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
easy_contact_scotland_by_urban <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` =="positive",
    `Urban-Rural 8`!= "Total"
  )

easy_contact_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_urban,
  x_var = Percentage,
  y_var = factor(
    `Urban-Rural 8`,
    levels = rev(sort(unique(`Urban-Rural 8`)))
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
easy_contact_scotland_by_chronic_pain <- master_demographics %>%
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` == "positive",
    !(`Chronic Pain` %in% c("Total", "Skipped Q42"))
  )


easy_contact_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_chronic_pain, 
  x_var = Percentage,
  y_var = `Chronic Pain`,
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
easy_contact_scotland_by_long_term <- master_demographics %>% 
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` == "positive",
    !(`Long-Term Condition` %in% c("Total", "Skipped Question"))
  )



easy_contact_scotland_by_long_term_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_long_term,
  x_var = Percentage,
  y_var = `Long-Term Condition`,
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
  )

easy_contact_scotland_by_long_term_barchart
#save_plot_with_script_name(easy_contact_scotland_by_long_term_barchart)

## Barchart  by Sexual Orientation ##
easy_contact_scotland_by_sexual_orientation <- master_demographics %>% 
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` == "positive",
    !(`Sexual Orientation` %in% c("Total", "Skipped Q43"))
  )

easy_contact_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_sexual_orientation,
  x_var = Percentage,
  y_var = reorder(`Sexual Orientation`, Percentage),
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
easy_contact_scotland_by_ethnicity <- master_demographics %>% 
  filter(
    Year == survey_year,
    `Question Number` %in% question_lookup$easy_contact,
    `Response Option` == "positive",
    !(Ethnicity %in% c("Total", "Skipped Q44"))
  )


easy_contact_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = easy_contact_scotland_by_ethnicity,
  x_var = Percentage,
  y_var = Ethnicity,
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
easy_contact_scotland_timeseries <- master_data_all %>% 
  filter(
    Area == "Scotland",
    `Question Number` %in% question_lookup$easy_contact,
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


easy_contact_scotland_timeseries_scatter <- make_scatter(
  data = easy_contact_scotland_timeseries %>%
    distinct(Year, .keep_all = TRUE) %>%
    arrange(Year) %>%
    slice_tail(n = 5),
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
easy_contact_variation_by_GP <- variation_data %>%
  filter(
    `Question Number` == question_lookup$easy_contact,
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

## This will extract the chosen HSCP based on highest number of clusters and individual practices
easy_contact_chosen_hscp <- easy_contact_variation_by_hscp %>%
  arrange(desc(num_clusters), desc(num_practices)) %>%
  slice_head(n = 1) %>% 
  pull(hscp_name)
easy_contact_chosen_hscp

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

# What clusters in the HSCP have the largest range
top_range <- easy_contact_variation_by_cluster %>%
  arrange(desc(range_pct)) %>%
  slice_head(n = 5)

easy_contact_cluster_variation_tails <- top_range %>% 
  pull(hscp_gpcl_name)
easy_contact_cluster_variation_tails

#Plot a combinition of the most varied clusters 
easy_contact_variation_by_GP_Cluster_and_HSCP_plot_data <- easy_contact_variation_by_GP %>%
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

easy_contact_variation_by_GP_Cluster_and_HSCP_plot <-
  make_jitter_plot(
    data = easy_contact_variation_by_GP_Cluster_and_HSCP_plot_data %>%
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
    x_var = cluster_label,
    y_var = Percentage,
    colour_var = cluster_label,
    shape_var = cluster_label,
    title = "Variation of the positive ease of contact rating for practices within a HSCP, grouped by GP Clusters",
    x_lab = "",
    y_lab = "Percentage of respondents rating their ease of contact positively (%)"
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
easy_contact_variation_by_GP_Cluster_and_HSCP_plot
#save_plot_with_script_name(easy_contact_variation_by_GP_Cluster_and_HSCP_plot, width = 29, height =15 ,show_title = TRUE)
SCRIPT_NAME <- "GP - Ease of contacting"
#Save plots
save_plot_with_script_name(easy_contact_GP_barchart, width = 26, height =13)
save_plot_with_script_name(easy_contact_HSCP_barchart)
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
