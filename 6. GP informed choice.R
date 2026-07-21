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
  arrange(Percentage) %>%
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
  geom_col(fill = "#D071A7")+
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
#Geographical variation 

HSCP_barchart <- make_barchart_multiple_groups(
  data = informed_choice_HSCP ,
  x_var = Percentage,
  y_var = reorder(Area, Percentage),
  title = "Percentage by HSCP",
  x_lab = "Percentage (%)",
  y_lab = ""
)+
  geom_col(fill = "#D071A7")+
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


################################################################################
##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
informed_choice_scotland_by_sex <- Sex %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`Sex`)

informed_choice_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_sex,
  x_var = Percentage,
  y_var = reorder(Sex, Percentage),
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by sex",
    width = 60
  ), 
  x_lab = "Sex", 
  y_lab = "Percentage (%)",
  bar_colour = "#D071A7"
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
informed_choice_scotland_by_sex_barchart
save_plot_with_script_name(informed_choice_scotland_by_sex_barchart)

## Barchart of Scotland average answer by Age band #
informed_choice_scotland_by_age <- `Age Band` %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`Age Band`)

informed_choice_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_age,
  x_var = Percentage,
  y_var = `Age Band`,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by age",
    width = 60
  ), 
  x_lab = "Percentage (%)",
  y_lab = "Age Band",
  bar_width = 0.75,
  bar_colour = "#D071A7"
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
  
informed_choice_scotland_by_age_barchart
save_plot_with_script_name(informed_choice_scotland_by_age_barchart)

# Barchart of Scotland average answer by SIMD #
informed_choice_scotland_by_SIMD <- SIMD %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`Scottish Index of Multiple Deprivation Decile`) 

informed_choice_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_SIMD,
  x_var = Percentage,
  y_var = reorder(
    `Scottish Index of Multiple Deprivation Decile`,
    11-as.numeric(sub("^([0-9]+).*", "\\1",
                   `Scottish Index of Multiple Deprivation Decile`))
  ),
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by SIMD",
    width = 60
  ), 
  x_lab = "Percentage (%)", 
  y_lab = "SIMD decile",
  bar_colour = "#D071A7"
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
informed_choice_scotland_by_SIMD_barchart
save_plot_with_script_name(informed_choice_scotland_by_SIMD_barchart)

# Barchart of Scotland average answer by Urban 8 #
informed_choice_scotland_by_urban <- `Urban-Rural 8` %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(`Urban-Rural 8-fold classification`)

informed_choice_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_urban,
  x_var = Percentage,
  y_var = factor(
    `Urban-Rural 8-fold classification`,
    levels = rev(sort(unique(`Urban-Rural 8-fold classification`)))
  ),
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by Urban-Rural 8",
    width = 60
  ), 
  x_lab = "Percentage (%)",
  y_lab = "", 
  bar_colour = "#D071A7"
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
informed_choice_scotland_by_urban_barchart
save_plot_with_script_name(informed_choice_scotland_by_urban_barchart)

##----------------------------------------------------------------------------#
##Barchart by Chronic Pain ##
informed_choice_scotland_by_chronic_pain <- `Chronic Pain` %>% 
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive",
    `By Question Response Option` != "Skipped Q42"
  ) %>%
  group_by(`By Question Response Option`)

informed_choice_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_chronic_pain,
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by Chronic pain",
    width = 60
  ), 
  x_lab = "Percentage (%)",
  y_lab = "Chronic pain?", 
  bar_colour = "#D071A7"
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
    limits = c(0,100))


informed_choice_scotland_by_chronic_pain_barchart
save_plot_with_script_name(informed_choice_scotland_by_chronic_pain_barchart, width = 15.46, height = 3.77, show_title = FALSE)

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
    `Response Option` =="positive",
    `By Question Response Option`!= "Skipped Q43"
  ) %>%
  group_by(`By Question Response Option`) %>%
  mutate(
    `By Question Response Option` =
      forcats::fct_reorder(
        `By Question Response Option`,
        Percentage,
        .desc = TRUE
      )
  )


informed_choice_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_sexual_orientation,
  x_var = Percentage,
  y_var = reorder(`By Question Response Option`,Percentage),
  title = str_wrap(
    "The percentage responding positively to, 'I felt able to  make an informed choice about my treatment and care' by sexual orientation",
    width = 60
  ), 
  x_lab = "Percentage (%)", 
  y_lab = "Sexual orientation",
  bar_colour = "#D071A7"
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

informed_choice_scotland_by_sexual_orientation_barchart
save_plot_with_script_name(informed_choice_scotland_by_sexual_orientation_barchart)

## # Barchart by Ethnicity ##
informed_choice_scotland_by_ethnicity <- Ethnicity %>% 
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive",
    `By Question Response Option`!= "Skipped Q44"
  ) %>%
  group_by(`By Question Response Option`)

informed_choice_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_by_ethnicity,
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = str_wrap(
    "The percentage of respondents responding positively to, 'I felt able to  make an informed choice about my treatment and care' by ethnicity",
    width = 60
  ), 
  y_lab = "Ethnic group", 
  x_lab = "Percentage (%)", 
  bar_colour = "#D071A7"
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
    limits = c(0,100))

informed_choice_scotland_by_ethnicity_barchart
save_plot_with_script_name(informed_choice_scotland_by_ethnicity_barchart, width = 15.46, height = 3.77, show_title = FALSE)

###############################################################################
## Comparing to the last surveys results at Scotland level ##
## Cleaning 2021 results
informed_choice_scotland_2022 <- `Scotland - PNN Questions` %>% 
  filter(
    `Question Number` == "13l"
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
informed_choice_scotland_2024 <- `Positive, Neutral or Negative` %>% 
  filter(
    `Geography Type` == "Scotland",
    `Question Number` == "q16m"
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

informed_choice_scotland_2026 <- Scotland %>%
  filter(
    `Question Number` == "q16m",
    `Response Option`=="positive"
  ) %>% 
  mutate(
    "Year"="2025-26"
  )%>% 
  select(-c("Topic", "Lower 95% Confidence Interval", "Upper 95% Confidence Interval"))

informed_choice_scotland_timeseries <- bind_rows(
  informed_choice_scotland_2022,
  informed_choice_scotland_2024,
  informed_choice_scotland_2026,
  ) %>%
  mutate(`Response Option` = as.factor(`Response Option`))

glimpse(informed_choice_scotland_timeseries)

informed_choice_scotland_timeseries_barchart <- make_barchart_multiple_groups(
  data = informed_choice_scotland_timeseries,
  x_var = Year, 
  y_var = Percentage,
  title = "Timeseries of informed choice rated positive",
  x_lab = "Year",
  y_lab = "Percentage (%)"
)+
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  )+
  geom_col(
    fill = "#D071A7"
  )+
  geom_text(
    aes(label = paste0(round(Percentage, 0), "%")),
    vjust = 2,
    size = 3,
    colour = "black"
  )
informed_choice_scotland_timeseries_barchart
save_plot_with_script_name(informed_choice_scotland_timeseries_barchart)

informed_choice_scotland_timeseries_scatter <- make_scatter(
  data = informed_choice_scotland_timeseries,
  x_var = Year,
  y_var = Percentage,
  title = "Timeseries of informed choice rated positive",
  y_lab = "Percentage (%)",
  x_lab = "Year"
  )+
  geom_line(
    colour = "#D071A7",
    aes(group = 2), 
    linewidth = 2, 
    ) +
  geom_point(size = 4, colour = "#D071A7")+
  geom_text(
    aes(
      label = paste0(round(Percentage, 0), "%")),
    vjust = -0.9,
    size = 4
  )
informed_choice_scotland_timeseries_scatter
save_plot_with_script_name(informed_choice_scotland_timeseries_scatter)




################################################################################
#------------------## informed choice variation analysis ##--------------------#
informed_choice_variation_by_GP <- variation_data_2025 %>%
  filter(
    `Question Number` == "q16m",
    `Response Option` =="positive"
  ) %>%
  group_by(hscp_name, `GP Practice name`) %>%
  summarise(
    informed_choice_percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

informed_choice_variation_by_hscp <- informed_choice_variation_by_GP %>%
  group_by(hscp_name) %>%
  summarise(
    num_practices = n(),
    sd_pct = sd(informed_choice_percentage, na.rm = TRUE),
    min_pct = min(informed_choice_percentage, na.rm = TRUE),
    max_pct = max(informed_choice_percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))


informed_choice_variation_tails <- bind_rows(
  Top5 = informed_choice_variation_by_hscp %>% slice_head(n = 5),
  Bottom5 = informed_choice_variation_by_hscp %>% slice_tail(n = 5),
  .id = "Group"
) %>%
  pull(hscp_name)


ggplot(
  informed_choice_variation_by_GP %>%
    filter(hscp_name %in% informed_choice_variation_tails),
  aes(x = reorder(hscp_name, informed_choice_percentage),
      y = informed_choice_percentage)
) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  coord_flip() +
  labs(
    title = "Informed choice: Top 5 and Bottom 5 HSCPs by variation",
    x = "HSCP",
    y = "% positive"
  ) +
  theme_minimal()


chosen_informed_choice_variation_by_hscp <- informed_choice_variation_by_GP %>%
  filter(
    hscp_name %in% c("North Lanarkshire","Aberdeenshire","Edinburgh","Orkney Islands")
  )


chosen_informed_choice_variation_by_hscp_plot <- ggplot(
  chosen_informed_choice_variation_by_hscp, 
  aes(x = reorder(hscp_name, informed_choice_percentage), y = informed_choice_percentage)) +
  geom_jitter(
    aes(colour = hscp_name),
    width = 0.4, height = 0,
    size = 3, alpha = 0.75
  ) +
  scale_y_continuous(limits = c(0,100))+
  labs(
    title = "% positive they could make an informed choice ",
    x = "HSCP",
    y = "% positive"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0)
  )
chosen_informed_choice_variation_by_hscp_plot
save_plot_with_script_name(chosen_informed_choice_variation_by_hscp_plot)
