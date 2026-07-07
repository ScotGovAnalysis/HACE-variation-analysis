### Analysis for HACE Q24
# June 2026

## HSC vision portfolio indicators 
# A&E/GP Out of Hours – treatment

## What it measures 
#Percentage of people responding 'I was treated with compassion and understanding'
# during A&E or GP Out of Hours care.

## HACE Question (Q24)
# Q24 How much would you agree or disagree with the following statements about 
# your experience?


## Core value
# Person-centered – delivering outcomes that matter to people

#------------------------------------------------------------------------------#
# Set SGplot for default chart colours
sgplot::use_sgplot()
#Source function to save plots from utility script
source("1. Utility.R")


OOH_care_scotland <- Scotland %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive") %>%
  summarise(
    percentage_OOH_care_scotland = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pull(percentage_OOH_care_scotland)

# For individual GP practices
OOH_care_GP <- `GP Practice` %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive") %>%
  group_by(`GP Practice name`) %>%
  summarise(
    percentage_OOH_care_GP = sum(Percentage, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(percentage_OOH_care_GP) %>%
  mutate(order = row_number())

# For GP Clusters
OOH_care_cluster <- `GP Cluster` %>%
  {na_n <- sum(is.na(.$Percentage))
  
  if (na_n > 0) {
    message(
      "#############################################
## NOTE: Redacted responses (NAs) removed ###
#############################################")
  }
  
  .
  } %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive",
    !is.na(Percentage)) %>%
  group_by(`Area`)

# For Health and Social Care partnerships
OOH_care_HSCP <- HSCP %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_OOH_care_HSCP = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_OOH_care_HSCP) %>%
  mutate(order = row_number())

# For Health Board
OOH_care_HB <- `Health Board` %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive") %>%
  group_by(`Area`) %>%
  summarise(percentage_OOH_care_HB = sum(Percentage, na.rm = TRUE), 
            .groups = "drop") %>%
  arrange(percentage_OOH_care_HB) %>%
  mutate(order = row_number())
################################################################################
## Barchart of urgent access national level ------------------------------------
bands <- paste0(
  seq(0, 95, 5),
  "-",
  seq(5, 100, 5))

OOH_care_cluster_binned <- OOH_care_cluster %>%
  ungroup() %>%
  mutate(
    pct_band = cut(
      Percentage,
      breaks = seq(0, 100, 5),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_clusters") %>%
  complete(
    pct_band = factor(bands, levels = bands),
    fill = list(n_clusters = 0)
  )


OOH_care_cluster_barchart <- make_barchart_multiple_groups(
  data = OOH_care_cluster_binned,
  x_var = pct_band,
  y_var = n_clusters,
  title = str_wrap(
    "% responding positively to 'I was treated with compassion and understanding' during A&E or GP OOH care by GP cluster",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Number of GP clusters",
  bar_colour = "#801650"
)+
geom_text(
aes(
  label = round(n_clusters, 0),
  colour = n_clusters < 25,
  vjust = ifelse(n_clusters < 25, -0.5, 2)
),
size = 4,
show.legend = FALSE
) +
scale_colour_manual(
values = c("FALSE" = "white",
           "TRUE" = "black")
)

OOH_care_cluster_barchart
save_plot_with_script_name(OOH_care_cluster_barchart)

# By HSCP
OOH_care_HSCP_binned <- OOH_care_HSCP %>%
  mutate(
    pct_band = cut(
      percentage_OOH_care_HSCP,
      breaks = seq(0, 100, by = 10),
      labels = bands,
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(pct_band, name = "n_practices") %>%
  complete(pct_band = bands, fill = list(n_practices = 0)) %>%
  mutate(pct_band = factor(pct_band, levels = bands))

scotland_y <- OOH_care_HSCP_binned %>%
  filter(pct_band == scotland_band) %>%
  pull(n_practices)


OOH_care_HSCP_barchart <- make_barchart_multiple_groups(
  data = OOH_care_HSCP_binned,
  x_var = pct_band,
  y_var = n_practices,
  title = str_wrap(
    "% responding positively to 'I was treated with compassion and understanding' during A and E or GP OOH care.' by GP HSCP",
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
    label = paste0("Scottish average ", round(OOH_care_scotland, 0), "%"),
    vjust = -0.8,
    colour = "red",
    fontface = "bold"
  )
OOH_care_HSCP_barchart
save_plot_with_script_name(OOH_care_HSCP_barchart)

################################################################################
##-----------------------------------------------------------------------------#
# Barchart of Scotland average answer by sex #
OOH_care_scotland_by_sex <- Sex %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive"
  ) %>%
  group_by(`Sex`)

OOH_care_scotland_by_sex_barchart <- make_barchart_multiple_groups(
  data = OOH_care_scotland_by_sex %>% 
    filter(Sex != "Scotland Total"),
  x_var = Percentage,
  y_var = Sex,
  title = str_wrap(
    "% responding positively to 'I was treated with compassion and understanding' during A and E or GP OOH care.' by sex",
    width = 60
  ), 
  x_lab = "Percentage (%)", 
  y_lab = "Sex",
  bar_colour = "#801650"
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
OOH_care_scotland_by_sex_barchart
save_plot_with_script_name(OOH_care_scotland_by_sex_barchart)

## Barchart of Scotland average answer by Age band #
OOH_care_scotland_by_age <- `Age Band` %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive"
  ) %>%
  group_by(`Age Band`)

OOH_care_scotland_by_age_barchart <- make_barchart_multiple_groups(
  data = OOH_care_scotland_by_age,
  x_var = Percentage,
  y_var = `Age Band`,
  title = str_wrap(
    "% responding positively to 'I was treated with compassion and understanding' during A and E or GP OOH care.' by age",
    width = 60
  ), 
  x_lab = "Percentage (%)", 
  y_lab = "Age Band",
  bar_colour = "#801650"
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

OOH_care_scotland_by_age_barchart
save_plot_with_script_name(OOH_care_scotland_by_age_barchart)

# Barchart of Scotland average answer by SIMD #
OOH_care_scotland_by_SIMD <- SIMD %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive"
  ) %>%
  group_by(`Scottish Index of Multiple Deprivation Decile`)

OOH_care_scotland_by_SIMD_barchart <- make_barchart_multiple_groups(
  data = OOH_care_scotland_by_SIMD,
  x_var = Percentage,
  y_var = reorder(
    `Scottish Index of Multiple Deprivation Decile`,
    11-as.numeric(sub("^([0-9]+).*", "\\1",
                   `Scottish Index of Multiple Deprivation Decile`))
  ),
  title = str_wrap(
    "% responding positively to 'I was treated with compassion and understanding' during A and E or GP OOH care.' by SIMD",
    width = 60
  ), 
  x_lab = "SIMD", 
  y_lab = "Percentage (%)",
  bar_colour = "#801650"
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
  scale_x_continuous(limits = c(0,100),
                     expand = c(0, 0))
OOH_care_scotland_by_SIMD_barchart
save_plot_with_script_name(OOH_care_scotland_by_SIMD_barchart)

# Barchart of Scotland average answer by Urban 8 #
OOH_care_scotland_by_urban <- `Urban-Rural 8` %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive"
  ) %>%
  group_by(`Urban-Rural 8-fold classification`)

OOH_care_scotland_by_urban_barchart <- make_barchart_multiple_groups(
  data = OOH_care_scotland_by_urban,
  x_var = Percentage, 
  y_var = factor(
    `Urban-Rural 8-fold classification`,
    levels = rev(sort(unique(`Urban-Rural 8-fold classification`)))
  ),
  title = str_wrap(
    "% responding positively to 'I was treated with compassion and understanding' during A and E or GP OOH care.' by Urban-Rural 8",
    width = 60
  ), 
  x_lab = "Urban-Rural 8-fold classification", 
  y_lab = "Percentage (%)", 
  bar_colour = "#801650"
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
OOH_care_scotland_by_urban_barchart
save_plot_with_script_name(OOH_care_scotland_by_urban_barchart)

##----------------------------------------------------------------------------#
##Barchart by Chronic Pain ##
OOH_care_scotland_by_chronic_pain <- `Chronic Pain` %>% 
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive",
    `By Question Response Option` != "Skipped Q42"
  ) %>%
  group_by(`By Question Response Option`)


OOH_care_scotland_by_chronic_pain_barchart <- make_barchart_multiple_groups(
  data = OOH_care_scotland_by_chronic_pain,
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = str_wrap(
    "% responding positively to 'I was treated with compassion and understanding' during A and E or GP OOH care.' by Chronic pain",
    width = 60
  ), 
  x_lab = "Percentage (%)", 
  y_lab = "Experienced chronic pain?",
  bar_colour = "#801650"
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

OOH_care_scotland_by_chronic_pain_barchart
save_plot_with_script_name(OOH_care_scotland_by_chronic_pain_barchart, width = 11,height = 5)

##  Barchart by Long term condition ##
OOH_care_scotland_by_long_term <- `Long-Term Condition` %>% 
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive",
    `By Question Response Option` != "Skipped Question"
  ) %>%
  group_by(`By Question Response Option`) 


OOH_care_scotland_by_long_term_barchart <- make_barchart_multiple_groups(
  data = OOH_care_scotland_by_long_term,
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = str_wrap(
    "% responding positively to 'I was treated with compassion and understanding' during A and E or GP OOH care.' by long term",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "Long term condition?",
  bar_colour = "#801650"
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

OOH_care_scotland_by_long_term_barchart
save_plot_with_script_name(OOH_care_scotland_by_long_term_barchart,width = 11,height = 5)

## Barchart  by Sexual Orientation ##
OOH_care_scotland_by_sexual_orientation <- `Sexual Orientation` %>% 
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive",
    `By Question Response Option` != "Skipped Q43"
  ) %>%
  group_by(`By Question Response Option`) 

OOH_care_scotland_by_sexual_orientation_barchart <- make_barchart_multiple_groups(
  data = OOH_care_scotland_by_sexual_orientation,
  x_var = Percentage, 
  y_var = reorder(`By Question Response Option`,Percentage),
  title = str_wrap(
    "% responding 'I was treated with compassion and understanding' during A and E or GP OOH care.' by sexual orientation",
    width = 60
  ), 
  x_lab = "Percentage (%)", 
  y_lab = "Sexual Orientation",
  bar_colour = "#801650"
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

OOH_care_scotland_by_sexual_orientation_barchart
save_plot_with_script_name(OOH_care_scotland_by_sexual_orientation_barchart)

## # Barchart by Ethnicity ##
OOH_care_scotland_by_ethnicity <- Ethnicity %>% 
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive",
    `By Question Response Option`!= "Skipped Q44"
  ) %>%
  group_by(`By Question Response Option`) 

OOH_care_scotland_by_ethnicity_barchart <- make_barchart_multiple_groups(
  data = OOH_care_scotland_by_ethnicity, 
  x_var = Percentage,
  y_var = `By Question Response Option`,
  title = str_wrap(
    "% responding 'I was treated with compassion and understanding' during A and E or GP OOH care.' by ethnicity",
    width = 60
  ),
  x_lab = "Percentage (%)",
  y_lab = "What is your ethnic group?", 
  bar_colour = "#801650"
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

OOH_care_scotland_by_ethnicity_barchart
save_plot_with_script_name(OOH_care_scotland_by_ethnicity_barchart, width = 11,height = 5)

###############################################################################
## Comparing to the last surveys results at Scotland level ##
## Cleaning 2021 results
OOH_care_scotland_2022 <- `Scotland - PNN Questions` %>% 
  filter(
    `Question Number` == "26c"
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
OOH_care_scotland_2024 <- `Positive, Neutral or Negative` %>% 
  filter(
    `Geography Type` == "Scotland",
    `Question Number` == "q24c"
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

OOH_care_scotland_2026 <- Scotland %>%
  filter(
    `Question Number` == "q24c",
    `Response Option`== "positive"
  ) %>% 
  mutate(
    "Year"="2025-26"
  )%>% 
  select(-c("Topic", "Lower 95% Confidence Interval", "Upper 95% Confidence Interval"))

OOH_care_scotland_timeseries <- bind_rows(
  OOH_care_scotland_2022,
  OOH_care_scotland_2024,
  OOH_care_scotland_2026,
  # 2019 row
  tibble(
    `Question Number` = rep("20",2),
    `Question Text` = rep(" I was treated with compassion and understanding",2),
    `Number of Responses` = c(22294,48975),
    `Response Option` = rep(c("positive"),2),
    `Percentage` = c(84, #2019
                     86),#2017
    `Year` = c("2019-20","2017-18")
  )
) %>%
  mutate(`Response Option` = as.factor(`Response Option`))

glimpse(OOH_care_scotland_timeseries)

OOH_care_scotland_timeseries_barchart <- make_barchart_multiple_groups(
  data = OOH_care_scotland_timeseries,
  x_var = Year, 
  y_var = Percentage,
  title = "Timeseries of out of hours care rated positive",
  x_lab = "Year",
  y_lab = "Percentage (%)"
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
OOH_care_scotland_timeseries_barchart  
save_plot_with_script_name(OOH_care_scotland_timeseries_barchart)

OOH_care_scotland_timeseries_scatter <- make_scatter(
  data = OOH_care_scotland_timeseries,
  x_var = Year,
  y_var = Percentage,
  title = "Timeseries of OOH care rated positive",
  y_lab = "Percentage (%)",
  x_lab = "Year"
  )+
  geom_line(
    colour = "#801650",
    aes(group = 2), 
    linewidth = 2, 
  ) +
  geom_point(size = 4, colour = "#801650")+
  geom_text(
    aes(
      label = paste0(round(Percentage, 0), "%")),
    vjust = -0.9,
    size = 4
  )
OOH_care_scotland_timeseries_scatter
save_plot_with_script_name(OOH_care_scotland_timeseries_scatter)


#------------------## Out of hours care variation analysis ##----------------------#
### COME back to as will need to merge lookup to a new dataset ###
SG_Practice_lookup_HSCP <- SG_Practice_lookup %>% 
  select(
    "hscp_name",
    "hb_name"
  ) %>% 
  distinct(hscp_name, .keep_all = TRUE)

OOH_variation_data_2025 <- `HSCP` %>% 
  left_join(
    SG_Practice_lookup_HSCP,
    by = c("Area" = "hscp_name")
  ) %>% 
  select(
    "Question Number","Topic","Question Text","Response Option","Area Type",
    "Area","hb_name",
    "Number of Responses","Percentage","Lower 95% Confidence Interval",
    "Upper 95% Confidence Interval"
  ) %>% 
  rename("hscp_name"="Area")

OOH_care_variation_by_hscp <- OOH_variation_data_2025 %>%
  filter(
    `Question Number` == "q24c",
    `Response Option` =="positive"
  ) %>%
  group_by(hb_name, hscp_name) %>%
  summarise(
    OOH_care_percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

OOH_care_variation_by_hb <- OOH_care_variation_by_hscp %>%
  group_by(hb_name) %>%
  summarise(
    num_practices = n(),
    sd_pct = sd(OOH_care_percentage, na.rm = TRUE),
    min_pct = min(OOH_care_percentage, na.rm = TRUE),
    max_pct = max(OOH_care_percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))


OOH_care_variation_tails <- bind_rows(
  Top5 = OOH_care_variation_by_hscp %>% slice_head(n = 5),
  Bottom5 = OOH_care_variation_by_hscp %>% slice_tail(n = 5),
  .id = "Group"
) %>%
  pull(hb_name)


ggplot(
  OOH_care_variation_by_hscp %>%
    filter(hb_name %in% OOH_care_variation_tails),
  aes(x = reorder(hb_name, OOH_care_percentage),
      y = OOH_care_percentage)
) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  coord_flip() +
  labs(
    title = "OOH_care: Top 5 and Bottom 5 HSCPs by variation",
    x = "HB",
    y = "% positive"
  ) +
  theme_minimal()


chosen_OOH_care_variation_by_hb <- OOH_care_variation_by_hscp %>%
  filter(
    hb_name %in% c("NHS Ayrshire and Arran","NHS Tayside","NHS Greater Glasgow and Clyde", "NHS Lothian")
  )


chosen_OOH_care_variation_by_hb_plot <- ggplot(
  chosen_OOH_care_variation_by_hb, 
  aes(x = reorder(hb_name, OOH_care_percentage), y = OOH_care_percentage)) +
  geom_jitter(
    aes(colour = hb_name),
    width = 0.25, height = 0,
    size = 3, alpha = 0.75
  ) +
  scale_y_continuous(limits = c(0,100))+
  labs(
    title = "% positive experience OOH care",
    x = "HB",
    y = "% positive"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0)
  )

chosen_OOH_care_variation_by_hb_plot
save_plot_with_script_name(chosen_OOH_care_variation_by_hb_plot)


##################################################################################
# #Calculating the number of redacted responses at each geography level
# `GP Practice` %>%
#   filter(
#     `Question Number` == "q24c",
#     `Response Option` =="positive") |> 
#   summarise(
#     na_n = sum(is.na(Percentage)),
#     total_n = n(),
#     na_pct = (na_n / total_n) * 100
#   )
# `GP Cluster` %>%
#   filter(
#     `Question Number` == "q24c",
#     `Response Option` =="positive") |> 
#   summarise(
#     na_n = sum(is.na(Percentage)),
#     total_n = n(),
#     na_pct = (na_n / total_n) * 100
#   )
# `Health Board` %>%
#   filter(
#     `Question Number` == "q24c",
#     `Response Option` =="positive") |> 
#   summarise(
#     na_n = sum(is.na(Percentage)),
#     total_n = n(),
#     na_pct = (na_n / total_n) * 100
#   )
# 
# #------------------------------------------------------------------------------#
# ## By Health board
# OOH_care_HB <- `Health Board` %>%
#   filter(
#     `Question Number` == "q24c",
#     `Response Option` =="positive") %>%
#   # Group the data by GP Practice so calculations are done per practice
#   group_by(`Area`) %>%
#   # For each GP practice, sum the percentages of the selected response options
#   summarise(
#     percentage_OOH_care_HB = sum(Percentage, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   arrange(percentage_OOH_care_HB) %>%
#   # Order practices by % urgent respondants positive, lowest to highest
#   mutate(order = row_number())
# 
# #Scatterplot by Health Board
# OOH_care_HB_scatterplot <- make_scatter(
#   data = OOH_care_HB,
#   x_var = order, 
#   y_var = percentage_OOH_care_HB,
#   title = str_wrap(
#     "The percentage of respondents responding positively to 'I was treated with compassion and understanding during A&E or GP Out of Hours care' by Health Board", 
#     width = 60
#   ), 
#   x_lab = "Health board",
#   y_lab = "Percentage (%) responding positively"
# )
# OOH_care_HB_scatterplot
# # Saves plot to working directory
# save_plot_with_script_name(OOH_care_HB_scatterplot)
# 
# #Boxplot by GP cluster
# OOH_care_HB_boxplot <- make_boxplot_single_group(
#   data = OOH_care_HB,
#   x_var = percentage_OOH_care_HB,
#   title = str_wrap(
#     "The percentage of respondents responding positively to 'I was treated with compassion and understanding during A&E or GP Out of Hours care' by Health Board", 
#     width = 60
#   ),
#   x_lab = "Percentage",
#   y_lab = "")
# OOH_care_HB_boxplot
# # Saves plot to working directory
# save_plot_with_script_name(OOH_care_HB_boxplot)
# 
# ## Histogram by GP Cluster ##
# OOH_care_HB_histogram <- make_histogram(
#   data = OOH_care_HB, 
#   x_var = percentage_OOH_care_HB, 
#   title = str_wrap(
#     "The percentage of respondents responding positively to 'I was treated with compassion and understanding during A&E or GP Out of Hours care' by Health Board", 
#     width = 60
#   ),
#   x_lab = "Percentage (%) responding positively",
#   y_lab = "Number of Health boards")
# OOH_care_HB_histogram
# # Save plot to working directory
# save_plot_with_script_name(OOH_care_HB_histogram)
# 
# #------------------------------------------------------------------------------#
# ## By GP Cluster 
# OOH_care_cluster <- `GP Cluster` %>%
#   {na_n <- sum(is.na(.$Percentage))
#     
#     if (na_n > 0) {
#       message(
#       "#############################################
# ## NOTE: Redacted responses (NAs) removed ###
# #############################################")
#     }
#     
#     .
#   } %>%
#   filter(
#     `Question Number` == "q24c",
#     `Response Option` == "positive",
#     !is.na(Percentage)
#   ) %>%
#   group_by(`Area`) %>%
#   summarise(
#     percentage_OOH_care_cluster = sum(Percentage, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   arrange(percentage_OOH_care_cluster) %>%
#   mutate(order = row_number())
# 
# #Scatterplot by GP cluster
# OOH_care_cluster_scatterplot <- make_scatter(
#   data = OOH_care_cluster,
#   x_var = order, 
#   y_var = percentage_OOH_care_cluster,
#   title = str_wrap(
#     "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A and E or GP Out of Hours care.' by GP cluster", 
#     width = 60
#   ), 
#   x_lab = "GP cluster",
#   y_lab = "Percentage (%) responding positively"
# )
# OOH_care_cluster_scatterplot
# # Saves plot to working directory
# save_plot_with_script_name(OOH_care_cluster_scatterplot)
# 
# #Boxplot by GP cluster
# OOH_care_cluster_boxplot <- make_boxplot_single_group(
#   data = OOH_care_cluster,
#   x_var = percentage_OOH_care_cluster,
#   title = str_wrap(
#     "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A and E or GP Out of Hours care' by GP cluster", 
#     width = 60
#   ),
#   x_lab = "Percentage",
#   y_lab = "")
# OOH_care_cluster_boxplot
# # Saves plot to working directory
# save_plot_with_script_name(OOH_care_cluster_boxplot)
# 
# ## Histogram by GP Cluster ##
# OOH_care_cluster_histogram <- make_histogram(
#   data = OOH_care_cluster, 
#   x_var = percentage_OOH_care_cluster, 
#   title = str_wrap(
#     "The percentage of respondents responding positively to 'I was treated with compassion and understanding' during A and E or GP Out of Hours care' by GP cluster", 
#     width = 60
#   ),
#   x_lab = "Percentage (%) responding positively",
#   y_lab = "Number of GP clusters")
# OOH_care_cluster_histogram
# # Save plot to working directory
# save_plot_with_script_name(OOH_care_cluster_histogram)
# 
# 
# #----- BY GP Practice - too many redacted answers to be insightful yet --------#
# # Summary table showing the Percentage of people responding 'I was treated with 
# # compassion and understanding' during A&E or GP Out of Hours care.
# # OOH_care_GP <- `GP Practice` %>%
# #   filter(
# #     `Question Number` == "q24c",
# #     `Response Option` == "positive") %>%
# #   # Group the data by GP Practice so calculations are done per practice
# #   group_by(`GP Practice name`) %>%
# #   # For each GP practice, sum the percentages of the selected response options
# #   summarise(
# #     percentage_OOH_care_GP = sum(Percentage, na.rm = TRUE),
# #     .groups = "drop"
# #   ) %>%
# #   arrange(percentage_OOH_care_GP) %>%
# #   # Order practices by % respondents positive, lowest to highest
# #   mutate(order = row_number())
# # 
# # #Scatteplot by GP practice
# # OOH_care_GP_scatterplot <- make_scatter(
# #   data = OOH_care_GP,
# #   x_var = order, 
# #   y_var = percentage_OOH_care_GP,
# #   title = str_wrap(
# #     "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A&E or GP Out of Hours care.' by GP practice", 
# #     width = 60
# #   ), 
# #   x_lab = "GP practice",
# #   y_lab = "Percentage (%) responding positively"
# # )
# # OOH_care_GP_scatterplot
# # # Saves plot to working directory
# # save_plot_with_script_name(OOH_care_GP_scatterplot)
# # 
# # # Box plot by GP practice
# # OOH_care_GP_boxplot <- make_boxplot_single_group(
# #   data = OOH_care_GP,
# #   x_var = percentage_OOH_care_GP, 
# #   title = str_wrap(
# #     "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A&E or GP Out of Hours care.' by GP practice", 
# #     width = 60
# #   ),
# #   x_lab = "Percentage (%)",
# #   y_lab = "")
# # OOH_care_GP_boxplot
# # # Save plot to working directory
# # save_plot_with_script_name(OOH_care_GP_boxplot)
# # 
# # ## Histogram by GP Practice ##
# # OOH_care_GP_histogram <- make_histogram(
# #   data = OOH_care_GP, 
# #   x_var = percentage_OOH_care_GP, 
# #   title = str_wrap(
# #     "The percentage of respondents responding positively to, 'I was treated with compassion and understanding' during A&E or GP Out of Hours care.' by GP practice", 
# #     width = 60
# #   ), 
# #   x_lab = "Percentage (%) responding positively",
# #   y_lab = "Number of GP Practices")
# # OOH_care_GP_histogram
# # # Save plot to working directory
# # save_plot_with_script_name(OOH_care_GP_histogram)
# 
# #------------------------------------------------------------------------------#