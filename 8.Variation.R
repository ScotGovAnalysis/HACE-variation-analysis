#------------------------------------------------------------------------------#
sgplot::use_sgplot()

#### Variation test ##
variation_data_2025 <- `GP Practice` %>% 
  left_join(
    SG_Practice_lookup,
    by = c("GP Practice Code" = "gp_prac_no")
    ) %>% 
  select(
    "Question Number","Topic","Question Text","Response Option","Area Type",
    "hscp_name","hscp_gpcl_name","hb_name","GP Practice name",
    "Number of Responses","Percentage","Lower 95% Confidence Interval",
     "Upper 95% Confidence Interval"
    )

#------------------------## Within 2 days analysis ##--------------------------#
within_2_days_variation_by_GP <- variation_data_2025 %>%
  filter(
    `Question Number` == "q10",
    `Response Option` %in% within_2_days_responses
  ) %>%
  group_by(hscp_name, `GP Practice name`) %>%
  summarise(
    within_2_days_percentage = sum(Percentage, na.rm = TRUE),
    .groups = "drop"
  )

within_2_days_variation_by_hscp <- within_2_days_variation_by_GP %>%
  group_by(hscp_name) %>%
  summarise(
    num_practices = n(),
    sd_pct = sd(within_2_days_percentage, na.rm = TRUE),
    min_pct = min(within_2_days_percentage, na.rm = TRUE),
    max_pct = max(within_2_days_percentage, na.rm = TRUE),
    range_pct = max_pct - min_pct,
    .groups = "drop"
  ) %>%
  arrange(desc(sd_pct))


within_2_days_variation_tails <- bind_rows(
  Top5 = within_2_days_variation_by_hscp %>% slice_head(n = 5),
  Bottom5 = within_2_days_variation_by_hscp %>% slice_tail(n = 5),
  .id = "Group"
) %>%
  pull(hscp_name)


ggplot(
  within_2_days_variation_by_GP %>%
    filter(hscp_name %in% within_2_days_variation_tails),
  aes(x = reorder(hscp_name, within_2_days_percentage),
      y = within_2_days_percentage)
) +
  geom_boxplot(outlier.shape = NA, fill = "lightgrey") +
  coord_flip() +
  labs(
    title = "Urgent access: Top 5 and Bottom 5 HSCPs by variation",
    x = "HSCP",
    y = "% within 2 days"
  ) +
  theme_minimal()

chosen_within_2_days_variation_by_hscp <- within_2_days_variation_by_GP %>%
  filter(
    hscp_name %in% c("Glasgow City","North Lanarkshire","West Dunbartonshire","Scottish Borders")
  )


chosen_within_2_days_variation_by_hscp_plot <- ggplot(chosen_within_2_days_variation_by_hscp, 
       aes(x = hscp_name, y = within_2_days_percentage)) +
  geom_jitter(
    aes(colour = hscp_name),
    width = 0.4, height = 0,
    size = 3, alpha = 0.75
  ) +
  scale_y_continuous(limits = c(0,100))+
  labs(
    title = "Urgent access by HSCP",
    x = "HSCP",
    y = "% seen within 2 days"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0)
  )
chosen_within_2_days_variation_by_hscp_plot
save_plot_with_script_name(chosen_within_2_days_variation_by_hscp_plot)

#------------------------## Overall care variation analysis ##--------------------------#
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
  Top5 = overall_care_variation_by_hscp %>% slice_head(n = 5),
  Bottom5 = overall_care_variation_by_hscp %>% slice_tail(n = 5),
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
    hscp_name %in% c("North Lanarkshire","Aberdeenshire","East Dunbartonshire","Glasgow City")
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
    title = "% rating overall care positive",
    x = "HSCP",
    y = "% positive"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0)
  )
chosen_overall_care_variation_by_hscp_plot
save_plot_with_script_name(chosen_overall_care_variation_by_hscp_plot)
#------------------## informed choice variation analysis ##----------------------#
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


chosen_informed_choice_variation_by_hscp_plot <- ggplot(chosen_informed_choice_variation_by_hscp, 
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


chosen_OOH_care_variation_by_hb_plot <- ggplot(chosen_OOH_care_variation_by_hb, 
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
