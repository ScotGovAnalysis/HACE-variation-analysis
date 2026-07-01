Indicators_scotland_timeseries <- bind_rows(
  easy_contact_Indicators_scotland_timeseries <- easy_contact_scotland_timeseries %>% 
    mutate("Indicator"="Ease of contacting GP"),
  overall_care_Indicators_scotland_timeseries<- overall_care_scotland_timeseries %>% 
    mutate("Indicator"="Overall care"),
  informed_choice_Indicators_scotland_timeseries<- informed_choice_scotland_timeseries %>% 
    mutate("Indicator"="Were able to make an informed choice"),
  OOH_care_Indicators_scotland_timeseries<- OOH_care_scotland_timeseries %>% 
    mutate("Indicator"="Treated with compassion and understanding during out of hours care")
  )

Indicators_scotland_timeseries_scatter <- ggplot(
  Indicators_scotland_timeseries,
  aes(x = Year, y = Percentage, colour = Indicator, group = Indicator)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  labs(
    title = "Percentage responding positively to indicators over time",
    x = "Year",
    y = "Percentage (%)",
    colour = "Indicator"
  ) +
  theme_minimal()+
  theme(
    legend.position = c(0.5, 0.1),
    legend.justification = c(0.05, 0), 
    legend.background = element_rect(fill = "white", colour = "black")
  )

Indicators_scotland_timeseries_scatter
save_plot_with_script_name(Indicators_scotland_timeseries_scatter)
