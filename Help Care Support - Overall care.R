SCRIPT_NAME <- "Help Care Support - Overall care"

###############################################################################
## Comparing to the last surveys results at Scotland level ##
manual_df <- data.frame(
  "Year" = c("2017-18", "2019-20", "2021-22", "2023-24","2025-26"),
  "Percentage" = c(82,69,62,63,62),
  check.names = FALSE
)

help_care_support_overall_care_scotland_timeseries_scatter <- make_scatter(
  data = manual_df,
  x_var = Year,
  y_var = Percentage,
  title = "Timeseries of the percentage of positive ratings for overall help, care and support with everyday living",
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

help_care_support_overall_care_scotland_timeseries_scatter
save_plot_with_script_name(help_care_support_overall_care_scotland_timeseries_scatter,width = 22, height =10 , show_title = FALSE)
