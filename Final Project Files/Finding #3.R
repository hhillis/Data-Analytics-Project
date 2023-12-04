# Change in CRP Acres and HPI Over Time
static_plot <- ggplot(
  clean_data, 
  aes(x = ACRES,
      y = HPI_base_2000,
      size = 0.1,
      colour = STATE)
) +
  geom_point(show.legend = FALSE,
             size = 0.1) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_continuous(
    labels = scales::number_format(scale = 1e-3, accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(accuracy = 1)
  ) +
  labs(x = "Conservation Easement Acres in 1,000s",
       y = "Housing Price Index (Base Year 2000)",
       title = "Change in HPI and CRP Acres in the U.S. from 1986-2022",
       subtitle = "Year: {frame_time}") +
  theme(plot.title = element_text(hjust = 0.5))
static_plot
finding_3_HPI_ACRES_changing_over_time <- static_plot + transition_time(as.integer(YEAR))

# Save plot
print(finding_3_HPI_ACRES_changing_over_time)
anim_save("C:/Users/s75j325/OneDrive - Montana State University/Holly - Personal/School Files/ECNS 560 - Advanced Data Analytics in Economics/Final Project Files/Graphs/finding_3_HPI_ACRES_changing_over_time.gif", finding_3_HPI_ACRES_changing_over_time, width = 12, height = 6, units = "in")

