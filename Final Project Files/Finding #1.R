# Change in Housing Price Index Over Time
finding_1.1_HPI_changes_plot <- ggplot(data = clean_data, aes(x = HPI_base_2000, y = as.factor(YEAR))) +
  geom_path(
    data = means_year_HPI_df,
    aes(x = as.factor(YEAR), y = Ytbar_mean_HPI_base_2000_by_year),
    color = "black",
    alpha = 0.85
  ) +
  geom_point(
    data = means_year_HPI_df,
    aes(x = as.factor(YEAR), y = Ytbar_mean_HPI_base_2000_by_year),
    color = "black",
    shape = 16,
    size = 2
  ) +
  labs(
    x = "Year",
    y = "HPI (Base Year 2000)",
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(
    "HPI (Base Year 2000)",
    labels = scales::dollar_format(accuracy = 1)
  ) +
  scale_x_discrete(
    "Year",
    breaks = seq(min(clean_data$YEAR), max(clean_data$YEAR), by = 5)
  ) +
  theme_pander(base_size = 15) +
  theme(
    legend.position = "none"
  )

# Change in Logged HPI Over Time
finding_2.2_ln_HPI_changes_plot <- ggplot(data = clean_data, aes(x = ln_HPI_base_2000, y = as.factor(YEAR))) +
  geom_path(
    data = means_year_ln_HPI_df,
    aes(x = as.factor(YEAR), y = Ytbar_mean_ln_HPI_base_2000_by_year),
    color = "black",
    alpha = 0.85
  ) +
  geom_point(
    data = means_year_ln_HPI_df,
    aes(x = as.factor(YEAR), y = Ytbar_mean_ln_HPI_base_2000_by_year),
    color = "black",
    shape = 16,
    size = 2
  ) +
  labs(
    x = "Year",
    y = "Logged HPI (Base Year 2000)",
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(
    "Logged HPI (Base Year 2000)",
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_x_discrete(
    "Year",
    breaks = seq(min(clean_data$YEAR), max(clean_data$YEAR), by = 5)
  ) +
  theme_pander(base_size = 15) +
  theme(
    legend.position = "none"
  )

# Combine both graphs side-by-side
finding_1_combined_HPI_plots <- plot_grid(finding_1.1_HPI_changes_plot,
                                          finding_2.2_ln_HPI_changes_plot,
                                          align = "hv")

# Add a title
finding_1_title <- ggdraw() +
  draw_label("Housing Price Index (HPI) in the U.S. from 1986-2022", size = 16)

# Combine the title and the plot
finding_1_combined_HPI_plots <- plot_grid(finding_1_title, finding_1_combined_HPI_plots, ncol = 1, rel_heights = c(0.1, 1))

# Print or save the combined plot
print(finding_1_combined_HPI_plots)
ggsave("C:/Users/s75j325/OneDrive - Montana State University/Holly - Personal/School Files/ECNS 560 - Advanced Data Analytics in Economics/Final Project Files/Graphs/finding_1_combined_HPI_plots.jpeg", finding_1_combined_HPI_plots, width = 12, height = 6, units = "in")

