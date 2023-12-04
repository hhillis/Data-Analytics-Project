# Change in CRP Acres Over Time
finding_2.1_ACRES_changes_plot <- ggplot(data = clean_data, aes(x = ACRES, y = as.factor(YEAR))) +
  geom_path(
    data = means_year_ACRES_df,
    aes(x = as.factor(YEAR), y = Xtbar_mean_ACRES_by_year),
    color = "black",
    alpha = 0.85
  ) +
  geom_point(
    data = means_year_ACRES_df,
    aes(x = as.factor(YEAR), y = Xtbar_mean_ACRES_by_year),
    color = "black",
    shape = 16,
    size = 2
  ) +
  labs(
    x = "Year",
    y = "Number of CRP Acres"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(
    "Number of CRP Acres",
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

# Change in Logged CRP Acres Over Time
finding_2.2_ln_ACRES_changes_plot <- ggplot(data = clean_data, aes(x = ln_ACRES, y = as.factor(YEAR))) +
  geom_path(
    data = means_year_ln_ACRES_df,
    aes(x = as.factor(YEAR), y = Xtbar_mean_ln_ACRES_by_year),
    color = "black",
    alpha = 0.85
  ) +
  geom_point(
    data = means_year_ln_ACRES_df,
    aes(x = as.factor(YEAR), y = Xtbar_mean_ln_ACRES_by_year),
    color = "black",
    shape = 16,
    size = 2
  ) +
  labs(
    x = "Year",
    y = "Logged Number of CRP ln_ACRES"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(
    "Logged Number of CRP ACRES",
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
finding_2_combined_ACRES_plots <- plot_grid(finding_2.1_ACRES_changes_plot,
                                            finding_2.2_ln_ACRES_changes_plot,
                                            align = "hv")

# Add a title
finding_2_title <- ggdraw() +
  draw_label("CRP Acres in the U.S. from 1986-2022", size = 16)

# Combine the title and the plot
finding_2_combined_ACRES_plots <- plot_grid(finding_2_title, finding_2_combined_ACRES_plots, ncol = 1, rel_heights = c(0.1, 1))

# Print or save the combined plot
print(finding_2_combined_ACRES_plots)
ggsave("C:/Users/s75j325/OneDrive - Montana State University/Holly - Personal/School Files/ECNS 560 - Advanced Data Analytics in Economics/Final Project Files/Graphs/finding_2_combined_ACRES_plots.jpeg", finding_2_combined_ACRES_plots, width = 12, height = 6, units = "in")

