finding_4.1_data = clean_data |>
  mutate(ln_ACRES_bins = factor(ntile(ln_ACRES, 50))) |>
  group_by(ln_ACRES_bins) |>
  mutate(ln_HPI_binned = mean(ln_HPI_base_2000, na.rm = TRUE),
         ln_ACRES_binned = mean(ln_ACRES, na.rm = TRUE))

finding_4.1.1_bin_plot <- ggplot(finding_4.1_data) +
  geom_point(aes(x = ln_ACRES, y = ln_HPI_base_2000, color = ln_ACRES_bins)) +
  geom_point(aes(x = ln_ACRES_binned, y = ln_HPI_binned), size = 2) +
  labs(
    x = NULL,
    y = "Logged HPI (Base Year 2000)"
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  theme_pander(base_size = 15) +
  theme(
    legend.position = "none"
  )

finding_4.1.2_smooth_reg_plot <- ggplot(finding_4.1_data, aes(x = ln_ACRES, y = ln_HPI_base_2000)) +
  geom_point(aes(x = ln_ACRES_binned, y = ln_HPI_binned)) +
  geom_smooth() +
  labs(
    x = "Logged CRP Acres",
    y = NULL
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  theme_pander(base_size = 15) +
  theme(
    legend.position = "none"
  )

finding_4.1.3_broken_reg_plot <- ggplot(finding_4.1_data, aes(x = ln_ACRES, y = ln_HPI_base_2000)) +
  geom_point(aes(x = ln_ACRES_binned, y = ln_HPI_binned)) +
  geom_smooth(data = filter(finding_4.1_data, ln_ACRES_binned < 0.1), method = "lm", formula = y ~ x) +
  geom_smooth(data = filter(finding_4.1_data, ln_ACRES_binned >= 0.1 & ln_ACRES_binned <= 7.6), method = "lm", formula = y ~ x) +
  geom_smooth(data = filter(finding_4.1_data, ln_ACRES_binned > 7.6), method = "lm", formula = y ~ x) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  theme_pander(base_size = 15) +
  theme(
    legend.position = "none"
  )

# Combine graphs side-by-side
finding_4.1_combined_reg_plots <- plot_grid(finding_4.1.1_bin_plot,
                                            finding_4.1.2_smooth_reg_plot,
                                            finding_4.1.3_broken_reg_plot,
                                            ncol = 3,
                                            nrow = 1,
                                            align = "hv")

# Add a title
finding_4.1_title <- ggdraw() +
  draw_label("Relationship Between CRP Acres and HPI in the U.S. from 1986-2022", size = 16)

# Combine the title and the plot
finding_4.1_combined_reg_plots <- plot_grid(finding_4.1_title, finding_4.1_combined_reg_plots, ncol = 1, rel_heights = c(0.1, 1))

# Print and save the combined plot
print(finding_4.1_combined_reg_plots)
ggsave("C:/Users/s75j325/OneDrive - Montana State University/Holly - Personal/School Files/ECNS 560 - Advanced Data Analytics in Economics/Final Project Files/Graphs/finding_4.1_combined_reg_plots.jpeg", finding_4.1_combined_reg_plots, width = 12, height = 6, units = "in")
















finding_4.2_data = clean_data |>
  mutate(Xreg_ln_bins = factor(ntile(Xreg_ln, 50))) |>
  group_by(Xreg_ln_bins) |>
  mutate(Yreg_ln_binned = mean(Yreg_ln, na.rm = TRUE),
         Xreg_ln_binned = mean(Xreg_ln, na.rm = TRUE))

finding_4.2.1_bin_plot_FE <- ggplot(finding_4.2_data) +
  geom_point(aes(x = Xreg_ln, y = Yreg_ln, color = Xreg_ln_bins)) +
  geom_point(aes(x = Xreg_ln_binned, y = Yreg_ln_binned), size = 2) +
  labs(
    x = NULL,
    y = "De-meaned, Logged HPI (Base Year 2000)"
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  theme_pander(base_size = 15) +
  theme(
    legend.position = "none"
  )

finding_4.2.2_smooth_reg_plot_FE <- ggplot(finding_4.2_data, aes(x = Xreg_ln, y = Yreg_ln)) +
  geom_point(aes(x = Xreg_ln_binned, y = Yreg_ln_binned)) +
  geom_smooth() +
  labs(
    x = "De-meaned, Logged CRP Acres",
    y = NULL
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  theme_pander(base_size = 15) +
  theme(
    legend.position = "none"
  )

finding_4.2.3_broken_reg_plot_FE <- ggplot(finding_4.2_data, aes(x = Xreg_ln, y = Yreg_ln)) +
  geom_point(aes(x = Xreg_ln_binned, y = Yreg_ln_binned)) +
  geom_smooth(data = filter(finding_4.2_data, Xreg_ln_binned < -7.1), method = "lm", formula = y ~ x) +
  geom_smooth(data = filter(finding_4.2_data, Xreg_ln_binned >= -7.1 & Xreg_ln_binned <= -6), method = "lm", formula = y ~ x) +
  geom_smooth(data = filter(finding_4.2_data, Xreg_ln_binned >= -6 & Xreg_ln_binned <= -5.5), method = "lm", formula = y ~ x) +
  geom_smooth(data = filter(finding_4.2_data, Xreg_ln_binned > -5.5), method = "lm", formula = y ~ x) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1)
  ) +
  theme_pander(base_size = 15) +
  theme(
    legend.position = "none"
  )

# Combine graphs side-by-side
finding_4.2_combined_reg_plots_FE <- plot_grid(finding_4.2.1_bin_plot_FE,
                                               finding_4.2.2_smooth_reg_plot_FE,
                                               finding_4.2.3_broken_reg_plot_FE,
                                               ncol = 3,
                                               nrow = 1,
                                               align = "hv")

# Add a title
finding_4.2_title <- ggdraw() +
  draw_label("Relationship Between CRP Acres and HPI in the U.S. from 1986-2022", size = 16)

# Add a subtitle
finding_4.2_subtitle <- ggdraw() +
  draw_label("With Year and County Fixed Effects", size = 14)

# Combine the title and the plot
finding_4.2_combined_reg_plots_FE <- plot_grid(finding_4.2_title, finding_4.2_subtitle, finding_4.2_combined_reg_plots_FE, ncol = 1, rel_heights = c(0.1, 0.1, 1))

# Print and save the combined plot
print(finding_4.2_combined_reg_plots_FE)
ggsave("C:/Users/s75j325/OneDrive - Montana State University/Holly - Personal/School Files/ECNS 560 - Advanced Data Analytics in Economics/Final Project Files/Graphs/finding_4.2_combined_reg_plots_FE.jpeg", finding_4.2_combined_reg_plots_FE, width = 12, height = 6, units = "in")

