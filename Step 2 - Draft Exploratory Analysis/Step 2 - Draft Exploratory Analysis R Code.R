## Libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(skimr)
library(ggridges)
library(ggthemes)
library(viridis)
library(dslabs)
library(gganimate)

## Wrangle and Clean
# Read in excel files of data
setwd("C:/Users/s75j325/OneDrive - Montana State University/Holly - Personal/School Files/ECNS 560 - Advanced Data Analytics in Economics/Semester Project/")
easements_data <- read_excel("Conservation Easement Acres by County.xlsx")
housing_data <- read_excel("Housing Prices by County.xlsx")

# Clean easements_data and turn it to tidy format
clean_easements_data <- easements_data

# Verify that FIPS code is the primary key
dups_easements <- clean_easements_data |> 
  count(FIPS) |> 
  filter(n > 1)
stopifnot(nrow(dups_easements) == 0)

# Transform data to long/tidy format
tidy_clean_easements_data <- clean_easements_data |> 
  pivot_longer(cols = "1986":"2022", names_to="YEAR", values_to="ACRES")
tidy_clean_easements_data

# Verify that FIPS code and year are now the primary key
dups_tidy_easements <- tidy_clean_easements_data |> 
  count(FIPS, YEAR) |> 
  filter(n > 1)
stopifnot(nrow(dups_tidy_easements) == 0)
stopifnot(nrow(tidy_clean_easements_data) == nrow(tidy_clean_easements_data |> distinct(FIPS, YEAR)))

# Check column formats
str(tidy_clean_easements_data)

# convert YEAR to numeric
tidy_clean_easements_data <- tidy_clean_easements_data |>
  mutate(YEAR = as.numeric(YEAR))

# Check column formats
str(tidy_clean_easements_data)

# View summary of ACRES values
summary(tidy_clean_easements_data$ACRES)

# View if there is a pattern to missing ACRES values
missing_acres_data <- filter(tidy_clean_easements_data, is.na(ACRES))
print(missing_acres_data)

# Clean housing_data
# Rename columns to match easements data set
clean_housing_data <- housing_data
clean_housing_data <- clean_housing_data |>
  rename(STATE = "State")
clean_housing_data <- clean_housing_data |>
  rename(COUNTY = "County")
clean_housing_data <- clean_housing_data |>
  rename(FIPS = "FIPS code")
clean_housing_data <- clean_housing_data |>
  rename(YEAR = "Year")
clean_housing_data <- clean_housing_data |>
  rename(percent_annual_change = "Annual Change (%)")
clean_housing_data <- clean_housing_data |>
  rename(HPI_base_1990 = "HPI with 1990 base")
clean_housing_data <- clean_housing_data |>
  rename(HPI_base_2000 = "HPI with 2000 base")

# Mutate states to full names instead of postal abbreviations
state_lookup <- data.frame(
  abbrev = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  name = c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
)
clean_housing_data <- clean_housing_data |>
  mutate(STATE = toupper(state_lookup$name[match(clean_housing_data$STATE, state_lookup$abbrev)]))

# Mutate county names to all caps
clean_housing_data <- clean_housing_data |>
  mutate(COUNTY = toupper(clean_housing_data$COUNTY))

# Double check that FIPS code and year are the primary key
dups_housing <- clean_housing_data |> 
  count(FIPS, YEAR) |> 
  filter(n > 1)
stopifnot(nrow(dups_housing) == 0)

# Change period values to missing values
clean_housing_data <- clean_housing_data |>
  mutate(percent_annual_change = na_if(percent_annual_change, ".")) |>
  mutate(HPI = na_if(HPI, ".")) |>
  mutate(HPI_base_1990 = na_if(HPI_base_1990, ".")) |>
  mutate(HPI_base_2000 = na_if(HPI_base_2000, "."))

# Check column formats
str(clean_housing_data)

# Convert columns to numeric
clean_housing_data <- clean_housing_data |>
  mutate(FIPS = as.numeric(FIPS)) |>
  mutate(YEAR = as.numeric(YEAR)) |>
  mutate(percent_annual_change = as.numeric(percent_annual_change)) |>
  mutate(HPI = as.numeric(HPI)) |>
  mutate(HPI_base_1990 = as.numeric(HPI_base_1990)) |>
  mutate(HPI_base_2000 = as.numeric(HPI_base_2000))

# Check column formats
str(clean_housing_data)

# Merge data sets on FIPS code and year
summary(tidy_clean_easements_data$YEAR)
summary(clean_housing_data$YEAR)
merged_data <- dplyr::left_join(clean_housing_data, tidy_clean_easements_data, by = join_by(FIPS, YEAR, STATE, COUNTY))

# Double check that FIPS code and year are still the primary key
dups_merged <- merged_data |> 
  count(FIPS, YEAR) |> 
  filter(n > 1)
stopifnot(nrow(dups_merged) == 0)

## Explore
# View summaries of all variables in merged data
skim(merged_data)

# View what STATE variables are missing
missing_state_data <- filter(merged_data, is.na(STATE))

# Replace NAs in STATE variable with "DISTRICT OF COLUMBIA"
merged_data <- merged_data |>
  mutate(STATE = ifelse(is.na(STATE), "DISTRICT OF COLUMBIA", STATE))

# Transform HPI_base_2000 and ACRES variables
merged_data <- merged_data |>
  mutate(ln_ACRES = log(ACRES)) |>
  mutate(ln_ACRES = as.numeric(ln_ACRES)) |>
  mutate(ln_ACRES = replace(ln_ACRES, is.infinite(ln_ACRES) | is.nan(ln_ACRES), NA)) |>
  mutate(ln_HPI_base_2000 = log(HPI_base_2000)) |>
  mutate(ln_HPI_base_2000 = as.numeric(ln_HPI_base_2000))

# Save merged_data
save(merged_data, file = "clean_data.Rda")

# Plot outcome variable of housing price index (base year 2000) by year
ggplot(data = merged_data, aes(x = YEAR, y = HPI_base_2000)) +
  geom_point(na.rm = TRUE)
ggplot(data = merged_data, aes(x = YEAR, y = ln_HPI_base_2000)) +
  geom_point(na.rm = TRUE)

# Plot how HPI_base_2000 varies by state and year
ggplot(data = merged_data, aes(x = YEAR, y = ln_HPI_base_2000, color = STATE)) +
  geom_line(na.rm = TRUE)

# Plot how HPI_base_2000 varies by MT county and year
montana_data <- subset(merged_data, STATE == "MONTANA")
ggplot(data = montana_data, aes(x = YEAR, y = ln_HPI_base_2000, color = COUNTY)) +
  geom_line(na.rm = TRUE)

# Calculate means of ln_HPI_base_2000 by YEAR groups
means_HPI_df <- merged_data |>
  group_by(YEAR) |>
  summarize(mean_ln_HPI_base_2000 = mean(ln_HPI_base_2000, na.rm = TRUE))

# Plot the CEF of ln_HPI_base_2000
gg_cef_HPI <- ggplot(data = merged_data, aes(x = ln_HPI_base_2000, y = as.factor(YEAR))) +
  geom_density_ridges_gradient(
    aes(fill = ..x..),
    rel_min_height = 0.003,
    color = "white",
    scale = 2.5,
    size = 0.3
  ) +
  scale_x_continuous(
    "Percent Increase in HPI (Base Year 2000)",
    labels = scales::percent_format(scale = 1)
  ) +
  scale_y_discrete(
    "Year",
    breaks = seq(min(merged_data$YEAR), max(merged_data$YEAR), by = 5)
  ) +
  scale_fill_viridis(option = "magma") +
  theme_pander(base_size = 15) +
  theme(
    legend.position = "none"
  ) +
  coord_flip()

# Plot the conditional distributions
gg_cef_HPI

# Add the conditional distributions' means
gg_cef_HPI +
  geom_path(
    data = means_HPI_df,
    aes(x = mean_ln_HPI_base_2000, y = as.factor(YEAR)),
    color = "black",
    alpha = 0.85
  ) +
  geom_point(
    data = means_HPI_df,
    aes(x = mean_ln_HPI_base_2000, y = as.factor(YEAR)),
    color = "black",
    shape = 16,
    size = 2
  )

# Plot independent variable of conservation easement acres by year
ggplot(data = merged_data, aes(x = YEAR, y = ACRES)) +
  geom_point(na.rm = TRUE)
ggplot(data = merged_data, aes(x = YEAR, y = ln_ACRES)) +
  geom_point(na.rm = TRUE)

# Plot how ACRES varies by state and year
ggplot(data = merged_data, aes(x = YEAR, y = ln_ACRES, color = STATE)) +
  geom_line(na.rm = TRUE)

# Plot how ACRES varies by MT county and year
ggplot(data = montana_data, aes(x = YEAR, y = ln_ACRES, color = COUNTY)) +
  geom_line(na.rm = TRUE)

# Calculate means of ln_ACRES by YEAR groups
means_ACRES_df <- merged_data |>
  group_by(YEAR) |>
  summarize(mean_ln_ACRES = mean(ln_ACRES, na.rm = TRUE))

# Plot the CEF of ln_ACRES
gg_cef_ACRES <- ggplot(data = merged_data, aes(x = ln_ACRES, y = as.factor(YEAR))) +
  geom_density_ridges_gradient(
    aes(fill = ..x..),
    rel_min_height = 0.003,
    color = "white",
    scale = 2.5,
    size = 0.3
  ) +
  scale_x_continuous(
    "Percent Change in Conservation Easement Acres",
    labels = scales::percent_format(scale = 1)
  ) +
  scale_y_discrete(
    "Year",
    breaks = seq(min(merged_data$YEAR), max(merged_data$YEAR), by = 5)
  ) +
  scale_fill_viridis(option = "magma") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none"
  ) +
  coord_flip()

# Plot the conditional distributions
gg_cef_ACRES

# Add the conditional distributions' means
gg_cef_ACRES +
  geom_path(
    data = means_ACRES_df,
    aes(x = mean_ln_ACRES, y = as.factor(YEAR)),
    color = "black",
    alpha = 0.85
  ) +
  geom_point(
    data = means_ACRES_df,
    aes(x = mean_ln_ACRES, y = as.factor(YEAR)),
    color = "black",
    shape = 16,
    size = 2
  )

## Visualize and Communicate
# Filter out missing values
filtered_merged_data <- merged_data |>
  filter(!is.na(ACRES) | !is.nan(ACRES) | !is.infinite(ACRES) | !is.na(ln_ACRES) | !is.nan(ln_ACRES) | !is.infinite(ln_ACRES) | !is.na(HPI_base_2000) | !is.nan(HPI_base_2000) | !is.infinite(HPI_base_2000) | !is.na(ln_HPI_base_2000) | !is.nan(ln_HPI_base_2000) | !is.infinite(ln_HPI_base_2000))

## Conservation easement acres and HPI have both increased over the years.
# Plot relationship between conservation easement acres and HPI in 1990 and 2020
filter(filtered_merged_data, YEAR %in% c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) |>
  ggplot(aes(ACRES, HPI_base_2000, col = YEAR)) +
  geom_point() +
  facet_grid(. ~ YEAR) +
  theme(legend.position = "none") +
  xlab("Conservation Easement Acres") +
  ylab("Housing Price Index (Base Year 2000)")

## The rate of the increase for both of these have also increased over the years
filter(filtered_merged_data, YEAR %in% c(1986, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) |>
  ggplot(aes(ACRES, ln_HPI_base_2000, col = YEAR)) +
  geom_point() +
  facet_grid(. ~ YEAR) +
  theme(legend.position = "none") +
  xlab("Conservation Easement Acres") +
  ylab("Housing Price Index (Base Year 2000)")

## Both have increased, but the spread between each has increased as well
# Plot animation of conservation easements and HPI over time
static_plot <- ggplot(
  filtered_merged_data, 
  aes(x = ACRES, y=HPI_base_2000, size = 0.25, colour = STATE)
) +
  geom_point(show.legend = FALSE, alpha = 0.25) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "Conservation Easement Acres", y = "Housing Price Index (Base Year 2000)")
static_plot
static_plot + transition_time(YEAR) +
  labs(title = "Year: {frame_time}")
