# Libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(skimr)
library(ggridges)
library(ggthemes)
library(viridis)
library(dslabs)
library(gganimate)
library(binsreg)
library(fixest)
library(cowplot)
library(broom)
library(gt)

# Read in excel files of data
setwd()
easements_data <- read_excel("Conservation Easement Acres by County.xlsx")
housing_data <- read_excel("Housing Prices by County.xlsx")

# Clean easements_data
# Turn data set to tidy format
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

# View summaries of all variables in merged data
skim(merged_data)

# View what STATE variables are missing
missing_state_data <- filter(merged_data, is.na(STATE))

# Replace NAs in STATE variable with "DISTRICT OF COLUMBIA"
merged_data <- merged_data |>
  mutate(STATE = ifelse(is.na(STATE), "DISTRICT OF COLUMBIA", STATE))

# Filter out years before 1986 where we do not have ACRES data
subset_data <- merged_data |>
  filter(YEAR >= 1986)

# Transforming variables
# Transform HPI_base_2000 and ACRES variables
subset_data <- subset_data |>
  mutate(ACRES_to_be_logged = ACRES + 1) |>
  mutate(ln_ACRES = log(ACRES_to_be_logged)) |>
  mutate(ln_ACRES = as.numeric(ln_ACRES))

# Calculate the average ACRES for each county
means_county_ACRES_df <- subset_data |>
  group_by(FIPS) |>
  summarize(Xibar_mean_ACRES_by_county = ifelse(all(is.na(ACRES)), NA, mean(ACRES, na.rm = TRUE)))
subset_data <- merge(subset_data, means_county_ACRES_df, by = "FIPS", all.x = TRUE)

# Calculate the average ln_ACRES for each county
means_county_ln_ACRES_df <- subset_data |>
  group_by(FIPS) |>
  summarize(Xibar_mean_ln_ACRES_by_county = ifelse(all(is.na(ln_ACRES)), NA, mean(ln_ACRES, na.rm = TRUE)))
subset_data <- merge(subset_data, means_county_ln_ACRES_df, by = "FIPS", all.x = TRUE)

# Calculate the average ACRES for each year
means_year_ACRES_df <- subset_data |>
  group_by(YEAR) |>
  summarize(Xtbar_mean_ACRES_by_year = mean(ACRES, na.rm = TRUE))
subset_data <- merge(subset_data, means_year_ACRES_df, by = "YEAR", all.x = TRUE)

# Calculate the average ln_ACRES for each year
means_year_ln_ACRES_df <- subset_data |>
  group_by(YEAR) |>
  summarize(Xtbar_mean_ln_ACRES_by_year = mean(ln_ACRES, na.rm = TRUE))
subset_data <- merge(subset_data, means_year_ln_ACRES_df, by = "YEAR", all.x = TRUE)

# Log HPI_base_2000 observations
subset_data <- subset_data |>
  mutate(ln_HPI_base_2000 = log(HPI_base_2000)) |>
  mutate(ln_HPI_base_2000 = as.numeric(ln_HPI_base_2000))

# Calculate the average HPI_base_2000 for each county
means_county_HPI_df <- subset_data |>
  group_by(FIPS) |>
  summarize(Yibar_mean_HPI_base_2000_by_county = ifelse(all(is.na(HPI_base_2000)), NA, mean(HPI_base_2000, na.rm = TRUE)))
subset_data <- merge(subset_data, means_county_HPI_df, by = "FIPS", all.x = TRUE)

# Calculate the average ln_HPI_base_2000 for each county
means_county_ln_HPI_df <- subset_data |>
  group_by(FIPS) |>
  summarize(Yibar_mean_ln_HPI_base_2000_by_county = ifelse(all(is.na(ln_HPI_base_2000)), NA, mean(ln_HPI_base_2000, na.rm = TRUE)))
subset_data <- merge(subset_data, means_county_ln_HPI_df, by = "FIPS", all.x = TRUE)

# Calculate the average HPI_base_2000 for each year
means_year_HPI_df <- subset_data |>
  group_by(YEAR) |>
  summarize(Ytbar_mean_HPI_base_2000_by_year = mean(HPI_base_2000, na.rm = TRUE))
subset_data <- merge(subset_data, means_year_HPI_df, by = "YEAR", all.x = TRUE)

# Calculate the average ln_HPI_base_2000 for each year
means_year_ln_HPI_df <- subset_data |>
  group_by(YEAR) |>
  summarize(Ytbar_mean_ln_HPI_base_2000_by_year = mean(ln_HPI_base_2000, na.rm = TRUE))
subset_data <- merge(subset_data, means_year_ln_HPI_df, by = "YEAR", all.x = TRUE)

# Calculate the demeaned values and transform variable types
subset_data <- subset_data |>
  mutate(Yreg = HPI_base_2000 - Yibar_mean_HPI_base_2000_by_county - Ytbar_mean_HPI_base_2000_by_year) |>
  mutate(Yreg_ln = ln_HPI_base_2000 - Yibar_mean_ln_HPI_base_2000_by_county - Ytbar_mean_ln_HPI_base_2000_by_year) |>
  mutate(Xreg = ACRES - Xibar_mean_ACRES_by_county - Xtbar_mean_ACRES_by_year) |>
  mutate(Xreg_ln = ln_ACRES - Xibar_mean_ln_ACRES_by_county - Xtbar_mean_ln_ACRES_by_year)

# Create clean_data data frame
clean_data <- subset_data

# Convert YEAR and FIPS variables to factors to use them as dummy variables in regressions
clean_econo <- clean_data |>
  mutate(YEAR = as.factor(YEAR)) |>
  mutate(FIPS = as.factor(FIPS))