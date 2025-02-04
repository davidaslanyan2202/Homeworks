install.packages("dplyr")
install.packages("lubridate")
library(dplyr)
library(lubridate)

# Load the dataset
file_path <- "/Users/davidaslanyan/Documents/AUA I 2425Spring I Courses/Data Visualization/Homeworks_DV/crime_data.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Display the first 5 rows
head(df)

# Identify columns with missing values
missing_values <- colSums(is.na(df))
missing_values <- missing_values[missing_values > 0]
print("Missing values per column:")
print(missing_values)

# Drop columns where more than 50% of data is missing
df_cleaned <- df[, colSums(is.na(df)) < nrow(df) * 0.5]

# Standardize column names to lowercase and remove spaces
colnames(df_cleaned) <- tolower(trimws(colnames(df_cleaned)))

# Check if date_occ column exists
if (!"date_occ" %in% colnames(df_cleaned)) {
  stop("Error: date_occ column not found in dataset")
}

# Convert DATE OCC to datetime format and filter out invalid dates
df_cleaned$date_occ <- mdy(df_cleaned$date_occ)
df_cleaned <- df_cleaned %>% filter(!is.na(date_occ))

# Ensure there is valid data before extracting year, month, and day
if (nrow(df_cleaned) > 0) {
  df_cleaned$year <- year(df_cleaned$date_occ)
  df_cleaned$month <- month(df_cleaned$date_occ)
  df_cleaned$day <- day(df_cleaned$date_occ)
} else {
  stop("Error: No valid date data found.")
}

# Extract hour from TIME OCC (assuming it's in HHMM format)
df_cleaned$hour <- floor(df_cleaned$time_occ / 100)

# Filter for crimes in 2023 and for BURGLARY
df_2023 <- filter(df_cleaned, year == 2023)
df_burglary <- filter(df_2023, crm_cd_desc == "BURGLARY")

# Group by AREA NAME to calculate total crimes and average victim age
crime_summary <- df_cleaned %>%
  group_by(area_name) %>%
  summarise(total_crimes = n(),
            average_victim_age = mean(vict_age, na.rm = TRUE)) %>%
  arrange(desc(total_crimes))
print(head(crime_summary))

# Further Exploration: Top 3 most frequent crime descriptions
top_crimes <- df_cleaned %>% count(crm_cd_desc, sort = TRUE) %>% head(3)
print(top_crimes)

# Group by Hour and count number of crimes
crime_by_hour <- df_cleaned %>% count(hour)
print(crime_by_hour)

# Group by Victim Sex and calculate total crimes and average victim age
victim_summary <- df_cleaned %>%
  group_by(vict_sex) %>%
  summarise(total_crimes = n(),
            average_victim_age = mean(vict_age, na.rm = TRUE))
print(victim_summary)

# Advanced Analysis: Create Severity Score
df_cleaned <- df_cleaned %>%
  mutate(severity_score = case_when(
    "weapon_used_cd" %in% names(df_cleaned) & !is.na(weapon_used_cd) ~ 5,
    crm_cd_desc == "BURGLARY" ~ 3,
    TRUE ~ 1
  ))

severity_by_area <- df_cleaned %>%
  group_by(area_name) %>%
  summarise(total_severity_score = sum(severity_score))
print(severity_by_area)

