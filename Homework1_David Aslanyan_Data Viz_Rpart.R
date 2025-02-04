install.packages("dplyr")
install.packages("lubridate")
install.packages("data.table")

library(dplyr)
library(lubridate)
library(data.table)

file_path <- "/Users/davidaslanyan/Documents/AUA I 2425Spring I Courses/Data Visualization/Homeworks_DV/crime_data.csv"
df <- fread(file_path, stringsAsFactors = FALSE)

head(df)

missing_values <- colSums(is.na(df))
missing_values <- missing_values[missing_values > 0]
print("Missing values per column:")
print(missing_values)

cols_to_keep <- colSums(is.na(df)) < nrow(df) * 0.5
if (sum(cols_to_keep) == 0) {
  stop("Error: All columns have more than 50% missing values. No columns left to process.")
}
df_cleaned <- df[, ..cols_to_keep]

colnames(df_cleaned) <- tolower(gsub(" ", "_", trimws(colnames(df_cleaned))))

if (!"date_occ" %in% colnames(df_cleaned)) {
  stop("Error: date_occ column not found in dataset.")
}

print("First few rows of date_occ:")
print(head(df_cleaned$date_occ))

print("Missing values in date_occ:")
print(table(is.na(df_cleaned$date_occ)))
print("Empty values in date_occ:")
print(table(df_cleaned$date_occ == ""))

df_cleaned$date_occ <- mdy(df_cleaned$date_occ)

df_cleaned <- df_cleaned %>% filter(!is.na(date_occ))

if (nrow(df_cleaned) > 0) {
  df_cleaned$year <- year(df_cleaned$date_occ)
  df_cleaned$month <- month(df_cleaned$date_occ)
  df_cleaned$day <- day(df_cleaned$date_occ)
} else {
  stop("Error: No valid date data found.")
}

df_cleaned$hour <- floor(df_cleaned$time_occ / 100)

df_2023 <- filter(df_cleaned, year == 2023)
if (!"crm_cd_desc" %in% colnames(df_cleaned)) {
  stop("Error: crm_cd_desc column not found in dataset.")
}
df_burglary <- filter(df_2023, tolower(crm_cd_desc) == "burglary")

crime_summary <- df_cleaned %>%
  group_by(area_name) %>%
  summarise(total_crimes = n(),
            average_victim_age = mean(vict_age, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(desc(total_crimes))
print(head(crime_summary))

top_crimes <- df_cleaned %>% count(crm_cd_desc, sort = TRUE) %>% head(3)
print(top_crimes)

crime_by_hour <- df_cleaned %>% count(hour)
print(crime_by_hour)

victim_summary <- df_cleaned %>%
  group_by(vict_sex) %>%
  summarise(total_crimes = n(),
            average_victim_age = mean(vict_age, na.rm = TRUE))
print(victim_summary)

if (!"weapon_used_cd" %in% colnames(df_cleaned)) {
  warning("weapon_used_cd column not found. Severity score calculation may be incomplete.")
}
df_cleaned <- df_cleaned %>%
  mutate(severity_score = case_when(
    "weapon_used_cd" %in% names(df_cleaned) & !is.na(weapon_used_cd) ~ 5,
    tolower(crm_cd_desc) == "burglary" ~ 3,
    TRUE ~ 1
  ))

severity_by_area <- df_cleaned %>%
  group_by(area_name) %>%
  summarise(total_severity_score = sum(severity_score))
print(severity_by_area)

