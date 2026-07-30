### Update these each year ##
#==============================================================================
# ANNUAL UPDATE SECTION
#==============================================================================
library(readxl)
library(dplyr)
library(writexl)

survey_year <- "2025-26"

# Download the most recent publication tables from https://www.gov.scot/collections/health-and-care-experience-survey/ #
#Save them to the Raw data folder and update the file paths below#
file_path_geographies <- "Raw data/HACE+2025+-+2026+-+All+results+by+Geography.xlsx"
file_path_demographics <- "Raw data/HACE+2025+-+2026+-+All+results+by+Demographic+Characteristic.xlsx"

#If the question number has changed add new number here
question_easy_contact <- "q03" #How easy is it for you to contact your GP practice in the way that you want?
question_overall_care <- "q13" #Overall, how would you rate the care provided by your GP Practice?
question_informed_choice <- "q16m" #'I felt able to make an informed choice about my treatment and care'
question_OOH_care <- "q24c" #'I was treated with compassion and understanding' during A&E or GP Out of Hours care.

# Read current master
master_data_all <- readRDS(
  "Clean data/master_data_all.rds"
)

#==============================================================================
## Question lookup saving 
question_lookup_year <- readRDS(
  "Clean data/question_lookup_year.rds"
)

new_question_lookup <- tibble(
  question_type = c(
    "easy_contact",
    "overall_care",
    "informed_choice",
    "OOH_care"
  ),
  survey_year = survey_year,
  question_number = c(
    question_easy_contact,
    question_overall_care,
    question_informed_choice,
    question_OOH_care
  )
)

question_lookup_year <- bind_rows(
  question_lookup_year,
  new_question_lookup
) %>%
  distinct()

saveRDS(
  question_lookup_year,
  "Clean data/question_lookup_year.rds"
)
#==============================================================================

master_cols <- names(master_data_all)

standardise_geography <- function(df) {
  
  # Add year
  df$Year <- survey_year
  
  # Scotland sheet has neither Area Type nor Area
  if (!"Area Type" %in% names(df)) {
    df$`Area Type` <- "Country"
  }
  
  if (!"Area" %in% names(df)) {
    df$Area <- "Scotland"
  }
  
  # GP Practice sheet:
  # use GP Practice name as Area
  if ("GP Practice name" %in% names(df)) {
    df$Area <- df$`GP Practice name`
  }
  
  # Fill any blank Area Type / Area values
  df$`Area Type`[is.na(df$`Area Type`) | df$`Area Type` == ""] <- "Country"
  df$Area[is.na(df$Area) | df$Area == ""] <- "Scotland"
  
  # Geography data represents totals
  df$Sex <- "Total"
  df$`Age Band` <- "Total"
  df$SIMD <- "Total"
  df$`Urban-Rural 8` <- "Total"
  df$`Long-Term Condition` <- "Total"
  df$`Chronic Pain` <- "Total"
  df$`Sexual Orientation` <- "Total"
  df$Ethnicity <- "Total"
  
  # Add any other columns required by master_data_all
  missing_cols <- setdiff(master_cols, names(df))
  
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  # Keep ONLY the columns that exist in master_data_all
  df <- df[, master_cols]
  
  df
}

geo_sheets <- excel_sheets(file_path_geographies)
geo_sheets <- geo_sheets[geo_sheets != "Background"]

new_geography <- map_dfr(
  geo_sheets,
  function(sheet) {
    
    message("Processing: ", sheet)
    
    df <- read_excel(
      file_path_geographies,
      sheet = sheet
    )
    
    standardise_geography(df)
    
  }
)


master_data_all <- bind_rows(
  master_data_all,
  new_geography
) %>%
  distinct()
#==============================================================================
# Function to import demographic workbook
#==============================================================================

import_demographics <- function(file_path_demographics,
                                survey_year) {
  
  sex_data <- read_excel(
    file_path_demographics,
    sheet = "Sex"
  ) %>%
    transmute(
      Year = survey_year,
      `Question Number`,
      Topic,
      `Question Text`,
      `Response Option`,
      `Area Type` = "Country",
      Area = "Scotland",
      Sex,
      `Age Band` = "Total",
      SIMD = "Total",
      `Urban-Rural 8` = "Total",
      `Long-Term Condition` = "Total",
      `Chronic Pain` = "Total",
      `Sexual Orientation` = "Total",
      Ethnicity = "Total",
      `Number of Responses`,
      Percentage,
      `Lower 95% Confidence Interval`,
      `Upper 95% Confidence Interval`
    )
  
  age_band_data <- read_excel(
    file_path_demographics,
    sheet = "Age Band"
  ) %>%
    transmute(
      Year = survey_year,
      `Question Number`,
      Topic,
      `Question Text`,
      `Response Option`,
      `Area Type` = "Country",
      Area = "Scotland",
      Sex = "Total",
      `Age Band`,
      SIMD = "Total",
      `Urban-Rural 8` = "Total",
      `Long-Term Condition` = "Total",
      `Chronic Pain` = "Total",
      `Sexual Orientation` = "Total",
      Ethnicity = "Total",
      `Number of Responses`,
      Percentage,
      `Lower 95% Confidence Interval`,
      `Upper 95% Confidence Interval`
    )
  
  simd_data <- read_excel(
    file_path_demographics,
    sheet = "SIMD"
  ) %>%
    transmute(
      Year = survey_year,
      `Question Number`,
      Topic,
      `Question Text`,
      `Response Option`,
      `Area Type` = "Country",
      Area = "Scotland",
      Sex = "Total",
      `Age Band` = "Total",
      SIMD = `Scottish Index of Multiple Deprivation Decile`,
      `Urban-Rural 8` = "Total",
      `Long-Term Condition` = "Total",
      `Chronic Pain` = "Total",
      `Sexual Orientation` = "Total",
      Ethnicity = "Total",
      `Number of Responses`,
      Percentage,
      `Lower 95% Confidence Interval`,
      `Upper 95% Confidence Interval`
    )
  
  urban_rural_data <- read_excel(
    file_path_demographics,
    sheet = "Urban-Rural 8"
  ) %>%
    transmute(
      Year = survey_year,
      `Question Number`,
      Topic,
      `Question Text`,
      `Response Option`,
      `Area Type` = "Country",
      Area = "Scotland",
      Sex = "Total",
      `Age Band` = "Total",
      SIMD = "Total",
      `Urban-Rural 8` = `Urban-Rural 8-fold classification`,
      `Long-Term Condition` = "Total",
      `Chronic Pain` = "Total",
      `Sexual Orientation` = "Total",
      Ethnicity = "Total",
      `Number of Responses`,
      Percentage,
      `Lower 95% Confidence Interval`,
      `Upper 95% Confidence Interval`
    )
  
  long_term_condition_data <- read_excel(
    file_path_demographics,
    sheet = "Long-Term Condition"
  ) %>%
    transmute(
      Year = survey_year,
      `Question Number`,
      Topic,
      `Question Text`,
      `Response Option`,
      `Area Type` = "Country",
      Area = "Scotland",
      Sex = "Total",
      `Age Band` = "Total",
      SIMD = "Total",
      `Urban-Rural 8` = "Total",
      `Long-Term Condition` = `By Question Response Option`,
      `Chronic Pain` = "Total",
      `Sexual Orientation` = "Total",
      Ethnicity = "Total",
      `Number of Responses`,
      Percentage,
      `Lower 95% Confidence Interval`,
      `Upper 95% Confidence Interval`
    )
  
  chronic_pain_data <- read_excel(
    file_path_demographics,
    sheet = "Chronic Pain"
  ) %>%
    transmute(
      Year = survey_year,
      `Question Number`,
      Topic,
      `Question Text`,
      `Response Option`,
      `Area Type` = "Country",
      Area = "Scotland",
      Sex = "Total",
      `Age Band` = "Total",
      SIMD = "Total",
      `Urban-Rural 8` = "Total",
      `Long-Term Condition` = "Total",
      `Chronic Pain` = `By Question Response Option`,
      `Sexual Orientation` = "Total",
      Ethnicity = "Total",
      `Number of Responses`,
      Percentage,
      `Lower 95% Confidence Interval`,
      `Upper 95% Confidence Interval`
    )
  
  sexual_orientation_data <- read_excel(
    file_path_demographics,
    sheet = "Sexual Orientation"
  ) %>%
    transmute(
      Year = survey_year,
      `Question Number`,
      Topic,
      `Question Text`,
      `Response Option`,
      `Area Type` = "Country",
      Area = "Scotland",
      Sex = "Total",
      `Age Band` = "Total",
      SIMD = "Total",
      `Urban-Rural 8` = "Total",
      `Long-Term Condition` = "Total",
      `Chronic Pain` = "Total",
      `Sexual Orientation` = `By Question Response Option`,
      Ethnicity = "Total",
      `Number of Responses`,
      Percentage,
      `Lower 95% Confidence Interval`,
      `Upper 95% Confidence Interval`
    )
  
  ethnicity_data <- read_excel(
    file_path_demographics,
    sheet = "Ethnicity"
  ) %>%
    transmute(
      Year = survey_year,
      `Question Number`,
      Topic,
      `Question Text`,
      `Response Option`,
      `Area Type` = "Country",
      Area = "Scotland",
      Sex = "Total",
      `Age Band` = "Total",
      SIMD = "Total",
      `Urban-Rural 8` = "Total",
      `Long-Term Condition` = "Total",
      `Chronic Pain` = "Total",
      `Sexual Orientation` = "Total",
      Ethnicity = `By Question Response Option`,
      `Number of Responses`,
      Percentage,
      `Lower 95% Confidence Interval`,
      `Upper 95% Confidence Interval`
    )
  
  bind_rows(
    sex_data,
    age_band_data,
    simd_data,
    urban_rural_data,
    long_term_condition_data,
    chronic_pain_data,
    sexual_orientation_data,
    ethnicity_data
  )
}


# Import new demographics
new_demographics <- import_demographics(
  file_path_demographics,
  survey_year
)


# Append
master_data_all <- bind_rows(
  master_data_all,
  new_demographics
) %>%
  distinct()

## Save to master Rds
saveRDS(
  master_data_all,
  "Clean data/master_data_all.rds"
)

# Save a back up dated with this survey year incase other master gets corrupted
saveRDS(
  master_data_all,
  paste0(
    "Clean data/master_data_all_",
    gsub("-", "_", survey_year),
    ".rds"
  )
)

## Optional save as excel file to inspect/QA
write_xlsx(
  master_data_all,
  "Clean data/master_data_all.xlsx"
)


