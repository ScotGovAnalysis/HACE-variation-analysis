# Location of desk instructions:
# \\scotland.gov.uk\dc1\DCGroup_SA1\Dept. of Health\Linked Spreadsheets\Analytical Services (HD)\Sitrep Returns\Primary Care\HACE
library(readxl)
library(dplyr)
library(writexl)
library(purrr)
library(rvest)
library(stringr)
#==============================================================================
# ANNUAL UPDATE SECTION - update the below each year
#==============================================================================
survey_year <- "2025-26"

# Paste the URL of the new publication pages on gov.scot
# Data by geography
geography_url <- "https://www.gov.scot/publications/health-and-care-experience-survey-2025-to-2026-results-by-geographical-area/"
# Data by deopgraphic
demographic_url <-"https://www.gov.scot/publications/health-and-care-experience-survey-2025-to-2026-all-results-by-demographic-characteristic/"


#If the question number has changed add new number here
question_easy_contact <- "q03" #How easy is it for you to contact your GP practice in the way that you want?
question_overall_care <- "q13" #Overall, how would you rate the care provided by your GP Practice?
question_informed_choice <- "q16m" #'I felt able to make an informed choice about my treatment and care'
question_OOH_care <- "q24c" #'I was treated with compassion and understanding' during A&E or GP Out of Hours care.
question_OOH_overall_care <- "q25" #Overall care recieved during OOH care
#==============================================================================
## End of ANNUAL UPDATE SECTION
#==============================================================================

# Read current master
master_data_all <- readRDS(
  "Clean data/master_data_all.rds"
)

# Download publication workbooks
download_publication_workbook <- function(page_url, output_file) {
  
  page <- read_html(page_url)
  
  links <- page |>
    html_elements("a") |>
    html_attr("href") |>
    na.omit()
  
  excel_url <- links |>
    str_subset("\\.xlsx$") |>
    dplyr::first()
  
  # Convert relative URL to absolute URL
  if (!str_detect(excel_url, "^https?://")) {
    excel_url <- paste0("https://www.gov.scot", excel_url)
  }
  
  # Create folder if it doesn't exist
  dir.create(
    dirname(output_file),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  download.file(
    excel_url,
    output_file,
    mode = "wb"
  )
  
  output_file
}

geo_file <- download_publication_workbook(
  page_url = geography_url,
  output_file = paste0(
    "Raw data/HACE-Geographies-",
    survey_year,
    ".xlsx"
  )
)

demo_file <- download_publication_workbook(
  page_url = demographic_url,
  output_file = paste0(
    "Raw data/HACE-Demographics-",
    survey_year,
    ".xlsx"
  )
)

## Question lookup saving 
question_lookup_year <- tibble::tribble(
  ~question_type,      ~survey_year, ~question_number,
  
  "easy_contact",      "2017-18",    "03",
  "easy_contact",      "2019-20",    "03",
  "easy_contact",      "2021-22",    "3",
  "easy_contact",      "2023-24",    "q03",
  "easy_contact",      "2025-26",    "q03",
  
  "overall_care",      "2009-11",    "10",
  "overall_care",      "2011-12",    "10",
  "overall_care",      "2013-14",    "10",
  "overall_care",      "2015-16",    "10",
  "overall_care",      "2017-18",    "10",
  "overall_care",      "2019-20",    "10",
  "overall_care",      "2021-22",    "10",
  "overall_care",      "2023-24",    "q13",
  "overall_care",      "2025-26",    "q13",
  
  "informed_choice",   "2021-22",    "13l",
  "informed_choice",   "2023-24",    "q16m",
  "informed_choice",   "2025-26",    "q16m",
  
  "OOH_care",          "2017-18",    "20",
  "OOH_care",          "2019-20",    "20",
  "OOH_care",          "2021-22",    "26c",
  "OOH_care",          "2023-24",    "q24c",
  "OOH_care",          "2025-26",    "q24c",  
  
  "OOH_overall_care",          "2017-18",    "q22",
  "OOH_overall_care",          "2019-20",    "q21",
  "OOH_overall_care",          "2021-22",    "q27",
  "OOH_overall_care",          "2023-24",    "q25",
  "OOH_overall_care",          "2025-26",    "q25"
)

saveRDS(
  question_lookup_year,
  "Clean data/question_lookup_year.rds"
)

new_question_lookup <- tibble(
  question_type = c(
    "easy_contact",
    "overall_care",
    "informed_choice",
    "OOH_care",
    "OOH_overall_care"
  ),
  survey_year = survey_year,
  question_number = c(
    question_easy_contact,
    question_overall_care,
    question_informed_choice,
    question_OOH_care,
    question_OOH_overall_care
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

geo_sheets <- excel_sheets(geo_file)
geo_sheets <- geo_sheets[geo_sheets != "Background"]

new_geography <- map_dfr(
  geo_sheets,
  function(sheet) {
    
    message("Processing: ", sheet)
    
    df <- read_excel(
      geo_file,
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

import_demographics <- function(demo_file, survey_year) {
  
  sex_data <- read_excel(
    demo_file,
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
    demo_file,
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
    demo_file,
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
    demo_file,
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
    demo_file,
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
    demo_file,
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
    demo_file,
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
    demo_file,
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
  demo_file,
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


