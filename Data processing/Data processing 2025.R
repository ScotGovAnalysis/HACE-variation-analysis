##### THIS CODE DOES NOT NEED RERUN ###
# It is cleaning the data from early years upto 2025-26 #
# For the next HACE use Updates Script to append the new data # 

library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

master_data_2025_26 <- read_excel("Setup documents - to be deleted once storage location decided/Master data - Geography.xlsx")
SG_Practice_lookup <- readRDS("~/HSCA/HACE/HACE-variation-analysis/Setup documents - to be deleted once storage location decided/SG_Practice_lookup.rds")
survey_year <- "2025-26"
#------------------------------------------------------------------------------#
#### Appending 2023-24 data to master dataframe #####


pnn_data <- read_excel(
  file_path_geographies_2023,
  sheet = "Positive, Neutral or Negative"
)

pnn_tidy <- pnn_data %>%
  mutate(
    `Geography Type` = recode(
      `Geography Type`,
      "GP" = "GP Practice",
      "Scotland" = "Country",
      "GPCL" = "GP Cluster",
      "HSCP" = "Health and Social Care Partnership"
    ),
    
    `Percentage Neutral` = na_if(`Percentage Neutral`, "N/A"),
    
    across(
      c(
        `Percentage Positive`,
        `Percentage Neutral`,
        `Percentage Negative`
      ),
      as.numeric
    ),
    
    across(
      c(
        `Percentage Positive`,
        `Percentage Neutral`,
        `Percentage Negative`,
        `Lower 95% Confidence Interval - Percentage Positive`,
        `Upper 95% Confidence Interval - Percentage Positive`
      ),
      ~ . * 100
    )
  ) %>%
  pivot_longer(
    cols = c(
      `Percentage Positive`,
      `Percentage Neutral`,
      `Percentage Negative`
    ),
    names_to = "response_type",
    values_to = "Percentage"
  ) %>% 
  
  mutate(
    Year = "2023-24",
    
    `Response Option` = case_when(
      response_type == "Percentage Positive" ~ "positive",
      response_type == "Percentage Neutral" ~ "neutral",
      response_type == "Percentage Negative" ~ "negative"
    ),
    
    `Lower 95% Confidence Interval` = if_else(
      `Response Option` == "positive",
      `Lower 95% Confidence Interval - Percentage Positive`,
      NA_real_
    ),
    
    `Upper 95% Confidence Interval` = if_else(
      `Response Option` == "positive",
      `Upper 95% Confidence Interval - Percentage Positive`,
      NA_real_
    )
  ) %>%
  
  transmute(
    Year,
    `Question Number`,
    Topic = `Survey Section`,
    `Question Text`,
    `Response Option`,
    `By Question Text` = NA_character_,
    `By Question Response Option` = NA_character_,
    `Area Type` = `Geography Type`,
    Area = `Area Name`,
    `Number of Responses`,
    Percentage,
    `Lower 95% Confidence Interval`,
    `Upper 95% Confidence Interval`
  ) %>%
  filter(!is.na(Percentage)) %>%
  mutate(
    Topic = recode(
      Topic,
      "General Practice" = "Your GP Practice",
      "OOH" = "Out of Hours Healthcare",
      "Treatment or Advice from your General Practice" =
        "Treatment or Advice from your GP Practice",
      "Caring responsibilities" = "Caring Responsibilities"
    )
  )



info_data <- read_excel(
  file_path_geographies_2023,
  sheet = "Information Questions"
)

info_tidy <- info_data %>%
  mutate(
    Year = "2023-24",
    
    `Geography Type` = recode(
      `Geography Type`,
      "GP" = "GP Practice",
      "Scotland" = "Country",
      "GPCL" = "GP Cluster",
      "HSCP" = "Health and Social Care Partnership"
    ),
    
    `Percentage selecting this response option` =
      as.numeric(`Percentage selecting this response option`) * 100
  ) %>%
  
  transmute(
    Year,
    `Question Number`,
    Topic = `Survey Section`,
    `Question Text`,
    
    `Response Option` = `Response Option Text`,
    
    `By Question Text` = NA_character_,
    `By Question Response Option` = NA_character_,
    
    `Area Type` = `Geography Type`,
    Area = `Area Name`,
    
    `Number of Responses` = `Number of responses to question`,
    
    Percentage = `Percentage selecting this response option`,
    
    `Lower 95% Confidence Interval` = NA_real_,
    `Upper 95% Confidence Interval` = NA_real_
  ) %>%
  mutate(
    Topic = recode(
      Topic,
      "General Practice" = "Your GP Practice",
      "OOH" = "Out of Hours Healthcare",
      "Treatment or Advice from your General Practice" =
        "Treatment or Advice from your GP Practice",
      "Caring responsibilities" = "Caring Responsibilities"
    )
  )


master_data_2025_26 <- bind_rows(
  master_data_2025_26,
  pnn_tidy,
  info_tidy
)

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### Appending 2021-22 data to master dataframe #####
scotland_pnn_2021_22 <- `Scotland - PNN Questions` %>%

  pivot_longer(
    cols = c(`% Positive`, `% Neutral`, `% Negative`),
    names_to = "Response Option",
    values_to = "Percentage"
  ) %>%
  mutate(
    Year = "2021-22",
    
    `Response Option` = case_when(
      `Response Option` == "% Positive" ~ "positive",
      `Response Option` == "% Neutral" ~ "neutral",
      `Response Option` == "% Negative" ~ "negative"
    ),
    
    Percentage = as.numeric(as.character(Percentage))
  ) %>%
  transmute(
    Year,
    `Question Number`,
    Topic = as.character(`Questionnaire Section`),
    `Question Text` = as.character(`Question Text`),
    `Response Option`,
    `By Question Text` = NA_character_,
    `By Question Response Option` = NA_character_,
    `Area Type` = "Country",
    Area = "Scotland",
    `Number of Responses`,
    Percentage,
    `Lower 95% Confidence Interval` = NA_real_,
    `Upper 95% Confidence Interval` = NA_real_
  )

hb_pnn_2021_22 <- `HB - PNN Questions` %>%
  pivot_longer(
    cols = c(`% Positive`, `% Neutral`, `% Negative`),
    names_to = "Response Option",
    values_to = "Percentage"
  ) %>%
  mutate(
    Year = "2021-22",
    
    `Response Option` = case_when(
      `Response Option` == "% Positive" ~ "positive",
      `Response Option` == "% Neutral" ~ "neutral",
      `Response Option` == "% Negative" ~ "negative"
    ),
    
    Percentage = as.numeric(as.character(Percentage))
  ) %>%
  transmute(
    Year,
    `Question Number` = as.character(`Question Number`),
    Topic = as.character(`Questionnaire Section`),
    `Question Text` = as.character(`Question Text`),
    `Response Option`,
    `By Question Text` = NA_character_,
    `By Question Response Option` = NA_character_,
    `Area Type` = "Health Board",
    Area = as.character(`Health Board`),
    `Number of Responses`,
    Percentage,
    `Lower 95% Confidence Interval` = NA_real_,
    `Upper 95% Confidence Interval` = NA_real_
  )


hscp_pnn_2021_22 <- `HSCP - PNN Questions` %>%
  pivot_longer(
    cols = c(`% Positive`, `% Neutral`, `% Negative`),
    names_to = "Response Option",
    values_to = "Percentage"
  ) %>%
  mutate(
    Year = "2021-22",
    
    `Response Option` = case_when(
      `Response Option` == "% Positive" ~ "positive",
      `Response Option` == "% Neutral" ~ "neutral",
      `Response Option` == "% Negative" ~ "negative"
    ),
    
    Percentage = as.numeric(as.character(Percentage))
  ) %>%
  transmute(
    Year,
    `Question Number` = as.character(`Question Number`),
    Topic = as.character(`Questionnaire Section`),
    `Question Text` = as.character(`Question Text`),
    `Response Option`,
    `By Question Text` = NA_character_,
    `By Question Response Option` = NA_character_,
    `Area Type` = "Health and Social Care Partnership",
    Area = as.character(`HSCP Area`),
    `Number of Responses`,
    Percentage,
    `Lower 95% Confidence Interval` = NA_real_,
    `Upper 95% Confidence Interval` = NA_real_
  )

gpcluster_pnn_2021_22 <- `GP Cluster - PNN Questions` %>%
  pivot_longer(
    cols = c(`% Positive`, `% Neutral`, `% Negative`),
    names_to = "Response Option",
    values_to = "Percentage"
  ) %>%
  mutate(
    Year = "2021-22",
    
    `Response Option` = case_when(
      `Response Option` == "% Positive" ~ "positive",
      `Response Option` == "% Neutral" ~ "neutral",
      `Response Option` == "% Negative" ~ "negative"
    ),
    
    Percentage = as.numeric(as.character(Percentage))
  ) %>%
  transmute(
    Year,
    `Question Number` = as.character(`Question Number`),
    Topic = as.character(`Questionnaire Section`),
    `Question Text` = as.character(`Question Text`),
    `Response Option`,
    `By Question Text` = NA_character_,
    `By Question Response Option` = NA_character_,
    `Area Type` = "GP Cluster",
    Area = as.character(`GP Cluster`),
    `Number of Responses`,
    Percentage,
    `Lower 95% Confidence Interval` = NA_real_,
    `Upper 95% Confidence Interval` = NA_real_
  )

gp_pnn_2021_22 <- `GP - PNN Questions` %>%
  pivot_longer(
    cols = c(`% Positive`, `% Neutral`, `% Negative`),
    names_to = "Response Option",
    values_to = "Percentage"
  ) %>%
  mutate(
    Year = "2021-22",
    
    `Response Option` = case_when(
      `Response Option` == "% Positive" ~ "positive",
      `Response Option` == "% Neutral" ~ "neutral",
      `Response Option` == "% Negative" ~ "negative"
    ),
    
    Percentage = as.numeric(as.character(Percentage))
  ) %>%
  transmute(
    Year,
    `Question Number` = as.character(`Question Number`),
    Topic = as.character(`Questionnaire \r\nSection`),
    `Question Text` = as.character(`Question Text`),
    `Response Option`,
    `By Question Text` = NA_character_,
    `By Question Response Option` = NA_character_,
    `Area Type` = "GP Practice",
    Area = as.character(`GP Practice`),
    `Number of Responses`,
    Percentage,
    `Lower 95% Confidence Interval` = NA_real_,
    `Upper 95% Confidence Interval` = NA_real_
  )


pnn_2021_22 <- bind_rows(
  scotland_pnn_2021_22,
  hb_pnn_2021_22,
  hscp_pnn_2021_22,
  gpcluster_pnn_2021_22,
  gp_pnn_2021_22
)

master_data_2025_26 <- bind_rows(
  master_data_2025_26,
  pnn_2021_22
) %>%
  distinct()

#------------------------------------------------------------------------------#
master_data_all <- master_data_2025_26 %>%
  mutate(
    Sex = "Total",
    `Age Band` = "Total",
    SIMD = "Total",
    `Urban-Rural 8` = "Total",
    `Long-Term Condition` = "Total",
    `Chronic Pain` = "Total",
    `Sexual Orientation` = "Total",
    Ethnicity = "Total"
  ) %>%
  select(
    Year,
    `Question Number`,
    Topic,
    `Question Text`,
    `Response Option`,
    `Area Type`,
    Area,
    Sex,
    `Age Band`,
    SIMD,
    `Urban-Rural 8`,
    `Long-Term Condition`,
    `Chronic Pain`,
    `Sexual Orientation`,
    Ethnicity,
    `Number of Responses`,
    Percentage,
    `Lower 95% Confidence Interval`,
    `Upper 95% Confidence Interval`
  )
#------------------------------------------------------------------------------#

#### Appending 2025-26 demographic data to master dataframe #####
sex_2025_26 <- read_excel(
  file_path_demographics,
  sheet = "Sex"
) %>%
  transmute(
    Year = "2025-26",
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

age_band_2025_26 <- read_excel(
  file_path_demographics,
  sheet = "Age Band"
) %>%
  transmute(
    Year = "2025-26",
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


simd_2025_26 <- read_excel(
  file_path_demographics,
  sheet = "SIMD"
) %>%
  transmute(
    Year = "2025-26",
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

urban_rural_2025_26 <- read_excel(
  file_path_demographics,
  sheet = "Urban-Rural 8"
) %>%
  transmute(
    Year = "2025-26",
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

long_term_condition_2025_26 <- read_excel(
  file_path_demographics,
  sheet = "Long-Term Condition"
) %>%
  transmute(
    Year = "2025-26",
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

chronic_pain_2025_26 <- read_excel(
  file_path_demographics,
  sheet = "Chronic Pain"
) %>%
  transmute(
    Year = "2025-26",
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

sexual_orientation_2025_26 <- read_excel(
  file_path_demographics,
  sheet = "Sexual Orientation"
) %>%
  transmute(
    Year = "2025-26",
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

ethnicity_2025_26 <- read_excel(
  file_path_demographics,
  sheet = "Ethnicity"
) %>%
  transmute(
    Year = "2025-26",
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

demographics_2025_26 <- bind_rows(
  sex_2025_26,
  age_band_2025_26,
  simd_2025_26,
  urban_rural_2025_26,
  long_term_condition_2025_26,
  chronic_pain_2025_26,
  sexual_orientation_2025_26,
  ethnicity_2025_26
)

master_data_all <- bind_rows(
  master_data_all,
  demographics_2025_26
) %>%
  distinct()%>%
  mutate(
    Area = str_remove(
      Area,
      " \\([0-9]+\\)$"
    )
  )
#------------------------------------------------------------------------------#
## Save the master data as an RDS
saveRDS(
  master_data_all,
  "Data/master_data_all.rds"
)

# Save a back up dated with this survey year incase other master gets corrupted
saveRDS(
  master_data_all,
  paste0(
    "Data/master_data_all_",
    gsub("-", "_", survey_year),
    ".rds"
  )
)

## Optional save as excel file to inspect/QA
write_xlsx(
  master_data_all,
  "Data/master_data_all.xlsx"
)
##==============================================================================

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
  "OOH_care",          "2025-26",    "q24c"
)

saveRDS(
  question_lookup_year,
  "Clean data/question_lookup_year.rds"
)


