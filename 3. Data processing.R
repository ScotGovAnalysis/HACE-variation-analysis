## Formatting publication tables ##
# Specify what columns to be treated as numeric #
numeric_cols <- c(
  "Number of Responses",
  "Percentage",
  "Lower 95% Confidence Interval",
  "Upper 95% Confidence Interval"
)

#function made to sort columns to correct type
clean_types <- function(df) {
  num_cols_present <- intersect(names(df), numeric_cols)
  
  df |>
    mutate(
      # Convert specified columns to numeric
      across(all_of(num_cols_present), as.numeric),
      # Convert all columns not specified as numeric to be factors
      across(-all_of(num_cols_present), as.factor)
    )
}

data_list_demographics_clean <- file_path_demographics |>
  # Get the sheet names from the published workbook
  excel_sheets() |>
  #Read all the sheets in except the cover page, in this case its called background
  (\(sheets) sheets[sheets != "Background"])() |>
  # Read all sheets from the published workbook into a named list
  set_names() |>
  map(\(sheet) read_excel(file_path_demographics, sheet = sheet)) |> 
  map(clean_types)

list2env(data_list_demographics_clean, envir = .GlobalEnv)

data_list_geographies_clean <- file_path_geographies |>
  # Get the sheet names from the published workbook
  excel_sheets() |>
  #Read all the sheets in except the cover page, in this case its called background
  (\(sheets) sheets[sheets != "Background"])() |>
  # Read all sheets from the published workbook into a named list
  set_names() |>
  map(\(sheet) read_excel(file_path_geographies, sheet = sheet)) |> 
  map(clean_types)

list2env(data_list_geographies_clean, envir = .GlobalEnv)

Scotland_characteristics_joined <- Scotland |> 
  mutate(
    "Sex"= "Scotland Total",
    "Age Band" = "Scotland Total",
    "Scottish Index of Multiple Deprivation Decile"= "Scotland Total",
    "Urban-Rural 8-fold classification" = "Scotland Total",
    "By Question Text" = "Scotland Total",
    "By Question Response Option" = "Scotland Total"
  ) %>% 
  mutate(
    `By Question Text` = as.factor(`By Question Text`),
    `By Question Response Option` = as.factor(`By Question Response Option`)
  )


Age_band_joined <- bind_rows(
  Scotland_characteristics_joined %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Age Band`, Percentage),
  `Age Band` %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Age Band`, Percentage)
)

Sex_joined <- bind_rows(
  Scotland_characteristics_joined %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Sex`, Percentage),
  `Sex` %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Sex`, Percentage)
)

SIMD_joined <- bind_rows(
  Scotland_characteristics_joined %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Scottish Index of Multiple Deprivation Decile`, Percentage),
  `SIMD` %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Scottish Index of Multiple Deprivation Decile`, Percentage)
)

Urban_Rural_8_joined <- bind_rows(
  Scotland_characteristics_joined %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Urban-Rural 8-fold classification`, Percentage),
  `Urban-Rural 8` %>%
    select(`Question Number`, `Question Text`, `Response Option`, `Urban-Rural 8-fold classification`, Percentage)
)

#Check the variables are correct type
glimpse(HSCP)
glimpse(SIMD)
glimpse(Scotland)
glimpse(Ethnicity_joined)
glimpse(Sexual_Orientation_joined)
