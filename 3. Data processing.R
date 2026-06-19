## Formatting publication tables ##

data_list_demographics <- file_path_demographics |>
  # Get the sheet names from the published workbook
  excel_sheets() |>
  #Read all the sheets in except the cover page, in this case its called background
  (\(sheets) sheets[sheets != "Background"])() |>
  # Read all sheets from the published workbook into a named list
  set_names() |>
  map(\(sheet) read_excel(file_path_demographics, sheet = sheet)) |> 
  map(clean_types) |> 
  #Unpack the list of data sets into the environment with same names as their tabs from the published workbook
  list2env(data_list_demographics, envir = .GlobalEnv)



data_list_geographies <- file_path_geographies |>
  # Get the sheet names from the published workbook
  excel_sheets() |>
  #Read all the sheets in except the cover page, in this case its called background
  (\(sheets) sheets[sheets != "Background"])() |>
  # Read all sheets from the published workbook into a named list
  set_names() |>
  map(\(sheet) read_excel(file_path_geographies, sheet = sheet)) |> 
  map(clean_types)  |> 
  #Unpack the list of data sets into the environment with same names as their tabs from the published workbook
  list2env(data_list_geographies, envir = .GlobalEnv)


# Specify what columns to be treated as numeric #
numeric_cols <- c(
  "Number of Responses",
  "Percentage",
  "Lower 95% Confidence Interval",
  "Upper 95% Confidence Interval"
)

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

#Check the conversion has worked
glimpse(HSCP)
glimpse(SIMD)
