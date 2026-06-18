## Utility Script ##
# Download the most recent publication tables from https://www.gov.scot/collections/health-and-care-experience-survey/ #

#Load required packages#
library(readxl)
library(purrr)
library(dplyr)

# File path to the most recent years data #
# TO BE UPDATED WHEN STORAGE LOCATION DECIDED #
file_path_demographics <- "Setup documents - to be deleted once storage location decided/HACE+2025+-+2026+-+All+results+by+Demographic+Characteristic (1).xlsx"
file_path_geographies <- "Setup documents - to be deleted once storage location decided/HACE+2025+-+2026+-+All+results+by+Geography (2).xlsx"



