# ----------------------------
# Install required packages (if not already installed)
# ----------------------------
packages <- c("dplyr", "jsonlite", "lubridate", "stringr", "magrittr", "R6")
for(pkg in packages){
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org/")
    library(pkg, character.only = TRUE)
  }
}

# ----------------------------
# Load libraries
# ----------------------------
library(lubridate)
library(jsonlite)
library(dplyr)
library(stringr)
library(magrittr)
library(R6)

# ----------------------------
# Source project scripts
# ----------------------------
source("Project_3_R/cleanup.R")
source("Project_3_R/datafiller.R")
source("Project_3_R/dataProcessingnumberdate.R")
source("Project_3_R/aggrigation.R")

# ----------------------------
# Read, clean, and fill data
# ----------------------------
clean_data_table <- clean_data("Project_3_R/datasets/large_data.json") %>%
  data_filler()

# ----------------------------
# Process numbers, dates, and aggregations
# ----------------------------
numberTable        <- number_proc(clean_data_table)
Date_tables        <- date_proc(clean_data_table)
aggregation_tables <- aggregation_general(clean_data_table)

# ----------------------------
# Write output CSVs
# ----------------------------
write.csv(numberTable, "Project_3_R/dataOutput/number_data.csv", row.names = FALSE)


output_dir <- "Project_3_R/dataOutput"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


# Write date tables
for (name in names(Date_tables)) {
  file_path <- file.path(output_dir, paste0(name, ".csv"))
  write.csv(Date_tables[[name]], file_path, row.names = FALSE)
  message("Saved: ", file_path)
}

# Write aggregation tables
for (name in names(aggregation_tables)) {
  file_path <- file.path(output_dir, paste0(name, ".csv"))
  write.csv(aggregation_tables[[name]], file_path, row.names = FALSE)
  message("Saved: ", file_path)
}
