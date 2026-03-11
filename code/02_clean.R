# --------------------------------------
# 02_clean.R
# Clean and process raw ENEM data
# --------------------------------------

library(data.table)

# Function to clean a single year's ENEM data
clean_enem <- function(year, raw_folder = "data/raw", processed_folder = "data/processed") {
  
  # Create processed folder if it doesn't exist
  dir.create(processed_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Check if zip file exists
  zip_file <- file.path(raw_folder, paste0("enem_", year, ".zip"))
  if (!file.exists(zip_file)) {
    cat("❌ Zip file not found for", year, "
")
    return(NULL)
  }
  
  # Unzip
  cat("Unzipping ENEM", year, "...
")
  unzip_dir <- file.path(raw_folder, paste0("enem_", year))
  unzip(zip_file, exdir = unzip_dir)
  
  # Find CSV file
  csv_files <- list.files(unzip_dir, pattern = "\.csv$", recursive = TRUE, full.names = TRUE)
  
  # Usually the main data file is the largest or contains "MICRODADOS"
  csv_file <- csv_files[grepl("MICRODADOS", csv_files, ignore.case = TRUE)]
  if (length(csv_file) == 0) {
    # If no file with "MICRODADOS", take the largest
    file_sizes <- file.size(csv_files)
    csv_file <- csv_files[which.max(file_sizes)]
  }
  
  cat("Reading data for", year, "...
")
  
  # Determine encoding (Latin-1 for 2017-2019, UTF-8 for 2020+)
  encoding <- ifelse(year <= 2019, "Latin-1", "UTF-8")
  
  # Read data
  data <- fread(csv_file[1], encoding = encoding, showProgress = TRUE)
  
  cat("✅ Data loaded:", format(nrow(data), big.mark = ","), "rows,", ncol(data), "columns
")
  
  # Save as RDS (compressed, faster to load later)
  rds_file <- file.path(processed_folder, paste0("enem_", year, "_raw.rds"))
  saveRDS(data, rds_file)
  cat("✅ Saved:", basename(rds_file), "
")
  
  return(data)
}

# Clean all years
years <- 2017:2024
for (year in years) {
  clean_enem(year)
}

cat("
✅ All data cleaned and saved as RDS!
")

