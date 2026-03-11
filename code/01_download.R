# --------------------------------------
# 01_download.R
# Download ENEM microdata from INEP website
# --------------------------------------

# This script downloads raw ENEM data for 2017-2024
# Note: Raw data files are large (~2GB each) and not included in repo

library(data.table)

# Function to download ENEM data for a given year
download_enem <- function(year, dest_folder = "data/raw") {
  
  # Create destination folder if it doesn't exist
  dir.create(dest_folder, recursive = TRUE, showWarnings = FALSE)
  
  # URL pattern for ENEM data
  url <- paste0("https://download.inep.gov.br/microdados/microdados_enem_", year, ".zip")
  dest_file <- file.path(dest_folder, paste0("enem_", year, ".zip"))
  
  cat("Downloading ENEM", year, "...
")
  
  # Download file
  tryCatch({
    download.file(url, dest_file, mode = "wb")
    cat("✅ Downloaded:", basename(dest_file), "
")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Error downloading", year, ":", e$message, "
")
    return(FALSE)
  })
}

# Download years 2017-2024
years <- 2017:2024
for (year in years) {
  download_enem(year)
}

cat("
✅ All downloads completed!
")

