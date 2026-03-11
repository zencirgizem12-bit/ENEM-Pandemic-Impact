# --------------------------------------
# 03_create_variables.R
# Create derived variables (income groups, internet access, etc.)
# --------------------------------------

library(data.table)

# Function to create derived variables for a single year
create_variables <- function(year, processed_folder = "data/processed") {
  
  # Load raw data
  rds_file <- file.path(processed_folder, paste0("enem_", year, "_raw.rds"))
  if (!file.exists(rds_file)) {
    cat("❌ File not found for", year, "
")
    return(NULL)
  }
  
  data <- readRDS(rds_file)
  
  # Select relevant variables (matching our panel structure)
  clean_data <- data[, .(
    student_id = if("NU_INSCRICAO" %in% names(data)) NU_INSCRICAO else NA,
    math_score = NU_NOTA_MT,
    science_score = NU_NOTA_CN,
    human_score = NU_NOTA_CH,
    language_score = NU_NOTA_LC,
    essay_score = NU_NOTA_REDACAO,
    gender = TP_SEXO,
    age = NU_IDADE,
    state = SG_UF_PROVA,
    city_code = CO_MUNICIPIO_PROVA,
    city_name = NO_MUNICIPIO_PROVA,
    school_type = TP_DEPENDENCIA_ADM_ESC,
    school_location = TP_LOCALIZACAO_ESC,
    family_income = Q006,
    internet_access = Q022,
    computer_access = Q024,
    father_education = Q001,
    mother_education = Q002,
    math_presence = TP_PRESENCA_MT,
    science_presence = TP_PRESENCA_CN,
    human_presence = TP_PRESENCA_CH,
    language_presence = TP_PRESENCA_LC
  )]
  
  # Add year
  clean_data[, year := year]
  
  # Filter students who took all exams
  clean_data <- clean_data[math_presence == 1 & science_presence == 1 & 
                           human_presence == 1 & language_presence == 1]
  
  # Save cleaned data
  clean_file <- file.path(processed_folder, paste0("enem_", year, "_clean.rds"))
  saveRDS(clean_data, clean_file)
  cat("✅ Saved:", basename(clean_file), "-", format(nrow(clean_data), big.mark = ","), "students
")
  
  return(clean_data)
}

# Create variables for all years
years <- 2017:2024
all_data <- list()

for (year in years) {
  all_data[[as.character(year)]] <- create_variables(year)
}

# Combine into panel
cat("
🔄 Creating panel dataset...
")
enem_panel <- rbindlist(all_data, fill = TRUE, use.names = TRUE)

# Save panel
panel_file <- file.path("data/processed", "enem_panel_2017_2024.rds")
saveRDS(enem_panel, panel_file)
cat("✅ Panel saved:", basename(panel_file), "-", format(nrow(enem_panel), big.mark = ","), "students
")

# Create derived groups
cat("
🔧 Creating derived groups...
")

# Income groups
enem_panel[, income_group := fcase(
  family_income %in% c("A", "B", "C"), "Low",
  family_income %in% c("P"), "High",
  default = "Middle"
)]

# Internet groups
enem_panel[, internet_group := fcase(
  internet_access == "A", "Has Internet",
  internet_access == "B", "No Internet",
  default = NA_character_
)]

# School type groups
enem_panel[, school_group := fcase(
  school_type == 4, "Private",
  school_type %in% c(1, 2, 3), "Public",
  default = "Other"
)]

# Period groups
enem_panel[, period := fcase(
  year %in% c(2017, 2018, 2019), "Pre-Pandemic",
  year == 2020, "Pandemic",
  year %in% c(2021, 2022, 2023, 2024), "Post-Pandemic",
  default = NA_character_
)]

# Save panel with groups
panel_groups_file <- file.path("data/processed", "enem_panel_with_groups.rds")
saveRDS(enem_panel, panel_groups_file)
cat("✅ Panel with groups saved!
")

