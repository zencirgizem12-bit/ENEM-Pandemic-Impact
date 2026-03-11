# --------------------------------------
# 06_robustness.R
# Robustness checks and alternative specifications
# --------------------------------------

library(data.table)

# Load panel data
enem_panel <- readRDS("data/processed/enem_panel_with_groups.rds")

# Filter 2019 and 2021 for comparison
did_data <- enem_panel[year %in% c(2019, 2021)]

# ======================================
# 1. DIFFERENT LOW-INCOME DEFINITIONS
# ======================================

cat("
========================================
")
cat("📊 ROBUSTNESS: DIFFERENT LOW-INCOME DEFINITIONS
")
cat("========================================
")

# Original: A, B, C
did_data[, low_orig := family_income %in% c("A", "B", "C")]

# Wide: A, B, C, D
did_data[, low_wide := family_income %in% c("A", "B", "C", "D")]

# Narrow: A, B
did_data[, low_narrow := family_income %in% c("A", "B")]

# Very narrow: A only
did_data[, low_very_narrow := family_income == "A"]

# Test each definition
definitions <- c("orig", "wide", "narrow", "very_narrow")
results_low <- data.table()

for(def in definitions) {
  low_var <- paste0("low_", def)
  
  # 2019 gap
  gap_2019 <- did_data[year == 2019, 
                       mean(math_score[get(low_var) == FALSE], na.rm = TRUE) - 
                       mean(math_score[get(low_var) == TRUE], na.rm = TRUE)]
  
  # 2021 gap
  gap_2021 <- did_data[year == 2021, 
                       mean(math_score[get(low_var) == FALSE], na.rm = TRUE) - 
                       mean(math_score[get(low_var) == TRUE], na.rm = TRUE)]
  
  # Change
  change <- gap_2021 - gap_2019
  
  results_low <- rbind(results_low, data.table(
    Definition = def,
    Gap_2019 = round(gap_2019, 1),
    Gap_2021 = round(gap_2021, 1),
    Change = round(change, 1)
  ))
}

print(results_low)
fwrite(results_low, "output/tables/robustness_lowincome.csv")

cat("
✅ Robustness checks complete!
")

