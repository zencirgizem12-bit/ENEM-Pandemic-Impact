# --------------------------------------
# 04_analysis.R
# Main analysis: DiD, learning loss, inequality
# --------------------------------------

library(data.table)
library(fixest)
library(ggplot2)

# Load panel data with groups
enem_panel <- readRDS("data/processed/enem_panel_with_groups.rds")

# ======================================
# 1. MAIN DiD ANALYSIS
# ======================================

cat("
========================================
")
cat("📊 MAIN DiD: PANDEMIC EFFECT BY INCOME GROUP
")
cat("========================================
")

# Create treatment indicators
enem_panel[, low_income := income_group == "Low"]

# Model with period and income interaction
did_main <- feols(math_score ~ i(period, low_income, ref = "Pre-Pandemic") | 
                    city_code + year, 
                  data = enem_panel)

summary(did_main)

# Save results
saveRDS(did_main, "output/results/did_main_model.rds")

# ======================================
# 2. LEARNING LOSS (MATH VS LANGUAGE)
# ======================================

cat("
========================================
")
cat("📊 LEARNING LOSS: MATH vs LANGUAGE
")
cat("========================================
")

learning_loss <- enem_panel[, .(
  math_mean = mean(math_score, na.rm = TRUE),
  lang_mean = mean(language_score, na.rm = TRUE),
  gap = mean(math_score, na.rm = TRUE) - mean(language_score, na.rm = TRUE)
), by = .(period, income_group)]

print(learning_loss)
fwrite(learning_loss, "output/tables/learning_loss_by_period.csv")

# ======================================
# 3. INEQUALITY DYNAMICS
# ======================================

cat("
========================================
")
cat("📊 INEQUALITY DYNAMICS (P90-P10 Gap)
")
cat("========================================
")

inequality <- enem_panel[, .(
  p10 = quantile(math_score, 0.1, na.rm = TRUE),
  p90 = quantile(math_score, 0.9, na.rm = TRUE),
  gap = quantile(math_score, 0.9, na.rm = TRUE) - quantile(math_score, 0.1, na.rm = TRUE)
), by = .(year, period)]

print(inequality)
fwrite(inequality, "output/tables/inequality_by_year.csv")

# ======================================
# 4. DIGITAL DIVIDE
# ======================================

cat("
========================================
")
cat("📊 DIGITAL DIVIDE BY INCOME GROUP
")
cat("========================================
")

digital_gap <- enem_panel[!is.na(internet_group), .(
  with_internet = mean(math_score[internet_group == "Has Internet"], na.rm = TRUE),
  without_internet = mean(math_score[internet_group == "No Internet"], na.rm = TRUE),
  gap = mean(math_score[internet_group == "Has Internet"], na.rm = TRUE) - 
        mean(math_score[internet_group == "No Internet"], na.rm = TRUE)
), by = .(period, income_group)]

print(digital_gap)
fwrite(digital_gap, "output/tables/digital_gap_by_period.csv")

cat("
✅ Analysis complete! Results saved to output/tables/ and output/results/
")

