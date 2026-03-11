# --------------------------------------
# 05_figures.R
# Generate all figures
# --------------------------------------

library(data.table)
library(ggplot2)

# Load panel data
enem_panel <- readRDS("data/processed/enem_panel_with_groups.rds")

# Create output folder
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# ======================================
# 1. DIGITAL DIVIDE FIGURE
# ======================================

cat("📊 Creating digital divide figure...
")

plot_digital <- enem_panel[!is.na(internet_group), .(
  math_mean = mean(math_score, na.rm = TRUE)
), by = .(year, income_group, internet_group)]

p1 <- ggplot(plot_digital, aes(x = year, y = math_mean, 
                               color = income_group, 
                               linetype = internet_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "orange", linewidth = 1) +
  facet_wrap(~income_group) +
  scale_color_manual(values = c("Low" = "red", "Middle" = "blue", "High" = "green")) +
  labs(title = "Digital Divide by Income Group",
       subtitle = "Red = Pandemic onset | Orange = Pandemic year",
       x = "Year", y = "Average Math Score") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/digital_divide_by_income.png", p1, width = 14, height = 8, dpi = 300)

# ======================================
# 2. INEQUALITY TREND FIGURE
# ======================================

cat("📊 Creating inequality trend figure...
")

inequality <- enem_panel[, .(
  gap = quantile(math_score, 0.9, na.rm = TRUE) - quantile(math_score, 0.1, na.rm = TRUE)
), by = year]

p2 <- ggplot(inequality, aes(x = year, y = gap)) +
  geom_line(linewidth = 1.2, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = 2020.5, linetype = "dashed", color = "orange", linewidth = 1) +
  labs(title = "Inequality Dynamics (P90-P10 Gap)",
       x = "Year", y = "Score Gap") +
  theme_minimal()

ggsave("output/figures/inequality_trend.png", p2, width = 10, height = 6, dpi = 300)

cat("
✅ Figures saved to output/figures/
")

