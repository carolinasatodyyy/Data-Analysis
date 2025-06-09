# R Script: 01_cell_viability.R
# Purpose: Compare cell counts between untreated and saponin-permeabilized A549 cells

# ────────────────────────────────────────────────────────────────
# Load Libraries
# ────────────────────────────────────────────────────────────────
library(ggplot2)
library(dplyr)
library(ggpubr)

# ────────────────────────────────────────────────────────────────
# 1. Raw Cell Counts
# ────────────────────────────────────────────────────────────────
untreated     <- c(18722, 9691, 18667, 18969, 17759, 18894)
permeabilized <- c(19575, 13344, 15442, 15554, 18929, 18940)

data <- data.frame(
  Condition = factor(rep(c("Untreated", "Permeabilized"), each = 6),
                     levels = c("Untreated", "Permeabilized")),
  CellCount = c(untreated, permeabilized)
)

# ────────────────────────────────────────────────────────────────
# 2. F-test for Equal Variances
# ────────────────────────────────────────────────────────────────
cat("F-test for equal variances:\n")
variance_test <- var.test(CellCount ~ Condition, data = data)
print(variance_test)

# ────────────────────────────────────────────────────────────────
# 3. Summary Statistics (Mean ± SEM)
# ────────────────────────────────────────────────────────────────
summary_stats <- data %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(CellCount),
    SD   = sd(CellCount),
    SEM  = SD / sqrt(n()),
    .groups = "drop"
  )
print("Summary Statistics:")
print(summary_stats)

# ────────────────────────────────────────────────────────────────
# 4. Bar Plot with Student’s t-test Annotation
# ────────────────────────────────────────────────────────────────
y_bracket <- max(data$CellCount) + 1000

ggplot(data, aes(x = Condition, y = CellCount, fill = Condition)) +
  geom_bar(stat = "summary", fun = mean, width = 0.6, colour = "black") +
  geom_errorbar(
    data = summary_stats,
    aes(y = Mean, ymin = Mean - SEM, ymax = Mean + SEM),
    width = 0.2, colour = "black"
  ) +
  geom_jitter(width = 0.1, size = 3, shape = 21, fill = "white") +
  stat_compare_means(
    method      = "t.test",
    method.args = list(var.equal = TRUE),
    comparisons = list(c("Untreated", "Permeabilized")),
    label       = "p.signif",
    tip.length  = 0.02,
    label.y     = y_bracket,
    colour      = "black"
  ) +
  scale_fill_manual(
    values = c("Untreated" = "#009E73", "Permeabilized" = "#E69F00"),
    guide = "none"
  ) +
  labs(
    x = "",
    y = "Cell count"
  ) +
  theme_bw(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = "none",
    axis.title.x     = element_text(colour = "black", size = 20, face = "bold", margin = margin(t = 10)),
    axis.title.y     = element_text(colour = "black", size = 20, face = "bold", margin = margin(r = 10)),
    axis.text.x      = element_text(colour = "black", size = 14, face = "bold"),
    axis.text.y      = element_text(colour = "black", size = 14, face = "bold"),
    plot.title       = element_text(colour = "black", size = 18, face = "bold", hjust = 0.5)
  )
