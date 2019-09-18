library(readxl)
library(tidyverse)

# Load Phb2 liver GC-MS dataset
Phb2_Liver <- read_excel("data/GC_MS_Phb2_Liver.xlsx", col_types = c("text", "text", "text", "numeric", "numeric", "numeric"))
View(Phb2_Liver)

# Change to long format
Phb2_Liver_long <- Phb2_Liver %>%
  select(-c(Glucose)) %>%
  gather(Metabolite, Count, 4:5)

View(Phb2_Liver_long)

# Rank the weeks
Phb2_Liver_long$Weeks <- factor(Phb2_Liver_long$Weeks, levels = c("4", "5", "6", "10"))

# ggplot Phb2 liver fucose ------------------------------------------------
# Set standard theme for dotplot
standard_theme_dotplot <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(color = "black", size = 16, face = "bold"),
  axis.text.y = element_text(color = "black", size = 16, face = "bold"),
  axis.title.x = element_text(color = "black", size = 18, face = "bold"),
  axis.title.y = element_text(color = "black", size = 18, face = "bold"),
  strip.text.x = element_text(color = "black", size = 18, face = "bold"),
  strip.background = element_rect(fill = "white"),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, size = 2),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
  plot.title = element_text(color = "black", size = 20, face = "bold")
)

# Plot Fucose Phb2
fucose_p1 <- Phb2_Liver_long %>%
  filter(Metabolite == "Fucose") %>%
  ggplot(aes(x = Weeks, y = Count, color = Genotype)) +
  stat_boxplot(geom = "boxplot") +
  geom_boxplot()


fucose_p2 <- fucose_p1 +
  labs(x = "Age (weeks)", y = "Fucose maximum scaled %") +
  scale_color_manual(values = c("blue", "red3")) +
  standard_theme_dotplot

fucose_p2

# R basic boxplot Phb2 liver fucose ---------------------------------------
# Subset based on genotype
Phb2_Liver_CF <- Phb2_Liver %>%
  filter(Genotype == "Phb2fl/fl")

Phb2_Liver_KO <- Phb2_Liver %>%
  filter(Genotype == "Phb2-/-")

# Set margins of boxplots
par(mar = c(5.1, 7, 4.1, 2.1), mgp = c(3, 1, 0), las = 0)

# Boxplot fucose on Phb2 liver
boxplot(Phb2_Liver_CF$Fucose ~ Phb2_Liver_CF$Weeks, border = c("blue"), boxwex = 0.2, at = c(1:4 - 0.15), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")

# Boxplot sorbitol on Phb2-/- plasma
boxplot(Phb2_Liver_KO$Fucose ~ Phb2_Liver_KO$Weeks, add = TRUE, boxwex = 0.2, border = c("red"), at = c(1:4 + 0.15), axes = FALSE, frame = TRUE, xlab = "", ylab = "")

# Add legends, axis and title
legend(x = "topleft", col = c("blue", "red"), legend = c("Phb2fl/fl", "Phb2-/-"), pch = 15)
axis(1, at = 1:4, labels = c(4, 5, 6, 10))
axis(2, at = (0:10) * 10)
title(main = "Fucose liver", ylab = "Fucose maximum scaled %")
