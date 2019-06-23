library(tidyverse)
library(readxl)

# Load data set
RNA_Prep <- read_excel("data/RNA_Prep.xlsx", col_types = c("text", "text", "text", "numeric", "text", "numeric", "numeric", "numeric"))

View(RNA_Prep)

# Rank the sample
RNA_Prep$Sample <- factor(RNA_Prep$Sample, levels = c("Pancreas 1/8", "Islet"))

# Set standard theme for barplot
standard_theme_barplot <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(color = "black", size = 16, face = "bold"),
  axis.text.y = element_text(color = "black", size = 16, face = "bold"),
  axis.title.x = element_text(color = "black", size = 18, face = "bold"),
  axis.title.y = element_text(color = "black", size = 18, face = "bold"),
  strip.text.x = element_text(color = "black", size = 18, face = "bold"),
  strip.background = element_rect(fill = "white"),
  legend.title = element_blank(),
  legend.text = element_text(color = "black", size = 18, face = "bold"),
  legend.key = element_rect(fill = "white"), # Remove grey background of the legend
  legend.position = c(0.9, 0.9),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, size = 2),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
  plot.title = element_text(color = "black", size = 20, face = "bold")
)

# Barplot for RNA yield
rna_plot <- ggplot(data = RNA_Prep, aes(x = Genotype, y = ng, fill = RNA_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = ng), vjust = 0, color = "black", position = position_dodge(0.9), size = 5) +
  labs(title = "RNA yield", x = NULL, y = "RNA (ng)") +
  scale_fill_manual(values = c("green4", "red3")) +
  facet_grid(cols = vars(Sample)) +
  standard_theme_barplot

rna_plot

ggsave(here::here("graph/RNA_ng_230619.png"), rna_plot) # Saving 10.3 x 6.17 in image
