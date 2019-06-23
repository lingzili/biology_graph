library(tidyverse)
library(readxl)

# Open data set of GTT
GTT <- read_excel("data/Gcg_GTT.xlsx", col_types = c(
  "text", "text", "text", "text", "text",
  "numeric", "numeric",
  "numeric", "numeric",
  "numeric", "numeric",
  "numeric", "numeric",
  "numeric", "numeric",
  "numeric", "numeric"
))

View(GTT)

# Calculate average of each duplicates
for (i in c(0, 15, 30, 60, 90, 120)) {
  GTT[, paste0("min", i)] <- rowMeans(GTT[c(paste0("min", i, "_n1"), paste0("min", i, "_n2"))], na.rm = TRUE)
}

View(GTT)

# Remove duplicated raw data
GTT_min <- subset(GTT, select = -c(min0_n1:min120_n2))

View(GTT_min)

# Change to long format
GTT_long <- GTT_min %>%
  gather(Minute, Glycemia, 6:11)

View(GTT_long)

# Change Minute to numeric
GTT_long$Minute <- gsub("min", "", GTT_long$Minute)

GTT_long$Minute <- as.numeric(GTT_long$Minute)

# Calculate mean and sd
GTT_stat <- GTT_long %>%
  group_by(Diet_duration, Sex, Diet, Minute) %>%
  summarise(
    avg_Glc = mean(Glycemia, na.rm = TRUE),
    sd_Glc = sd(Glycemia, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(Minute)

View(GTT_stat)

# Rank the diet duration
GTT_stat$Diet_duration <- factor(GTT_stat$Diet_duration, levels = c("2 day", "1 week", "2 week", "4 week"))

# Set standard theme for faceted line graph
standard_theme_facet_line <- theme(
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

# New facet label names for diet duration
hfd.labs <- c("2 day HFD", "1 week HFD", "2 week HFD", "4 week HFD")
names(hfd.labs) <- c("2 day", "1 week", "2 week", "4 week")

# GTT line graph for male
GTT_p1 <- GTT_stat %>%
  filter(Sex == "Male" & Diet_duration %in% c("1 week", "4 week")) %>%
  ggplot(aes(x = Minute, y = avg_Glc, color = Diet))

GTT_p2 <- GTT_p1 +
  geom_point(size = 4) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = avg_Glc - sd_Glc, ymax = avg_Glc + sd_Glc), size = 1, width = 4) +
  facet_grid(cols = vars(Diet_duration), labeller = labeller(Diet_duration = hfd.labs)) +
  labs(title = "ipGTT - Male", y = "Blood glucose (mM)") +
  scale_x_continuous(breaks = seq(0, 120, 15)) + ylim(0, 25) +
  scale_color_manual(values = c("green4", "red3")) +
  standard_theme_facet_line
  
GTT_p2

ggsave(here::here("graph/GTT_Male_230619.png"), GTT_p2) # Saving 8.67 x 5.34 in image

# GTT line graph for female
GTT_p1 <- GTT_stat %>%
  filter(Sex == "Female" & Diet_duration %in% c("1 week", "4 week")) %>%
  ggplot(aes(x = Minute, y = avg_Glc, color = Diet))

GTT_p2 <- GTT_p1 +
  geom_point(size = 4) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = avg_Glc - sd_Glc, ymax = avg_Glc + sd_Glc), size = 1, width = 4) +
  facet_grid(cols = vars(Diet_duration), labeller = labeller(Diet_duration = hfd.labs)) +
  labs(title = "ipGTT - Female", y = "Blood glucose (mM)") +
  scale_x_continuous(breaks = seq(0, 120, 15)) + ylim(0, 25) +
  scale_color_manual(values = c("green4", "red3")) +
  standard_theme_facet_line

GTT_p2

ggsave(here::here("graph/GTT_Female_230619.png"), GTT_p2)
