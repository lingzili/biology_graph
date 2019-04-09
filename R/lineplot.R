# Line plot with mean and SD
library(tidyverse)
library(readxl)

# GTT 1 week --------------------------------------------------------------
# Opem data set of GTT
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

# Calculate n number in each group
GTT_min %>%
  filter(Diet_duration == "1 week") %>%
  group_by(Sex, Diet) %>%
  summarise(total.count = n())

# Change to long format
GTT_long <- GTT_min %>%
  gather(Minute, Glycemia, 6:11)

head(GTT_long)

# Change Minute to numeric
GTT_long$Minute <- gsub("min", "", GTT_long$Minute)

GTT_long$Minute <- as.numeric(GTT_long$Minute)

# Calculate mean and sd of Week 1 data
GTT_week1 <- GTT_long %>%
  filter(Diet_duration == "1 week") %>%
  group_by(Sex, Diet, Minute) %>%
  summarise(
    avg_Glc = mean(Glycemia, na.rm = TRUE),
    sd_Glc = sd(Glycemia, na.rm = TRUE)
  ) %>%
  arrange(Minute)

View(GTT_week1)

# Line plot
GTT_week1$Sex <- factor(GTT_week1$Sex, levels = c("Male", "Female"))

week1_p1 <- GTT_week1 %>%
  ggplot(aes(x = Minute, y = avg_Glc, color = Diet))

week1_p2 <- week1_p1 +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = avg_Glc - sd_Glc, ymax = avg_Glc + sd_Glc), size = 1, width = 4) +
  facet_grid(cols = vars(Sex)) +
  labs(y = "Blood glucose (mM)") +
  scale_x_continuous(breaks = seq(0, 120, 15)) + ylim(0, 25) +
  scale_color_brewer(palette = "Set1") +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    strip.text.x = element_text(color = "black", size = 14),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(fill = "white") # Remove grey background of the legend
  )

week1_p2

ggsave(here::here("graph/GTT_week1.png"), week1_p2)
