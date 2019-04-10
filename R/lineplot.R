# Line plot with mean and SD
library(tidyverse)
library(readxl)

# GTT ---------------------------------------------------------------------
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

# Rank male and female
GTT_stat$Sex <- factor(GTT_stat$Sex, levels = c("Male", "Female"))

# Line plot of week 1
week1_p1 <- GTT_stat %>%
  filter(Diet_duration == "1 week") %>%
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

# Line plot of week 4
week4_p1 <- GTT_stat %>%
  filter(Diet_duration == "4 week") %>%
  ggplot(aes(x = Minute, y = avg_Glc, color = Diet))

week4_p2 <- week4_p1 +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = avg_Glc - sd_Glc, ymax = avg_Glc + sd_Glc), size = 1, width = 4) +
  labs(title = "4 weeks of HFD: ipGTT", y = "Blood glucose (mM)") +
  scale_x_continuous(breaks = seq(0, 120, 15)) + ylim(0, 25) +
  scale_color_brewer(palette = "Set1") +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(fill = "white") # Remove grey background of the legend
  )

week4_p2

ggsave(here::here("graph/GTT_week4.png"), week4_p2)

# Body weight over time ---------------------------------------------------
# Load data set
BW <- read_excel("data/Gcg_BW.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(BW)

# Change from wide to long format
BW_long <- BW %>%
  na.omit() %>% # Remove rows with missting values
  gather(Week, Weight, 5:9) %>%
  arrange(ID)

View(BW_long)

str(BW_long)

# Change Week to numeric
BW_long$Week <- gsub("Week ", "", BW_long$Week)

BW_long$Week <- as.numeric(BW_long$Week)

View(BW_long)

# Calculate mean and sd of BW
BW_stat <- BW_long %>%
  group_by(Diet, Week) %>%
  summarise(
    avg_BW = mean(Weight, na.rm = TRUE),
    sd_BW = sd(Weight, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(Week)

View(BW_stat)

# Line plot
BW_p1 <- BW_stat %>%
  ggplot(aes(x = Week, y = avg_BW, color = Diet))

BW_p2 <- BW_p1 +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = avg_BW - sd_BW, ymax = avg_BW + sd_BW), size = 1, width = .1) +
  labs(title = "Body weight", y = "Body weight (g)") +
  scale_x_continuous(breaks = seq(0, 4, 1)) + ylim(20, 40) +
  scale_color_brewer(palette = "Set1") +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(fill = "white") # Remove grey background of the legend
  )

BW_p2

ggsave(here::here("graph/Cohort1_BW.png"), BW_p2)

# Fed glycemia over time --------------------------------------------------
# Load data set
Glc <- read_excel("data/Gcg_Fed_Glycemia.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(Glc)

# Change from wide to long format
Glc_long <- Glc %>%
  na.omit() %>% # Remove rows with missting values
  gather(Week, Glycemia, 5:9) %>%
  arrange(ID)

View(Glc_long)

str(Glc_long)

# Change Week to numeric
Glc_long$Week <- gsub("Week ", "", Glc_long$Week)

Glc_long$Week <- as.numeric(Glc_long$Week)

View(Glc_long)

# Calculate mean and sd of Glc
Glc_stat <- Glc_long %>%
  group_by(Diet, Week) %>%
  summarise(
    avg_Glc = mean(Glycemia, na.rm = TRUE),
    sd_Glc = sd(Glycemia, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(Week)

View(Glc_stat)

# Line plot
Glc_p1 <- Glc_stat %>%
  ggplot(aes(x = Week, y = avg_Glc, color = Diet))

Glc_p2 <- Glc_p1 +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = avg_Glc - sd_Glc, ymax = avg_Glc + sd_Glc), size = 1, width = .1) +
  labs(title = "Fed glycemia", y = "Fed blood glucose (mM)") +
  scale_x_continuous(breaks = seq(0, 4, 1)) + ylim(0, 15) +
  scale_color_brewer(palette = "Set1") +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_rect(fill = "white") # Remove grey background of the legend
  )

Glc_p2

ggsave(here::here("graph/Cohort1_Glycemia.png"), Glc_p2)
