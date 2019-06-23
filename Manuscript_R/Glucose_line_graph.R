library(tidyverse)
library(readxl)

# Load data set
Glc <- read_excel("data/Gcg_Fed_Glycemia.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(Glc)

# Change from wide to long format
Glc_long <- Glc %>%
  gather(Week, Glycemia, 5:9) %>%
  na.omit() %>% # Remove rows with missting values
  arrange(ID)

View(Glc_long)

str(Glc_long)

# Change Week to numeric
Glc_long$Week <- gsub("Week ", "", Glc_long$Week)

Glc_long$Week <- as.numeric(Glc_long$Week)

View(Glc_long)

# Calculate mean and sd of Glc
Glc_stat <- Glc_long %>%
  group_by(Sex, Diet, Week) %>%
  summarise(
    avg_Glc = mean(Glycemia, na.rm = TRUE),
    sd_Glc = sd(Glycemia, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(Week)

View(Glc_stat)

# Set standard theme for line graph
standard_theme_line <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(color = "black", size = 16, face = "bold"),
  axis.text.y = element_text(color = "black", size = 16, face = "bold"),
  axis.title.x = element_text(color = "black", size = 18, face = "bold"),
  axis.title.y = element_text(color = "black", size = 18, face = "bold"),
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

# Glucose line graph for male
Glc_p1 <- Glc_stat %>%
  filter(Sex == "Male") %>%
  ggplot(aes(x = Week, y = avg_Glc, color = Diet))

Glc_p2 <- Glc_p1 +
  geom_point(size = 4) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = avg_Glc - sd_Glc, ymax = avg_Glc + sd_Glc), size = 1.5, width = .1) +
  labs(title = "Blood glucose - Male", y = "Blood glucose (mM)") +
  scale_x_continuous(breaks = seq(0, 4, 1)) + ylim(0, 15) +
  scale_color_manual(values = c("green4", "red3")) +
  standard_theme_line

Glc_p2

ggsave(here::here("graph/Glucose_Male_230619.png"), Glc_p2) # Saving 7.64 x 6.12 in image

# Glucose line graph for female
Glc_p1 <- Glc_stat %>%
  filter(Sex == "Female") %>%
  ggplot(aes(x = Week, y = avg_Glc, color = Diet))

Glc_p2 <- Glc_p1 +
  geom_point(size = 4) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = avg_Glc - sd_Glc, ymax = avg_Glc + sd_Glc), size = 1.5, width = .1) +
  labs(title = "Blood glucose - Female", y = "Blood glucose (mM)") +
  scale_x_continuous(breaks = seq(0, 4, 1)) + ylim(0, 15) +
  scale_color_manual(values = c("green4", "red3")) +
  standard_theme_line

Glc_p2

ggsave(here::here("graph/Glucose_Female_230619.png"), Glc_p2)
