library(tidyverse)
library(readxl)

# Load data set
BW <- read_excel("data/Gcg_BW.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(BW)

# Change from wide to long format
BW_long <- BW %>%
  gather(Week, Weight, 5:9) %>%
  na.omit() %>% # Remove rows with missing values
  arrange(ID)

View(BW_long)

str(BW_long)

# Change Week to numeric
BW_long$Week <- gsub("Week ", "", BW_long$Week)

BW_long$Week <- as.numeric(BW_long$Week)

View(BW_long)

# Calculate mean and sd of BW (pool GcgcreTRAP and TRAP together)
BW_stat <- BW_long %>%
  group_by(Diet, Sex, Week) %>%
  summarise(
    avg_BW = mean(Weight, na.rm = TRUE),
    sd_BW = sd(Weight, na.rm = TRUE),
    count = n()
  ) 

View(BW_stat)

write.csv(BW_stat, "data/Mouse_count_200619.csv") # Save BW statistics

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

# BW line graph for male
BW_p1 <- BW_stat %>%
  filter(Sex == "Male") %>%
  ggplot(aes(x = Week, y = avg_BW, color = Diet))

BW_p2 <- BW_p1 +
  geom_point(size = 4) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = avg_BW - sd_BW, ymax = avg_BW + sd_BW), size = 1.5, width = .1) +
  labs(title = "Body weight - Male", y = "Body weight (g)") +
  scale_x_continuous(breaks = seq(0, 4, 1)) + ylim(20, 40) +
  scale_color_manual(values = c("green4", "red3")) +
  standard_theme_line

BW_p2

ggsave(here::here("graph/BW_Male_200619.png"), BW_p2) # Saving 7.64 x 5.76 in image

# BW line graph for female
BW_p1 <- BW_stat %>%
  filter(Sex == "Female") %>%
  ggplot(aes(x = Week, y = avg_BW, color = Diet))

BW_p2 <- BW_p1 +
  geom_point(size = 4) +
  geom_line(size = 1.5) +
  geom_errorbar(aes(ymin = avg_BW - sd_BW, ymax = avg_BW + sd_BW), size = 1.5, width = .1) +
  labs(title = "Body weight - Female", y = "Body weight (g)") +
  scale_x_continuous(breaks = seq(0, 4, 1)) + ylim(20, 40) +
  scale_color_manual(values = c("green4", "red3")) +
  standard_theme_line

BW_p2

ggsave(here::here("graph/BW_Female_200619.png"), BW_p2) # Saving 7.64 x 5.76 in image
