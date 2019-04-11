library(tidyverse)
library(readxl)

# Load data set
BW <- read_excel("data/Gcg_BW.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(BW)

# Lineplot with points: body weight over time -----------------------------

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

# Boxplot + dotplot: Body weight, 1-week HFD ------------------------------

# Change from wide to long format
BW_long <- BW %>%
  gather(Week, Weight, 5:9) %>%
  arrange(ID)

View(BW_long)

# Rank the sex
BW_long$Sex <- factor(BW_long$Sex, levels = c("Male", "Female"))

# Plot week 1 data based on sex and diet (standard plot)
week1_p1 <- BW_long %>%
  filter(Week == "Week 1") %>%
  ggplot(aes(x = Sex, y = Weight)) +
  geom_boxplot(aes(fill = Diet), position = position_dodge(width = 1), outlier.size = 0) +
  # Change fill color of the boxes
  scale_fill_manual(values = c("green3", "red")) +
  # Overlay dotplot
  geom_dotplot(aes(color = Diet), binwidth = .5, stackdir = "center",
    binaxis = "y", position = position_dodge(width = 1)
  ) +
  # Change the dot color to black
  scale_colour_manual(values = c("black", "black"))

week1_p2 <- week1_p1 +
  ylim(20, 40) +
  labs(title = "1-week HFD", x = NULL, y = "Weight (g)") +
  # Remove the label of the dots
  guides(color = FALSE) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 14, face = "bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
  
week1_p2

ggsave(here::here("graph/BW_week1_boxplot.png"), week1_p2) # Save 7.36 x 4.55 in image
