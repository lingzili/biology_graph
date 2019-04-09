# Boxplot with dotplot
library(tidyverse)
library(readxl)
library(Hmisc)

# Body weight (g) ---------------------------------------------------------
# Load data set
BW <- read_excel("data/Gcg_BW.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(BW)

# Change from wide to long format
BW_long <- BW %>%
  gather(Week, Weight, 5:9) %>%
  arrange(ID)

View(BW_long)

# Plot week 0 data based on sex and genotype
# Rank the sex
BW_long$Sex <- factor(BW_long$Sex, levels = c("Male", "Female"))

week0_p1 <- BW_long %>%
  filter(Week == "Week 0") %>%
  ggplot(aes(x = Genotype, y = Weight, fill = Genotype)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Sex))

week0_p2 <- week0_p1 +
  ylim(20, 40) + # Set y axis
  labs(title = "Start of HFD (11 - 12 weeks age)", x = NULL, y = "Weight (g)") +
  guides(fill = "none") + # Remove legends
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    strip.text.x = element_text(color = "black", size = 14),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

week0_p2

ggsave(here::here("graph/BW_week0.png"), week0_p2) # Save 7.36 x 4.55 in image

# Fed glycemia (mM) -------------------------------------------------------
# Load data set
Glc <- read_excel("data/Gcg_Fed_Glycemia.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(Glc)

# Change to long form
Glc_long <- Glc %>%
  gather(Week, Glycemia, 5:9) %>%
  arrange(ID)

View(Glc_long)

# Plot week 0 data based on sex and genotype
# Rank the sex
Glc_long$Sex <- factor(Glc_long$Sex, levels = c("Male", "Female"))

week0_p1 <- Glc_long %>%
  filter(Week == "Week 0") %>%
  ggplot(aes(x = Genotype, y = Glycemia, fill = Genotype)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Sex))

week0_p2 <- week0_p1 +
  ylim(0, 15) + # Set y axis
  labs(title = "Start of HFD (11 - 12 weeks age)", x = NULL, y = "Fed blood glucose (mM)") +
  guides(fill = "none") + # Remove legends
  theme(
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", size = 14, face = "bold"),
    axis.text.y = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    strip.text.x = element_text(color = "black", size = 14),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

week0_p2

ggsave(here::here("graph/Fed_Glc_week0.png"), week0_p2) #Save 7.36 x 4.55 in image
