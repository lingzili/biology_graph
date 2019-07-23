library(tidyverse)
library(readxl)

# BW categorized by birthdate ---------------------------------------------

# Load file of breeding info
Gcg_Breeding <- read_excel("data/Gcg_Breeding.xlsx", col_types = c("text", "text", "date", "text", "text", "text", "text"))
View(Gcg_Breeding)

# Merge BW and birthdate info
Gcg_BW_Breeding <- merge(BW, Gcg_Breeding, by = c("ID", "Sex", "Genotype"))
View(Gcg_BW_Breeding)

write.csv(Gcg_BW_Breeding, "data/Gcg_BW_Breeding.csv")

# Change from wide to long format
BW_Breeding_long <- Gcg_BW_Breeding %>%
  gather(Week, Weight, 5:9) %>%
  na.omit() %>% # Remove rows with missing values
  arrange(ID)

View(BW_Breeding_long)

str(BW_Breeding_long)

# Change Week to numeric
BW_Breeding_long$Week <- gsub("Week ", "", BW_Breeding_long$Week)

BW_Breeding_long$Week <- as.numeric(BW_Breeding_long$Week)

# Change Birth to character
BW_Breeding_long$Birth <- as.character(BW_Breeding_long$Birth)

View(BW_Breeding_long)

# Rank the sex
BW_Breeding_long$Sex <- factor(BW_Breeding_long$Sex, levels = c("Male", "Female"))

# Set standard theme for point plot with two facets
standard_theme_dotplot <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(color = "black", size = 16, face = "bold"),
  axis.text.y = element_text(color = "black", size = 16, face = "bold"),
  axis.title.x = element_text(color = "black", size = 18, face = "bold"),
  axis.title.y = element_text(color = "black", size = 18, face = "bold"),
  strip.text.x = element_text(color = "black", size = 18, face = "bold"),
  strip.text.y = element_text(color = "black", size = 18, face = "bold"),
  strip.background = element_rect(fill = "white"),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, size = 2),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
  plot.title = element_text(color = "black", size = 20, face = "bold"),
  legend.title = element_text(color = "black", size = 16, face = "bold"),
  legend.text = element_text(color = "black", size = 16, face = "bold"),
  legend.key = element_rect(fill = "white", color = NA)
)

# Plot individully
BW_p1 <- BW_Breeding_long %>%
  ggplot(aes(x = Week, y = Weight, color = Birth))

BW_p2 <- BW_p1 +
  geom_point(size = 2) +
  labs(title = "Body weight - Birthdate", x = "Week", y = "Body weight (g)") +
  facet_grid(Sex ~ Diet) + ylim(20, 40) +
  standard_theme_dotplot

BW_p2

ggsave(here::here("graph/BW_Birth_Individual_010719.png"), BW_p2) # Saving 8.56 x 6.54 in image

# Percentage of weight gain categorized by birthdate ----------------------

# Load file
Gcg_BW_Breeding <- read.csv("~/biology_graph/data/Gcg_BW_Breeding.csv", row.names = 1)
View(Gcg_BW_Breeding)

# Check column names
colnames(Gcg_BW_Breeding)

# Calculate percentage of weight gain
Percent_BW <- Gcg_BW_Breeding %>%
  mutate(
    Percent_week_0 = (`Week.0` / `Week.0` - 1) * 100,
    Percent_week_1 = (`Week.1` / `Week.0` - 1) * 100,
    Percent_week_2 = (`Week.2` / `Week.0` - 1) * 100,
    Percent_week_3 = (`Week.3` / `Week.0` - 1) * 100,
    Percent_week_4 = (`Week.4` / `Week.0` - 1) * 100
  )

View(Percent_BW)

# Change from wide to long format
BW_Breeding_long <- Percent_BW %>%
  gather(Week, Per_Gain, 14:18) %>%
  arrange(ID)

View(BW_Breeding_long)

str(BW_Breeding_long)

# Change Week to numeric
BW_Breeding_long$Week <- gsub("Percent_week_", "", BW_Breeding_long$Week)

BW_Breeding_long$Week <- as.numeric(BW_Breeding_long$Week)

# Change Birth to character
BW_Breeding_long$Birth <- as.character(BW_Breeding_long$Birth)

View(BW_Breeding_long)

# Rank the sex
BW_Breeding_long$Sex <- factor(BW_Breeding_long$Sex, levels = c("Male", "Female"))

# Set standard theme for point plot with two facets
standard_theme_dotplot <- theme(
  axis.line = element_line(colour = "black"),
  axis.text.x = element_text(color = "black", size = 16, face = "bold"),
  axis.text.y = element_text(color = "black", size = 16, face = "bold"),
  axis.title.x = element_text(color = "black", size = 18, face = "bold"),
  axis.title.y = element_text(color = "black", size = 18, face = "bold"),
  strip.text.x = element_text(color = "black", size = 18, face = "bold"),
  strip.text.y = element_text(color = "black", size = 18, face = "bold"),
  strip.background = element_rect(fill = "white"),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, size = 2),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
  plot.title = element_text(color = "black", size = 20, face = "bold"),
  legend.title = element_text(color = "black", size = 16, face = "bold"),
  legend.text = element_text(color = "black", size = 16, face = "bold"),
  legend.key = element_rect(fill = "white", color = NA)
)

# Plot individully, categorized by birthdate
BW_p1 <- BW_Breeding_long %>%
  ggplot(aes(x = Week, y = Per_Gain, color = Birth))

BW_p2 <- BW_p1 +
  geom_point(size = 2, na.rm = TRUE, position = position_dodge(width = 0.3)) +
  labs(title = "% Body weight gain - Birthdate", x = "Week", y = "Increase of body weight (%)") +
  scale_y_continuous(breaks = seq(-5, 25, 5)) +
  facet_grid(Sex ~ Diet) +
  standard_theme_dotplot

BW_p2

ggsave(here::here("graph/Percent_BW_Birth_Individual_010719.png"), BW_p2) # Saving 8.8 x 6.53 in image

# Plot individually, categorized by genotype
BW_p1 <- BW_Breeding_long %>%
  ggplot(aes(x = Week, y = Per_Gain, color = Genotype))

BW_p2 <- BW_p1 +
  geom_jitter(size = 2, na.rm = TRUE, width = 0.1, height = 0.1) +
  labs(title = "% Body weight gain - Genotype", x = "Week", y = "Increase of body weight (%)") +
  scale_y_continuous(breaks = seq(-5, 25, 5)) +
  facet_grid(Sex ~ Diet) +
  scale_color_manual(values = c("red3", "green4")) +
  standard_theme_dotplot

BW_p2

ggsave(here::here("graph/Percent_BW_Genotype_010719.png"), BW_p2) # Saving 9.5 x 6.56 in image
