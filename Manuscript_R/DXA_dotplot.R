library(tidyverse)
library(readxl)

# Load data set
Mass <- read_excel("data/Gcg_DEXA.xlsx", col_types = c(
  "text", "text", "text", "text", "text",
  "numeric", "numeric", "numeric"
))
View(Mass)

# Plot absolute values of fat and lean mass
Mass_long <- Mass %>%
  select(-c(Total_gram)) %>%
  gather(Composition, Gram, 6:7) %>%
  arrange(ID)

View(Mass_long)

# Remove "_gram" from data labels
Mass_long$Composition <- gsub("_gram", "", Mass_long$Composition)

# Rank the diet
Mass_long$Diet <- factor(Mass_long$Diet, levels = c("Chow", "HFD"))

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

# New facet label names for diet duration
hfd.labs <- c("1 week HFD", "4 week HFD")
names(hfd.labs) <- c("1 week", "4 week")

# Plot fat mass of male
fat_male_p1 <- Mass_long %>%
  filter(Sex == "Male", Composition == "Fat") %>%
  ggplot(aes(x = Diet, y = Gram, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Diet_duration), labeller = labeller(Diet_duration = hfd.labs))

fat_male_p2 <- fat_male_p1 +
  ylim(2, 6) +
  labs(title = "Fat mass - Male", x = NULL, y = "Fat mass (g)") +
  guides(fill = "none") + # Remove legends
  scale_fill_manual(values = c("green4", "red3")) +
  standard_theme_dotplot

fat_male_p2

ggsave(here::here("graph/Fat_Male_230619.png"), fat_male_p2) # Saving 7.64 x 4.4 in image

# Plot fat mass of female
fat_female_p1 <- Mass_long %>%
  filter(Sex == "Female", Composition == "Fat") %>%
  ggplot(aes(x = Diet, y = Gram, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Diet_duration), labeller = labeller(Diet_duration = hfd.labs))

fat_female_p2 <- fat_female_p1 +
  ylim(2, 6) +
  labs(title = "Fat mass - Female", x = NULL, y = "Fat mass (g)") +
  guides(fill = "none") + # Remove legends
  scale_fill_manual(values = c("green4", "red3")) +
  standard_theme_dotplot

fat_female_p2

ggsave(here::here("graph/Fat_Female_230619.png"), fat_female_p2) # Saving 7.64 x 4.4 in image

# Plot lean mass of male
lean_male_p1 <- Mass_long %>%
  filter(Sex == "Male", Composition == "Lean") %>%
  ggplot(aes(x = Diet, y = Gram, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Diet_duration), labeller = labeller(Diet_duration = hfd.labs))

lean_male_p2 <- lean_male_p1 +
  ylim(10, 30) +
  labs(title = "Lean mass - Male", x = NULL, y = "Lean mass (g)") +
  guides(fill = "none") + # Remove legends
  scale_fill_manual(values = c("green4", "red3")) +
  standard_theme_dotplot

lean_male_p2

ggsave(here::here("graph/Lean_Male_230619.png"), lean_male_p2) # Saving 7.64 x 4.4 in image

# Plot lean mass of female
lean_female_p1 <- Mass_long %>%
  filter(Sex == "Female", Composition == "Lean") %>%
  ggplot(aes(x = Diet, y = Gram, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Diet_duration), labeller = labeller(Diet_duration = hfd.labs))

lean_female_p2 <- lean_female_p1 +
  ylim(10, 30) +
  labs(title = "Lean mass - Female", x = NULL, y = "Lean mass (g)") +
  guides(fill = "none") + # Remove legends
  scale_fill_manual(values = c("green4", "red3")) +
  standard_theme_dotplot

lean_female_p2

ggsave(here::here("graph/Lean_Female_230619.png"), lean_female_p2) # Saving 7.64 x 4.4 in image
