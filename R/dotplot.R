# Boxplot with dotplot
library(tidyverse)
library(readxl)

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

# Plot absolute weight of week 1

week1_p1 <- BW_long %>%
  filter(Week == "Week 1") %>%
  ggplot(aes(x = Diet, y = Weight, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Sex))

week1_p2 <- week1_p1 +
  ylim(20, 40) +
  labs(title = "1 week of HFD: Body weight", x = NULL, y = "Body weight (g)") +
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

week1_p2

ggsave(here::here("graph/Weight_Week1.png"), week1_p2)

# Percentage of weight gain -----------------------------------------------
# Calculate percentage of weight gain
BW$Percent_Week1 <- (BW$`Week 1`/BW$`Week 0` - 1) * 100

# Rank the sex
BW$Sex <- factor(BW$Sex, levels = c("Male", "Female"))

week1_p1 <- BW %>%
  ggplot(aes(x = Diet, y = Percent_Week1, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Sex))

week1_p2 <- week1_p1 +
  ylim(-5, 20) +
  labs(title = "1 week of HFD: Weight gain normalized to T0", x = NULL, y = "Percentage of weight gain (%)") +
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

week1_p2

ggsave(here::here("graph/Weight_Gain_Week1.png"), week1_p2) # Saving 7.36 x 4.43 in image

# Fed glycemia (mM) -------------------------------------------------------
# Load data set
Glc <- read_excel("data/Gcg_Fed_Glycemia.xlsx", col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(Glc)

# Change to long form
Glc_long <- Glc %>%
  gather(Week, Glycemia, 5:9) %>%
  arrange(ID)

View(Glc_long)

# Rank the sex
Glc_long$Sex <- factor(Glc_long$Sex, levels = c("Male", "Female"))

# Plot week 0 data based on sex and genotype
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

ggsave(here::here("graph/Fed_Glc_week0.png"), week0_p2) # Save 7.36 x 4.55 in image

# Plot week 1 data based on sex and diet
week1_p1 <- Glc_long %>%
  filter(Week == "Week 1") %>%
  ggplot(aes(x = Diet, y = Glycemia, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Sex))

week1_p2 <- week1_p1 +
  ylim(0, 15) + # Set y axis
  labs(title = "1 week of HFD: Glycemia", x = NULL, y = "Fed blood glucose (mM)") +
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

week1_p2

ggsave(here::here("graph/Fed_Glc_week1.png"), week1_p2) # Save 7.36 x 4.43 in image

# Body composition --------------------------------------------------------
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

# Rank the sex
Mass_long$Sex <- factor(Mass_long$Sex, levels = c("Male", "Female"))

# Plot fat mass of week 1
week1_p1 <- Mass_long %>%
  filter(Composition == "Fat") %>%
  ggplot(aes(x = Diet, y = Gram, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Sex))

week1_p2 <- week1_p1 +
  ylim(2, 6) +
  labs(title = "1 week of HFD: Fat mass", x = NULL, y = "Fat mass (g)") +
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

week1_p2

ggsave(here::here("graph/Fat_mass_abs_week1.png"), week1_p2) # Save 7.36 x 4.43 in image

# Calculate percentage of fat and lean mass
Mass$Lean_percent <- Mass$Lean_gram / Mass$Total_gram * 100
Mass$Fat_percent <- Mass$Fat_gram / Mass$Total_gram * 100

# Change to long form
Mass_long <- Mass %>%
  select(-c(Lean_gram, Fat_gram, Total_gram)) %>%
  gather(Composition, Percent, 6:7) %>%
  arrange(ID)

View(Mass_long)

# Remove "_percent" from data labels
Mass_long$Composition <- gsub("_percent", "", Mass_long$Composition)

# Rank the sex
Mass_long$Sex <- factor(Mass_long$Sex, levels = c("Male", "Female"))

# Plot fat mass of week 1
week1_p1 <- Mass_long %>%
  filter(Composition == "Fat") %>%
  ggplot(aes(x = Diet, y = Percent, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Sex))

week1_p2 <- week1_p1 +
  ylim(5, 25) + # Set y axis
  labs(title = "1 week of HFD: Fat mass", x = NULL, y = "Percentage of fat mass (%)") +
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

week1_p2

ggsave(here::here("graph/Fat_mass_week1.png"), week1_p2) # Save 7.36 x 4.43 in image

# Plot lean mass of week 1
week1_p1 <- Mass_long %>%
  filter(Composition == "Lean") %>%
  ggplot(aes(x = Diet, y = Percent, fill = Diet)) +
  geom_boxplot(position = position_dodge(0.8)) +
  geom_jitter(width = .05, size = 3) +
  facet_grid(cols = vars(Sex))

week1_p2 <- week1_p1 +
  ylim(75, 100) + # Set y axis
  labs(title = "1 week of HFD: Lean mass", x = NULL, y = "Percentage of lean mass (%)") +
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

week1_p2

ggsave(here::here("graph/Lean_mass_week1.png"), week1_p2) # Save 7.36 x 4.43 in image
