# One-way ANOVA test followed by Tukey multiple pairwise-comparisons

# Body weight, T0 ---------------------------------------------------------

# Is there any significant difference between GcgcreTRAP and TRAP?
p_BW <- list()

for (i in unique(BW_long$Sex)) {
  df <- BW_long %>%
    filter(Sex == i, Week == "Week 0") %>%
    spread(Genotype, Weight)

  print(df)

  GcgcreTRAP <- as.numeric(as.character(unlist(df$GcgcreTRAP)))
  TRAP <- as.numeric(as.character(unlist(df$TRAP)))

  p <- t.test(GcgcreTRAP, TRAP)

  p_BW <- append(p_BW, i)

  p_BW <- append(p_BW, p)
}

p_BW

save(p_BW, file = "data/BW_Week0_a1.RData")

rm(df, p, p_BW)

# Is there any significant difference between male and female?
p_BW <- list()

for (i in unique(BW_long$Genotype)) {
  df <- BW_long %>%
    filter(Genotype == i, Week == "Week 0") %>%
    spread(Sex, Weight)

  print(df)

  Male <- as.numeric(as.character(unlist(df$Male)))
  Female <- as.numeric(as.character(unlist(df$Female)))

  p <- t.test(Male, Female)

  p_BW <- append(p_BW, i)

  p_BW <- append(p_BW, p)
}

p_BW

# Body weight, week 4 -----------------------------------------------------
# Any difference between Chow and HFD?
df <- BW_Breeding_long %>%
  filter(Week == 4, Sex == "Female") %>%
  na.omit() %>%
  spread(Diet, Per_Gain)

df

Chow <- as.numeric(as.character(unlist(df$Chow)))
HFD <- as.numeric(as.character(unlist(df$HFD)))

t.test(Chow, HFD)

# Body composition --------------------------------------------------------
# Any significant difference between chow and HFD?
p_fat <- list()

for (i in unique(Mass_long$Sex)) {
  df <- Mass_long %>%
    na.omit() %>%
    filter(Sex == i, Composition == "Lean", Diet_duration == "1 week") %>%
    spread(Diet, Gram)

  print(df)

  Chow <- as.numeric(as.character(unlist(df$Chow)))
  
  HFD <- as.numeric(as.character(unlist(df$HFD)))

  p <- t.test(Chow, HFD)

  p_fat <- append(p_fat, i)

  p_fat <- append(p_fat, p)
}

p_fat

# Another way of analysis
df <- Fat_mass %>%
  filter(Diet_duration == "4 week", Sex == "Female") %>%
  spread(Diet, Lean_gram)

df

Chow <- as.numeric(as.character(unlist(df$Chow)))
HFD <- as.numeric(as.character(unlist(df$HFD)))

t.test(Chow, HFD)

# Glycemia, T0 ------------------------------------------------------------

# Is there any significant difference between GcgcreTRAP and TRAP?
p_Glc <- list()

for (i in unique(Glc_long$Sex)) {
  df <- Glc_long %>%
    filter(Sex == i, Week == "Week 0") %>%
    spread(Genotype, Glycemia)

  print(df)

  GcgcreTRAP <- as.numeric(as.character(unlist(df$GcgcreTRAP)))
  TRAP <- as.numeric(as.character(unlist(df$TRAP)))

  p <- t.test(GcgcreTRAP, TRAP)

  p_Glc <- append(p_Glc, i)

  p_Glc <- append(p_Glc, p)
}

p_Glc

save(p_Glc, file = "data/p_Glc_week0.RData")

# Glycemia, week 1 --------------------------------------------------------
# Any difference between Chow and HFD?
df <- Glc_long %>%
  filter(Week == "Week 4") %>%
  na.omit() %>%
  spread(Diet, Glycemia)

df

Chow <- as.numeric(as.character(unlist(df$Chow)))
HFD <- as.numeric(as.character(unlist(df$HFD)))

t.test(Chow, HFD)

# GTT, 15 min -------------------------------------------------------------
# Any difference between Chow and HFD at 15 min?
df <- GTT_long %>%
  filter(Diet_duration == "4 week", Sex == "Female", Minute == 120) %>%
  spread(Diet, Glycemia)

df

Chow <- as.numeric(as.character(unlist(df$Chow)))
HFD <- as.numeric(as.character(unlist(df$HFD)))

t.test(Chow, HFD)

# Fasting glycemia, week 4 ------------------------------------------------
df <- GTT_long %>%
  filter(Diet_duration == "4 week", Minute == 0) %>%
  spread(Diet, Glycemia)

df

Chow <- as.numeric(as.character(unlist(df$Chow)))
HFD <- as.numeric(as.character(unlist(df$HFD)))

t.test(Chow, HFD)

# AUC, week 4 -------------------------------------------------------------
df <- AUC_ipGTT %>%
  filter(Diet_duration == "4 week", Sex == "Female") %>%
  spread(Diet, AUC)

df

Chow <- as.numeric(as.character(unlist(df$Chow)))
HFD <- as.numeric(as.character(unlist(df$HFD)))

t.test(Chow, HFD)

