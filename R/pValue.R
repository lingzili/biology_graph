# One-way ANOVA test followed by Tukey multiple pairwise-comparisons

# Body weight -------------------------------------------------------------
# Is there any significant difference between GcgcreTRAP and TRAP?
# Week 0
p_BW <- list()

for (i in unique(BW_long$Sex)) {
  df <- BW_long %>%
    filter(Sex == i, Week == "Week 0") %>%
    spread(Genotype, Weight)
  
  print(df)

  GcgcreTRAP <- as.numeric(as.character(unlist(df$GcgcreTRAP)))
  TRAP <- as.numeric(as.character(unlist(df$TRAP)))

  p <- wilcox.test(GcgcreTRAP, TRAP, correct = FALSE)

  p_BW <- append(p_BW, i)
  
  p_BW <- append(p_BW, p)
  
}

p_BW

save(p_BW, file = "data/p_BW_week0.RData")
