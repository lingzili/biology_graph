View(Gcg_BW_Breeding)

# Check column names
colnames(Gcg_BW_Breeding)

Percent_BW <- Gcg_BW_Breeding %>%
  mutate(
    Percent_week_0 = (`Week 0` / `Week 0` - 1) * 100,
    Percent_week_1 = (`Week 1` / `Week 0` - 1) * 100,
    Percent_week_2 = (`Week 2` / `Week 0` - 1) * 100,
    Percent_week_3 = (`Week 3` / `Week 0` - 1) * 100,
    Percent_week_4 = (`Week 4` / `Week 0` - 1) * 100
  )

View(Percent_BW)
