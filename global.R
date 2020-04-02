library(readxl)
library(dplyr)


b_plan <- read_excel("B_plan_data.xlsx", keepFormulas = TRUE)
PMI_distrib <- 0.29
lost_vect <- 0.123
usage_vect <- 0.4308
insecticide_eff_vect <- 0.0291
wear_tear_vect <- 0.0276

b_plan <- b_plan %>%
  filter(Variable == "Market size") %>%
  mutate(Amount)
