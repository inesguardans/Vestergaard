library(readxl)
library(dplyr)


b_plan<- read_excel("B_plan_data.xlsx")%>%
  #mutate(Amount = round(Amount, 0))%>%
  as.data.frame()


rownames(b_plan) <- b_plan$Variable
b_plan[1] <- NULL


PMI_distrib <- 0.29
lost_vect <- 0.123
usage_vect <- 0.4308
insecticide_eff_vect <- 0.0291
wear_tear_vect <- 0.0276

b_plan[3,3] <- b_plan[1,3]*b_plan[2,3]


b_plan <- b_plan %>%
  mutate()
  
typeof(b_plan[2,3])

dollar_format()(c(-100, 0.23, 1.456565, 2e3))
dollar_format()(c(1:10 * 10))
dollar(c(100, 0.23, 1.456565, 2e3))
dollar(c(1:10 * 10))

b_plan[c(3:13,15,16,18),3] <- dollar(b_plan[c(3:13,15,16,18),3])
