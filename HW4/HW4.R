if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  pacman,
  tidyverse,
  flextable,
  knitr,
  mlogit,
  broom
)

worktrips <- read_rds("data/worktrips_dfidx.rds")
worktrips

##### Question 1 #####
lmeq

lmint <- mlogit(CHOSEN ~ 1, worktrips)

lmtt <- mlogit(CHOSEN ~ TVTT, worktrips)


#### Question 2 ####
lmvot <- mlogit(CHOSEN ~ TVTT + COST, worktrips)
vot <- unname(lmvot$coefficients["TVTT"]) / unname(lmvot$coefficients["COST"]) * (60/100)


#### Question 3 ####
lmwait <- mlogit(CHOSEN ~ IVTT + OVTT, worktrips)
wait <- unname(lmwait$coefficients["OVTT"]) / unname(lmwait$coefficients["IVTT"])


#### Question 4 ####
lmdens <- mlogit(CHOSEN ~ COSTINC | RSPOPDEN + WKEMPDEN, worktrips)
