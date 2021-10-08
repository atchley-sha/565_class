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

alternatives <- worktrips$idx$alternative %>%
  unique()

num_alt <- length(alternatives)

##### Question 1 #####
lmeq <- tibble(
  Alternative = c("Drive Alone", "Share 2", "Share 3+", "Transit", "Bike", "Walk"),
  Intercept = rep.int(1/num_alt, times = num_alt))

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
