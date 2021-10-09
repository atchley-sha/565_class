if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  pacman,
  tidyverse,
  flextable,
  knitr,
  mlogit,
  broom,
  modelsummary,
  magrittr
)

worktrips <- read_rds("data/worktrips_dfidx.rds")
worktrips

alternatives <- worktrips$idx$alternative %>%
  unique()
population <- worktrips$idx$id %>%
  unique()

num_alt <- length(alternatives)
num_pop <- length(population)

##### Question 1 #####
lmeq <- tibble(
  Alternative = c("Drive Alone", "Share 2", "Share 3+", "Transit", "Bike", "Walk"),
  Intercept = rep.int(1/num_alt, times = num_alt),
  Statistic = rep.int("(NA)", times = num_alt))
lmeqLL <- num_pop * log(1/num_alt)


lmint <- mlogit(CHOSEN ~ 1, worktrips)
# lminttidy <- tidy(lmint)
# lmintidy$statistic %<>% round(digits = 2) %>%
#   paste0("(", . , ")")

lmtt <- mlogit(CHOSEN ~ TVTT, worktrips)
lmttr2m <- (1 - (lmtt$logLik / lmint$logLik))[1]
lmttr2n <- (1 - (lmtt$logLik / lmeqLL))[1]
# lmtttidy <- tidy(lmtt)
# lmtttidy$statistic %<>% round(digits = 2) %>%
#   paste0("(", . , ")")
# lmtttidy %<>% unite("Estimate", c("estimate", "statistic"), sep = " ")

modelsummary(
  list(Market = lmint, Travel = lmtt),
  output = "flextable",
  estimate = "{estimate} ({std.error}){stars}",
  statistic = NULL,
  gof_omit = ".IC"
  ) %>%
  autofit() %>%
  align(j = 2:3, align = "center", part = "all")

#### Question 2 ####
lmvot <- mlogit(CHOSEN ~ TVTT + COST, worktrips)
vot <- unname(lmvot$coefficients["TVTT"]) / unname(lmvot$coefficients["COST"]) * (60/100)


#### Question 3 ####
lmwait <- mlogit(CHOSEN ~ IVTT + OVTT, worktrips)
wait <- unname(lmwait$coefficients["OVTT"]) / unname(lmwait$coefficients["IVTT"])


#### Question 4 ####
lmdens <- mlogit(CHOSEN ~ COSTINC | RSPOPDEN + WKEMPDEN, worktrips)
