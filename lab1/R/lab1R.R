if (!require("pacman")) install.packages("pacman")
p_load(tidyverse)


# READ IN DATA #################################################################

veh <- read_csv("data/VEH_fixed.csv")
hh <- read_csv("data/HH_fixed.csv")
emp <- read_csv("data/EMP_fixed.csv")

