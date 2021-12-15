if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  char = as.character(read.csv("packages.csv", header = F)[,1])
)
install_github("atchley-sha/R-packageSHA")
library(packageSHA)

#######################################################

#Read in SFs
noBuild <- read_sf("data/loaded_networks/NoBuild.shp")
DT30 <- read_sf("data/loaded_networks/DT30.shp")
NC30 <- read_sf("data/loaded_networks/NC30.shp")
# basesf <- read_sf("data/loaded_networks/Base.shp")


###### MESSING AROUND ###########

breaks <- get_breaks(noBuild)$breaks
labels <- get_breaks(noBuild)$labels

noBuild %<>%
  mutate(VOL_CAT = cut(TOTAL_VOL + 0.001, breaks))

noBuild$VOL_CAT %<>%
  {`levels<-`(., labels)}

ggplot(noBuild) +
  geom_sf(aes(color = VOL_CAT), size = 1.5) +
  theme_void() +
  scale_color_manual(values = colorRampPalette(c("lightblue", "darkred"))(5)) +
  labs(color = "Total Volume")



DT30 %<>%
  mutate(VOL_CAT = cut(TOTAL_VOL + 0.001, breaks))

DT30$VOL_CAT %<>%
  {`levels<-`(., labels)}

ggplot(DT30) +
  geom_sf(aes(color = VOL_CAT), size = 1.5) +
  theme_void() +
  scale_color_manual(values = colorRampPalette(c("lightblue", "darkred"))(5)) +
  labs(color = "Total Volume")
