if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  char = as.character(read.csv("../packages.csv", header = F)[,1])
)
install_github("atchley-sha/R-packageSHA")
library(packageSHA)




noBuild <- "data/se_classified_2040_NO-BUILD.dbf" %>% 
  read.dbf() %>% 
  as_tibble()

zonal <- "data/ZONAL AT2040A.DBF" %>% 
  read.dbf() %>% 
  as_tibble()

fullBase <- left_join(noBuild,
          zonal,
          by = c("Z" = "N")
          )

base <- fullBase %>% 
  select(
    -DISTRICT,
    -COUNTY,
    -ACRES,
    -SG_NAME
  ) %>% 
  replace(is.na(.), 0) %>% 
  relocate(c(Z, ATYPE))

sample_downtown <- function(base, frac){
  
  baseDT <- base %>% 
    filter(Z %in% c(1,2,3,4,5))
  
  baseRU <- base %>% 
    filter(!Z %in% c(1,2,3,4,5))
  
  additions <- colSums(
    select(baseRU,
           !c(Z, ATYPE))
    ) * frac
  
  baseDT[,3:length(colnames(baseDT))] <- 
    baseDT[,3:length(colnames(baseDT))] +
    (additions / nrow(baseDT))
  
  baseRU[,3:length(colnames(baseRU))] <- 
    baseRU[,3:length(colnames(baseRU))] * (1-frac)
  
  rbind(baseDT, baseRU) %>% 
    arrange(Z)
}

sample_downtown(base, 0.3)