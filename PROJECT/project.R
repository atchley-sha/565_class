if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  char = as.character(read.csv("packages.csv", header = F)[,1])
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

DTZones <- c(1,2,3,4,5,6,7,8,9,10)
NCZones <- c(15,16,17)

sample_downtown <- function(scen, frac, zones){
  
  scenDT <- scen %>% 
    filter(Z %in% zones)
  
  scenRU <- scen %>% 
    filter(!Z %in% zones)
  
  additions <- colSums(
    select(scenRU,
           !c(Z, ATYPE))
    ) * frac
  
  scenDT[,3:length(colnames(scenDT))] <- 
    scenDT[,3:length(colnames(scenDT))] +
    (additions / nrow(scenDT))
  
  scenRU[,3:length(colnames(scenRU))] <- 
    scenRU[,3:length(colnames(scenRU))] * (1-frac)
  
  rbind(scenDT, scenRU) %>% 
    arrange(Z)
}

sample_nc <- function(scen, frac, zonesDT, zonesNC){
  
}

popbaseDT <- base[1:10,] %>% 
  colSums() %>% 
  {.["POP"]}

popbaseRU <- base[-(1:10),] %>% 
  colSums() %>% 
  {.["POP"]}

popDT <- scenDT %>% 
  colSums() %>% 
  {.["POP"]}

popRU <- scenRU %>% 
  colSums() %>% 
  {.["POP"]}

pop0 <- popbaseDT + popbaseRU
pop1 <- popDT + popRU

pop0
pop1

pop1-pop0
