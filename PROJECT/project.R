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

DTZones <- c(88,89,90,91,92,
             93,102,117,118,119,
             126,127,128,129,130,
             131,132,133,134,145,
             195,196,197,198,200,
             201)

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
    (
      (additions / nrow(scenDT)) %>% 
        {.[col(
          scenDT %>%
            select(3:length(colnames(scenDT))))]
        }
    )
  
  scenRU[,3:length(colnames(scenRU))] <- 
    scenRU[,3:length(colnames(scenRU))] * (1-frac)
  
  rbind(scenDT, scenRU) %>% 
    arrange(Z)
}

sample_nc <- function(scen, frac, zonesDT, zonesNC){
  
}

DTscen <- sample_downtown(base, 0.3, DTZones) %>% 
  as.data.frame

write.dbf(DTscen, "data/se_downtown.dbf")