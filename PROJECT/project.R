if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  char = as.character(read.csv("packages.csv", header = F)[,1])
)
install_github("atchley-sha/R-packageSHA")
library(packageSHA)
################################################################################

#Import SE data
noBuild <- "data/se_classified_2040_NO-BUILD.dbf" %>% 
  read.dbf() %>% 
  as_tibble()

#Rearrange SE data
base <- noBuild %>% 
  select(
    -DISTRICT,
    -COUNTY,
    -ACRES,
    -SG_NAME,
    -N,
    -(w0v0:p4v3)
  ) %>% 
  replace(is.na(.), 0) %>% 
  relocate(c(Z, POP))

################################################################################

#Set downtown zone IDs
DTZones <- c(88,89,90,91,92,
             93,102,117,118,119,
             126,127,128,129,130,
             131,132,133,134,145,
             195,196,197,198,200,
             201)

#Set neighborhood centers zone IDs
NCZones <- c(42,66,97,148,149,
             160,161,176,177,178,
             179,180,181)

################################################################################

#Function to sample residents and jobs and move them downtown
sample_downtown <- function(scen, frac, zones){
  
  scenDT <- scen %>% 
    filter(Z %in% zones)
  
  scenRU <- scen %>% 
    filter(!Z %in% zones)
  
  additions <- colSums(select(scenRU, !Z)) * frac
  
  scenDT[,2:length(colnames(scenDT))] <- 
    scenDT[,2:length(colnames(scenDT))] +
    (
      (additions / nrow(scenDT)) %>% 
        {.[col(
          scenDT %>%
            select(2:length(colnames(scenDT))))]
        }
    )
  
  scenRU[,2:length(colnames(scenRU))] <- 
    scenRU[,2:length(colnames(scenRU))] * (1-frac)
  
  scen <- rbind(scenDT, scenRU) %>% 
    arrange(Z) %>% 
    mutate(across(POP:SG_COL,
           as.integer))
  tibble(
    Z = noBuild$Z,
    DISTRICT = noBuild$DISTRICT,
    COUNTY = noBuild$COUNTY,
    ACRES = noBuild$ACRES,
    scen %>% 
      select(-Z),
    SG_NAME = noBuild$SG_NAME,
    N = noBuild$N
  )
}


#Function to sample residents and jobs and move them to neighborhood centers
#(keeps the downtown area unchanged)
sample_nc <- function(scen, frac, zonesDT, zonesNC){
  
  scenDT <- scen %>% 
    filter(Z %in% zonesDT)
  
  scenNC <- scen %>% 
    filter(Z %in% zonesNC)
  
  scenRU <- scen %>% 
    filter(!Z %in% zonesDT, !Z %in% zonesNC)
  
  additions <- colSums(select(scenRU, !Z)) * frac
  
  scenNC[,2:length(colnames(scenNC))] <- 
    scenNC[,2:length(colnames(scenNC))] +
    (
      (additions / nrow(scenNC)) %>% 
        {.[col(
          scenNC %>%
            select(2:length(colnames(scenNC))))]
        }
    )
  
  scenRU[,2:length(colnames(scenRU))] <- 
    scenRU[,2:length(colnames(scenRU))] * (1-frac)
  
  scen <- rbind(scenDT, scenNC, scenRU) %>% 
    arrange(Z) %>% 
    mutate(across(POP:SG_COL,
                  as.integer))
  
  tibble(
    Z = noBuild$Z,
    DISTRICT = noBuild$DISTRICT,
    COUNTY = noBuild$COUNTY,
    ACRES = noBuild$ACRES,
    scen %>% 
      select(-Z),
    SG_NAME = noBuild$SG_NAME,
    N = noBuild$N
  )
}

################################################################################

#Write .dbf's of sampled downtown
sample_downtown(base, 0.3, DTZones) %>% 
  as.data.frame() %>% 
  write.dbf("data/se_downtown30.dbf")

sample_downtown(base, 1.0, DTZones) %>% 
  as.data.frame() %>% 
  write.dbf("data/se_downtown100.dbf")


#Write .dbf's of sampled neighborhood centers
sample_nc(base, 0.3, DTZones, NCZones) %>% 
  as.data.frame() %>% 
  write.dbf("data/se_neighborhood30.dbf")

sample_nc(base, 1.0, DTZones, NCZones) %>% 
  as.data.frame() %>% 
  write.dbf("data/se_neighborhood100.dbf")

################################################################################

#Check output colsums match input colsums
sums0 <- colSums(base)

col_select <- function(df, base){
  df %>% 
    select(colnames(base))
}

sums1 <- colSums(col_select(sample_downtown(base, 0.3, DTZones), base))
sums2 <- colSums(col_select(sample_downtown(base, 1.0, DTZones), base))
sumsA <- colSums(col_select(sample_nc(base, 0.3, DTZones, NCZones), base))
sumsB <- colSums(col_select(sample_nc(base, 1.0, DTZones, NCZones), base))

sums <- c(sums1, sums2, sumsA, sumsB)

#Check errors
rmserr(rep.int(sums0, 4), sums)
