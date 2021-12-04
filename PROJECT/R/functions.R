#Calculate breaks
breaks <- function(sf, n = 5, col = TOTAL_VOL, style = "quantile"){
  
  col = enquo(arg = col)
  
  sf %>% 
    as_tibble() %>%
    select(!!col) %>% 
    unlist() %>% 
    unname() %>% 
    classIntervals(n = n, style = style) %>% 
    {.$brks} %>% 
    {if_else(. > 10000, round_any(., 1000), round_any(., 500))}

}

#Return breaks and labels
get_breaks <- function(sf, n = 5, col = TOTAL_VOL, style = "quantile"){
  
  col = enquo(arg = col)
  
  breaksl <- breaks(sf = sf, n = n, col = !!col, style = style)
  
  labels <- vector()
  for(i in 1:(length(breaksl) - 1)){
    labels[i] <- paste(
      breaksl[i],
      breaksl[i+1],
      sep = "\U2013"
    )
  }
  
  list(breaks = breaksl,
       labels = labels)
}

