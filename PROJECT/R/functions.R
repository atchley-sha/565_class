#Calculate breaks
calculate_breaks <- function(sf, n = 5, col = TOTAL_VOL, style = "quantile"){
  
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
  
  breaks <- calculate_breaks(sf = sf, n = n, col = !!col, style = style)
  
  labels <- vector()
  for(i in 1:(length(breaks) - 1)){
    labels[i] <- paste(
      breaks[i],
      breaks[i+1],
      sep = "\U2013"
    )
  }
  
  list(breaks = breaks,
       labels = labels)
}



#Plot congestion mapping
plot_congestion <- function(sf, breaks, labels){
  
  sf %<>%
    mutate(VOL_CAT = cut(TOTAL_VOL + 0.001, breaks))
  
  sf$VOL_CAT %<>%
    {`levels<-`(., labels)}
  
  ggplot(sf) +
    geom_sf(aes(color = VOL_CAT), size = 1.5) +
    theme_void() +
    scale_color_manual(values = colorRampPalette(c("lightblue", "darkred"))(5)) +
    labs(color = "Total Volume")
  
}


#Calculate differences in congestion
calculate_differences <- function(base = noBuild, new, col = TOTAL_VOL, pct = F){
  
  col = enquo(arg = col)
  
  basecol <- base %>%
    as_tibble() %>% 
    select(!!col) %>% 
    unlist() %>% 
    unname()
  newcol <- new %>%
    as_tibble() %>% 
    select(!!col) %>% 
    unlist() %>% 
    unname()
  
  if(pct){
  new %>%
    mutate(DIFF = ifelse(basecol != 0, (newcol - basecol)/basecol, 0))
  } else{
    new %>%
      mutate(DIFF = newcol - basecol)
  }
  
}


#Plot congestion differences
plot_differences <- function(sf, name){
  
  ggplot(sf) +
    geom_sf(aes(color = DIFF), size = 1.5) +
    coord_sf(xlim = c(st_bbox(noBuild)$xmin, st_bbox(noBuild)$xmax),
             ylim = c(st_bbox(noBuild)$ymin, st_bbox(noBuild)$ymax)) +
    theme_void() + 
    scale_color_gradient2(low = "blue", mid = "gray80", high = "red", name = name)
}
