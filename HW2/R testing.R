HBWlm <- lm(data = psrc_attractions, HBW ~ totemp)
HBShoplm <- lm(data = psrc_attractions, HBShop ~ retl)
HBOlm <- lm(data = psrc_attractions, HBO ~ tothh + retl)
NHBlm <- lm(data = psrc_attractions, NHB ~ retl + offi + gved)


lm(data = psrc_attractions, NHB ~ tothh + totemp + retl + offi + manu + gved + othr) %>%
  summary()

ggplot(data = psrc_attractions, aes(x = NHB, y = tothh)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(data = psrc_attractions, NHB ~ retl + offi + gved) %>%
  summary()
