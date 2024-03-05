

pop.year <- dataZH %>%
  select(Year, CityZurich, iso_cw) %>%
  filter( iso_cw==1) 
 
pop_weekly <- dataZH %>%
  select(Year,iso_cw) %>%
  full_join(pop.year) %>%
  arrange(Year, iso_cw) %>%
  mutate(CityZurich = round(zoo::na.approx(CityZurich, na.rm=FALSE),0),
         Cw_year= paste0(Year,"/W",iso_cw ))
  

write.xlsx(pop_weekly,"data/pop_weekly.xlsx", rowNames=FALSE, overwrite = TRUE)
save(pop_weekly,file="data/pop_weekly.RData")