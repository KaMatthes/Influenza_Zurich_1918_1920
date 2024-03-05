datacases <- read_excel("data_raw/Data_Zuerich.xlsx", sheet="WeeksMaster2") 
datapop <-  read_excel("data_raw/Data_Zuerich.xlsx", sheet="Population") 


dataZH <- datacases %>%
  mutate(Reporting= ymd(EndReportingPeriod),
         iso_cw = isoweek(Reporting),
         Year = ifelse(iso_cw==1,  year(EndReportingPeriod),year(StartReportingPeriod)),
         Month = ifelse(iso_cw==1,  month(EndReportingPeriod),month(StartReportingPeriod)),
         Cw_year= paste0(Year,"/W",iso_cw )) %>%
  left_join(datapop) %>%
  mutate(DeathsInc = CityDeathsTotal/CityZurich*1000,
         InfluenzaInc= CityCases/CityZurich*1000,
         # InfluenzaInc= ifelse(is.na(InfluenzaInc), 0,InfluenzaInc),
         HospInc = Total_Aufnahmen/CityZurich*1000,
         AndereInc = Andere_Infekt/CityZurich*1000,
         InfluenzaCantonInc = CantonCases/CantonZH*1000,
         # InfluenzaCantonInc= ifelse(is.na(InfluenzaCantonInc), 0,InfluenzaCantonInc),
         HospInf = Total_Aufnahmen - Andere_Infekt,
         HospInfInc =  HospInf/CityZurich*1000 ) 

pop.year <- dataZH %>%
  select(Year, CityZurich, CantonZH,iso_cw) %>%
  filter( iso_cw==1) 

pop_weekly <- dataZH %>%
  select(Year,iso_cw) %>%
  full_join(pop.year) %>%
  arrange(Year, iso_cw) %>%
  mutate(pop.weekly = round(zoo::na.approx(CityZurich, na.rm=FALSE),0),
         pop.weekly.canton = round(zoo::na.approx(CantonZH, na.rm=FALSE),0),
         Cw_year= paste0(Year,"/W",iso_cw )) %>%
  select(Cw_year, pop.weekly, pop.weekly.canton)

dataZH <- dataZH %>%
  left_join(pop_weekly)


# Temperatur

# dat.temp <- read.table("data_raw/order_108443_data.txt",header=TRUE,sep=";")
# 
# dat.temp <- dat.temp %>%
#   mutate(datum = ymd(time),
#          Year = year(datum),
#          iso_cw =  isoweek(datum),
#          Cw_year= paste0(Year,"/W",iso_cw ),
#          days_maxium = ifelse(tre200dx>=30, 1, 0),
#          days_minimum = ifelse(tre200dn<=-10, 1, 0)) %>%
#   group_by(Cw_year) %>%
#   summarise(mean_maxium = mean(tre200dx ),
#             mean_minimum = mean(tre200dn),
#             mean_mean = mean(tre200d0),
#             days_max = sum(days_maxium),
#             days_min = sum(days_minimum))
# 
# dataZH  <- dataZH %>%
#   full_join(dat.temp)

write.xlsx(dataZH,"data/dataZH.xlsx", rowNames=FALSE, overwrite = TRUE)
save(dataZH,file="data/dataZH.RData")

