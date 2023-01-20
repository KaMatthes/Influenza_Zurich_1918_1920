datacases <- read_excel("data_raw/Master_Zuerich.xlsx", sheet="WeeksMaster2")
datacases1962 <-  read_excel("data_raw/Master_Zuerich.xlsx", sheet="DeathsCityMonths6169") 
datapop <-  read_excel("data_raw/Master_Zuerich.xlsx", sheet="Population") 

           
         
monthly_cases <- weekToMonth(datacases$CityCases, datStart =datacases$EndReportingPeriod, wkMethod = "ISO") %>%
  rename(influenza_m = value)
monthly_death <- weekToMonth(datacases$CityDeathsTotal, datStart =datacases$EndReportingPeriod, wkMethod = "ISO")%>%
  rename(death_m = value)


dataZH <- monthly_cases %>%
  full_join(monthly_death) %>%
  mutate(influenza_m = round(influenza_m,0),
         death_m = round(death_m),
         Year = year(ym(yearMonth)),
         Month = month(ym(yearMonth))) %>%
  select(-yearMonth)

dataZH1962 <- datacases1962  %>%
  mutate(Reporting= ymd(EndReportingPeriod),
         iso_cw = isoweek(Reporting),
         Year = ifelse(iso_cw==1,  year(EndReportingPeriod),year(StartReportingPeriod)),
         Month = ifelse(iso_cw==1,  month(EndReportingPeriod),month(StartReportingPeriod))) %>%
  select(Year, Month, death_m= DeathsResidentsCity  )

dataZH1962_tmp <-  dataZH %>%
  filter(Year>1961) %>%
  select(-death_m) %>%
  full_join(dataZH1962)

dataZH_m <- dataZH %>%
  filter(Year < 1962)  %>%
  rbind(dataZH1962_tmp) %>%
  left_join(datapop)
  

pop.year <- dataZH_m %>%
  select(Year, CityZurich, Month) %>%
  filter( Month==1) 

pop_month <- dataZH_m %>%
  select(Year,Month) %>%
  full_join(pop.year) %>%
  arrange(Year, Month) %>%
  mutate(pop.monthly= round(zoo::na.approx(CityZurich, na.rm=FALSE),0)) %>%
  select(Year, Month, pop.monthly)

dataZH_m <- dataZH_m %>%
  left_join(pop_month)


dat.temp <- read.table("data_raw/order_108443_data.txt",header=TRUE,sep=";")

dat.temp <- dat.temp %>%
  mutate(datum = ymd(time),
         Year = year(datum),
         Month = month(datum),
         days_maxium = ifelse(tre200dx>=30, 1, 0),
         days_minimum = ifelse(tre200dn<=-10, 1, 0)) %>%
  group_by(Year, Month) %>%
  summarise(mean_maxium = mean(tre200dx ),
            mean_minimum = mean(tre200dn),
            mean_mean = mean(tre200d0),
            days_max = sum(days_maxium),
            days_min = sum(days_minimum)) %>%
  filter(!Year==1909) %>%
  droplevels

dataZH_month  <- dataZH_m %>%
  full_join(dat.temp) %>%
  select(Year, Month,pop.monthly,CityZurich,CantonZH,influenza_m,death_m, mean_maxium, mean_minimum, mean_mean,days_max,days_min)

write.xlsx(dataZH_month,"data/dataZH_month.xlsx", rowNames=FALSE, overwrite = TRUE)
save(dataZH_month,file="data/dataZH_month.RData")

