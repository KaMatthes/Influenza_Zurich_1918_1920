function_reproductive_table <- function(interval_length) {



function_reproductive <- function(wave, data_typ, Year_wave, beginn,end,mean.time,sd.time) {

  
  load("../data/dataZH.RData")
  load("../data/data_meldungen.RData")
  # 
  # data.wave <- dataZH %
  #   filter(Year==1918) %>%
  #   filter(iso_cw >26 & iso_cw < 31) %>%
  #   mutate(Days = c(0,7,14,21))
  # 
if(wave==1) {
    data.wave <- dataZH %>%
      filter(Year==Year_wave) %>%
      filter(iso_cw > beginn & iso_cw < end) %>%
      mutate(Days = c(0,7,14,21, 28))
    
}
  else if (wave==2) {
    
    data.wave <- dataZH %>%
      filter(Year==Year_wave) %>%
      filter(iso_cw > beginn & iso_cw < end) %>%
      mutate(Days = c(0,7,14,21,28,35,42,49,56))
    # mutate(Days = c(0,7,14,21,28,35,42,49,56,63,70,77,84))
  }
  
  else if (wave==3) {
    
    data.wave <- dataZH %>%
      filter(Year==Year_wave) %>%
      filter(iso_cw > beginn & iso_cw < end) %>%
      mutate(Days = c(0,7,14,21,28,35))
    # mutate(Days = c(0,7,14,21,28,35,42,49,56,63,70,77,84))
  }
  
  else if (wave==4) {
    
    data.wave <- data_meldungen %>%
      filter(Erhebung=="Disease")  %>%
      filter(Year==Year_wave) %>%
      filter(iso_cw > beginn & iso_cw < end) %>%
      mutate(Days = c(0,7,14,21,28,35))
    # mutate(Days = c(0,7,14,21,28,35,42,49,56,63,70,77,84))
  }
  
  else if (wave==5) {
    
    data.wave <- data_meldungen %>%
      filter(Erhebung=="Notifications")  %>%
      filter(Year==Year_wave) %>%
      filter(iso_cw > beginn & iso_cw < end) %>%
      mutate(Days = c(0,7,14,21,28,35))
    # mutate(Days = c(0,7,14,21,28,35,42,49,56,63,70,77,84))
  }
    
    GT.flu <- generation.time("gamma", c(mean.time, sd.time))
    Res.Wave <- est.R0.EG(epid=as.matrix(data.wave[, data_typ])[,1],GT=GT.flu,t=data.wave$Days,time.step = 7 )
    Res.Wave <- data.frame(Type=  data_typ, 
                           Wave=wave,
                           R=format(round(Res.Wave$R,2),nsmall = 2),
                           CIl=format(round(Res.Wave$conf.int[1],2),nsmall = 2),
                           CIu=format(round(Res.Wave$conf.int[2],2),nsmall = 2))
    return(Res.Wave)
  }
  

if(interval_length == "short") {
  
RO.inc.canton1 <- function_reproductive(1,"CantonCases", 1918, 25, 31, 2.6,1)
RO.inc.city1 <- function_reproductive(1,  "CityCases" , 1918, 25, 31,2.6,1)
RO.death.city1 <- function_reproductive(1,  "CityDeathsTotal" , 1918, 25, 31, 2.6,1)
RO.hos.city1 <- function_reproductive(1,  "Andere_Infekt" , 1918, 25, 31, 2.6,1)


RO.inc.canton2 <- function_reproductive(2,"CantonCases", 1918, 36, 46, 2.6,1)
RO.inc.city2 <- function_reproductive(2,  "CityCases" , 1918, 36, 46, 2.6,1)
RO.death.city2 <- function_reproductive(2,  "CityDeathsTotal" , 1918, 36, 46, 2.6,1)
RO.hos.city2 <- function_reproductive(2,  "Andere_Infekt" , 1918, 36, 46, 2.6,1)


RO.inc.canton3 <- function_reproductive(3,"CantonCases", 1920, 2, 9, 2.6,1)
RO.inc.city3 <- function_reproductive(3,  "CityCases" , 1920, 2,9, 2.6,1)
RO.death.city3 <- function_reproductive(3,  "CityDeathsTotal" , 1920, 2, 9, 2.6,1)
RO.hos.city3 <- function_reproductive(3,  "Andere_Infekt" , 1920, 2,9, 2.6,1)


RO.inc.Meldungen4 <- function_reproductive(4,"Faelle", 1918, 36, 43, 2.6,1)
RO.inc.Meldungen5 <- function_reproductive(5,"Faelle", 1918, 36, 43, 2.6,1)

RO.death.Meldungen4 <- function_reproductive(4,"Totesfaelle", 1918, 36, 43, 2.6,1)
RO.death.Meldungen5 <- function_reproductive(5,"Totesfaelle", 1918, 36, 43, 2.6,1)


Results_R0_short <- rbind(RO.inc.canton1,RO.inc.city1,RO.death.city1,RO.hos.city1,
                          RO.inc.canton2,RO.inc.city2,RO.death.city2,RO.hos.city2,
                          RO.inc.canton3,RO.inc.city3,RO.death.city3,RO.hos.city3,
                          RO.inc.Meldungen4, RO.inc.Meldungen5,
                          RO.death.Meldungen4, RO.death.Meldungen5) %>%
  mutate(
    Type = recode(Type,
                  "CantonCases" = "Canton Cases",
                  "CityCases"    = "City Zurich Cases",
                  "CityDeathsTotal" = "City Zurich Deaths",
                  "Andere_Infekt" = "Hospitalisation Infection inkl.Influenza"),
    Type = ifelse(Type=="Faelle" & Wave==4, "Reports Disease", Type),
    Type = ifelse(Type=="Faelle" & Wave==5, "Reports Notifications Disease", Type),
    Type = ifelse(Type=="Totesfaelle" & Wave==4, "Reports Death", Type),
    Type = ifelse(Type=="Totesfaelle" & Wave==5, "Reports Notifications Death", Type),
    Wave2 = Wave,
    Wave = recode(Wave, 
                  "1" = "1918 Summer",
                  "2" = "1918 Fall",
                  "3" = "1920",
                  "4" = "1918 Fall",
                  "5" = "1918 Fall"),
    Wave2 = recode(Wave2, 
                  "1" = "1",
                  "2" = "2",
                  "3" = "3",
                  "4" = "2",
                  "5" = "2"),
    Wave2 = as.numeric(Wave2),
    R_number = paste0(R," [",CIl,"-", CIu,"]")) %>%
  arrange(Wave2) %>%
  select(Date= Type, Wave, R_number) 

# 
# write.xlsx(Results_R0_short,"output/Results_R0_short.xlsx", rowNames=FALSE, overwrite = TRUE)

return(Results_R0_short)

}


else if(interval_length == "long") {
  

RO.inc.canton1 <- function_reproductive(1,"CantonCases", 1918, 25, 31, 4,1)
RO.inc.city1 <- function_reproductive(1,  "CityCases" , 1918, 25, 31, 4,1)
RO.death.city1 <- function_reproductive(1,  "CityDeathsTotal" , 1918, 25, 31, 4,1)
RO.hos.city1 <- function_reproductive(1,  "Andere_Infekt" , 1918, 25, 31, 4,1)


RO.inc.canton2 <- function_reproductive(2,"CantonCases", 1918, 36, 46, 4,1)
RO.inc.city2 <- function_reproductive(2,  "CityCases" , 1918, 36, 46, 4,1)
RO.death.city2 <- function_reproductive(2,  "CityDeathsTotal" , 1918, 36, 46, 4,1)
RO.hos.city2 <- function_reproductive(2,  "Andere_Infekt" , 1918, 36, 46, 4,1)


RO.inc.canton3 <- function_reproductive(3,"CantonCases", 1920, 2, 9, 4,1)
RO.inc.city3 <- function_reproductive(3,  "CityCases" , 1920, 2,9, 4,1)
RO.death.city3 <- function_reproductive(3,  "CityDeathsTotal" , 1920, 2, 9, 4,1)
RO.hos.city3 <- function_reproductive(3,  "Andere_Infekt" , 1920, 2,9, 4,1)


RO.inc.city4 <- function_reproductive(4,"Faelle", 1918, 36, 43, 4,1)
RO.inc.city5 <- function_reproductive(5,"Faelle", 1918, 36, 43, 4,1)


RO.inc.Meldungen4 <- function_reproductive(4,"Faelle", 1918, 36, 43, 4,1)
RO.inc.Meldungen5 <- function_reproductive(5,"Faelle", 1918, 36, 43, 4,1)

RO.death.Meldungen4 <- function_reproductive(4,"Totesfaelle", 1918, 36, 43, 4,1)
RO.death.Meldungen5 <- function_reproductive(5,"Totesfaelle", 1918, 36, 43, 4,1)

Results_R0_long <- rbind(RO.inc.canton1,RO.inc.city1,RO.death.city1,RO.hos.city1,
                          RO.inc.canton2,RO.inc.city2,RO.death.city2,RO.hos.city2,
                          RO.inc.canton3,RO.inc.city3,RO.death.city3,RO.hos.city3,
                          RO.inc.Meldungen4, RO.inc.Meldungen5,
                          RO.death.Meldungen4, RO.death.Meldungen5) %>%
  mutate(
    Type = recode(Type,
                  "CantonCases" = "Canton Cases",
                  "CityCases"    = "City Zurich Cases",
                  "CityDeathsTotal" = "City Zurich Deaths",
                  "Andere_Infekt" = "Hospitalisation Infection inkl.Influenza"),
    Type = ifelse(Type=="Faelle" & Wave==4, "Reports Disease", Type),
    Type = ifelse(Type=="Faelle" & Wave==5, "Reports Notifications Disease", Type),
    Type = ifelse(Type=="Totesfaelle" & Wave==4, "Reports Death", Type),
    Type = ifelse(Type=="Totesfaelle" & Wave==5, "Reports Notifications Death", Type),
    Wave2 = Wave,
    Wave = recode(Wave, 
                  "1" = "1918 Summer",
                  "2" = "1918 Fall",
                  "3" = "1920",
                  "4" = "1918 Fall",
                  "5" = "1918 Fall"),
    Wave2 = recode(Wave2, 
                   "1" = "1",
                   "2" = "2",
                   "3" = "3",
                   "4" = "2",
                   "5" = "2"),
    Wave2 = as.numeric(Wave2),
    R_number = paste0(R," [",CIl,"-", CIu,"]")) %>%
  arrange(Wave2) %>%
  select(Date= Type, Wave, R_number) 




return(Results_R0_long)
# write.xlsx(Results_R0_long,"output/Results_R0_long.xlsx", rowNames=FALSE, overwrite = TRUE)

}

}