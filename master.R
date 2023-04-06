.libPaths(c("H:/Documents/R/win-library/4.2", "C:/Program Files/R/R-4.2.2/library"))

library(vcdExtra)
library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(rgdal)
library(zoo)
library(foreach)
library(arsenal)
library(rmarkdown)
library(kableExtra)
library(sf)
library(sp)
library(spdep)
library(rgdal)
library(rgeos)
# library(tmap)
# library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
library(ggpattern)
library(INLA)
library(maptools)
library(colorspace)
library(viridis)
library(RColorBrewer)
library(scales)
library(ggsci)
library(cowplot)
library(tsoutliers)
library(strucchange)
library(conflicted)
library(wktmo)
library(R0)
library(ggtext)



conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("summarise", "dplyr")


# Plot parameter

lwd_size <- 1.2
text_size <- 20
legend_size <- 12
axis_legend_size <- 20
title_size <- 20


size_axis <-12
size_axis_title <- 12


col_pal <- pal_jco()(8)
pd <-position_dodge(width=2)


lims1 <- as.POSIXct(ymd("1909-07-31"))    
lims2 <- as.POSIXct(ymd("1925-12-31"))    

lims3 <- as.POSIXct(ymd("1909-01-01"))    
lims4 <- as.POSIXct(ymd("1970-12-31"))  


lims5 <- ymd("1918-05-01")
lims6 <- ymd("1920-05-30")    

lims_swiss_re1 <- as.POSIXct(ymd("1909-07-31"))    
lims_swiss_re2 <- as.POSIXct(ymd("1930-12-31"))    


datlim1 <- as.POSIXct(ymd("1918-06-29"))
datlim2 <- as.POSIXct(ymd("1919-06-07"))

datlim3 <- as.POSIXct(ymd("1918-09-14"))
datlim4 <- as.POSIXct(ymd("1918-12-21"))

datlim5 <- as.POSIXct(ymd("1919-02-08"))
datlim6 <- as.POSIXct(ymd("1919-05-10"))

datlim7 <- as.POSIXct(ymd("1920-01-10"))
datlim8 <- as.POSIXct(ymd("1920-05-22"))

datlim9 <- as.POSIXct(ymd("1921-12-31"))
datlim10 <- as.POSIXct(ymd("1922-04-08"))

datlim11 <- as.POSIXct(ymd("1924-01-19"))
datlim12 <- as.POSIXct(ymd("1924-05-10"))

datlim13 <- as.POSIXct(ymd("1925-01-31"))
datlim14 <- as.POSIXct(ymd("1925-05-16"))

datlim15 <- as.POSIXct(ymd("1914-07-28"))
datlim16 <- as.POSIXct(ymd("1918-11-11"))

datlim17 <- as.POSIXct(ymd("1926-12-11"))
datlim18 <- as.POSIXct(ymd("1927-02-19"))

datlim19 <- as.POSIXct(ymd("1928-12-22"))
datlim20 <- as.POSIXct(ymd("1929-04-20"))

datlim21 <- as.POSIXct(ymd("1932-12-31"))
datlim22 <- as.POSIXct(ymd("1933-03-18"))

datlim23 <- as.POSIXct(ymd("1957-07-06"))
datlim24 <- as.POSIXct(ymd("1958-05-17"))


datlim25 <- as.POSIXct(ymd("1932-02-13"))
datlim26 <- as.POSIXct(ymd("1932-05-07"))

datlim27 <- as.POSIXct(ymd("1939-01-07"))
datlim28 <- as.POSIXct(ymd("1939-03-25"))

datlim29 <- as.POSIXct(ymd("1911-06-10"))
datlim30 <- as.POSIXct(ymd("1911-09-09"))

datlim31 <- as.POSIXct(ymd("1947-06-07"))
datlim32 <- as.POSIXct(ymd("1947-10-04"))

datlim33 <- as.POSIXct(ymd("1952-05-24"))
datlim34 <- as.POSIXct(ymd("1952-08-23"))

datlim35 <- as.POSIXct(ymd("1938-01-22"))
datlim36 <- as.POSIXct(ymd("1938-04-23"))

datlim37 <- as.POSIXct(ymd("1943-12-25"))
datlim38 <- as.POSIXct(ymd("1944-05-13"))

datlim39 <- as.POSIXct(ymd("1950-12-30"))
datlim40 <- as.POSIXct(ymd("1951-05-12"))


datlim41 <- as.POSIXct(ymd("1953-01-10"))
datlim42 <- as.POSIXct(ymd("1953-05-16"))

datlim43 <- as.POSIXct(ymd("1957-05-25"))
datlim44 <- as.POSIXct(ymd("1957-08-24"))

datlim45 <- as.POSIXct(ymd("1960-01-02"))
datlim46 <- as.POSIXct(ymd("1960-04-23"))


datlim47 <- as.POSIXct(ymd("1959-01-03"))
datlim48 <- as.POSIXct(ymd("1959-05-30"))

datlim49 <- as.POSIXct(ymd("1955-11-19"))
datlim50 <- as.POSIXct(ymd("1956-05-12"))

datlim51 <- as.POSIXct(ymd("1967-11-18"))
datlim52 <- as.POSIXct(ymd("1968-04-13"))

datlim53 <- as.POSIXct(ymd("1926-01-01"))

datlim55 <- as.POSIXct(ymd("1962-10-28"))
datlim56 <- as.POSIXct(ymd("1963-03-17"))


# datlim37 <- as.POSIXct(ymd("1940-01-06"))
# datlim38 <- as.POSIXct(ymd("1940-04-13"))
# 

# 
# datlim41 <- as.POSIXct(ymd("1945-01-06"))
# datlim42 <- as.POSIXct(ymd("1945-06-02"))
# 
# datlim43 <- as.POSIXct(ymd("1945-12-22"))
# datlim44 <- as.POSIXct(ymd("1946-04-20"))
# 
# datlim45 <- as.POSIXct(ymd("1946-12-21"))
# datlim46 <- as.POSIXct(ymd("1947-03-29"))
# 
# datlim47 <- as.POSIXct(ymd("1948-12-11"))
# datlim48 <- as.POSIXct(ymd("1949-04-30"))
# 
# datlim49 <- as.POSIXct(ymd("1948-12-11"))
# datlim50 <- as.POSIXct(ymd("1949-04-30"))
# 
# datlim49 <- as.POSIXct(ymd("1948-12-11"))
# datlim50 <- as.POSIXct(ymd("1949-04-30"))
# 


# create data
# source("R/data_weekly.R")


measures <- tibble(
  "A" = as.Date("10.07.1918", "%d.%m.%Y"),
  "B" = as.Date("19.07.1918", "%d.%m.%Y"),
  "C" = as.Date("03.08.1918", "%d.%m.%Y"),
  "D" = as.Date("06.09.1918", "%d.%m.%Y"),
  "E" = as.Date("12.10.1918", "%d.%m.%Y"),
  "F" = as.Date("17.10.1918", "%d.%m.%Y"),
  "G" = as.Date("31.10.1918", "%d.%m.%Y"),
  "H" = as.Date("11.11.1918", "%d.%m.%Y"),
  "I" = as.Date("19.11.1918", "%d.%m.%Y"),
  "J" = as.Date("08.02.1919", "%d.%m.%Y")) %>%
  gather(.,key, date, A:J, factor_key=TRUE)

explain <- tibble(
  "A" = "Schools go into summer vacation earlier",
  "B" = "Health workers are adviced to wear a mask and a special shirt when around infected patients",
  "C" = "1st closing of Movie theaters, Theater performance, Sport performances, concerts in closed locations,
    conferences, public church services, Banquets, Dance balls, dance courses, fairs, competitions, 
    assemblies, rehearsals and races",
  "D" = "Movie theater shows are allowed again",
  "E" = "Closure of public and private schools and caf?s and public places at 9pm",
  "F" = "2nd closing of movie theaters, etc. group meetings prohibited",
  "G" = "Masks are making their first appearances",
  "H" = "Big strike, military in city after demobilization",
  "I" = "??",
  "J" = "Subventions for local communities and their increased spendings due to the preventive measures") %>%
  gather(.,key, Explanation, A:J, factor_key=TRUE)

table_legend <- explain %>%
  full_join(measures) 



text_box <- table_legend %>%
  dplyr::select(key, Explanation) %>%
  mutate(key = as.character(key),
         text=paste(key,"=",Explanation, "<br>"),
         y=c(seq(0.1,2.8, by=0.3))) %>%
  dplyr::select(text,y) 

text_box2 <- text_box$text 
label_text <- paste(text_box2[1], text_box2[2], text_box2[3], text_box2[4], text_box2[5], text_box2[6],
                    text_box2[7], text_box2[8], text_box2[9], text_box2[10])

text_plot <- data.frame(
  x = as.Date("1919-10-15"),
  y = 500,
  label = label_text
)





# load scripts

source("R/Plot_weekly_tmp.R") 
source("R/Plot_weekly_spanishflu.R") 
source("R/Plot_monthly.R") 
source("R/Plot_Swiss_Re.R") 
source("R/Plot_Swiss_Re_spanishflu.R") 
source("R/Plot_yearly.R") 
source("R/Plot_Swiss_Re_yearly.R") 
source("R/excess_mortality_weekly_check.R") 
source("R/excess_mortality_monthly_check.R") 
source("R/excess_mortality_year_check.R") 
source("R/Meldungen_Erkrankungen.R") 
source("R/delay.R") 
source("R/Reproductive_value.R") 
source("R/delay_time.R") 
source("R/delay_distribution.R") 
# source("R/Plot_Geburten.R")
# source("R/Plot_Swiss_Re.R")
# source("R/TS_Marriages.R")
# source("R/TS_Birth.R")
# source("R/TS_Marriage_Birth.R")
# source("R/TS_stillborn.R")


# render(paste0("R/City_Zuerich_influenza.Rmd"), output_file = paste0("../output/",today(),"_Report_City_Zurich_influenza.html"))



render(paste0("R/City_Zuerich_spanishflu.Rmd"), output_file = paste0("../output/",today(),"_Report_City_Zurich_spanishflu.html"))

# 
# render(paste0("R/City_Zuerich.Rmd"), output_file = paste0("../output/",today(),"_Report_City_Zurich.html"))