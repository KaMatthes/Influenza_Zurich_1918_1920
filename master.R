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
library(MASS)



conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("summarise", "dplyr")


# Plot parameter

lwd_size <- 1.8
lwd_size_vline <- 0.8
text_size <- 25
text_size_heat_map <- 15
legend_size <- 25
axis_legend_size <- 25
axis_legend_size_heat_map <- 15
title_size <- 20

size_axis_x <- 20
size_axis <-15
size_axis_title <- 15


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


# 


# create data
# source("R/data_weekly.R")


measures <- tibble(
  "A" = as.Date("15.07.1918", "%d.%m.%Y"),
  "B" = as.Date("18.07.1918", "%d.%m.%Y"),
  "C" = as.Date("25.07.1918", "%d.%m.%Y"),
  "D" = as.Date("30.07.1918", "%d.%m.%Y"),
  "E" = as.Date("23.08.1918", "%d.%m.%Y"),
  "F" = as.Date("24.08.1918", "%d.%m.%Y"),
  "G" = as.Date("28.08.1918", "%d.%m.%Y"),
  "H" = as.Date("10.10.1918", "%d.%m.%Y"),
  "I" = as.Date("11.10.1918", "%d.%m.%Y"),
  "J" = as.Date("23.10.1918",  "%d.%m.%Y"),
  "K" = as.Date("18.11.1918", "%d.%m.%Y"),
  "L" = as.Date("05.12.1918", "%d.%m.%Y"),
  "M" = as.Date("11.12.1918", "%d.%m.%Y"),
  "N" = as.Date("14.12.1918", "%d.%m.%Y"),
  "O" = as.Date("28.12.1918", "%d.%m.%Y"),
  "P" = as.Date("23.05.1919", "%d.%m.%Y"),
  "Q" = as.Date("05.02.1920", "%d.%m.%Y")) %>%
  gather(.,key, date, A:Q, factor_key=TRUE)



date_label <- tibble(
  "A" = 310,
  "B" = 260,
  "C" = 300,
  "D" = 280,
  "E" = 310,
  "F" = 260,
  "G" = 290,
  "H" = 280,
  "I" = 320,
  "J" = 260,
  "K" = 310,
  "L" = 260,
  "M" = 280,
  "N" = 310,
  "O" = 310,
  "P" = 310,
  "Q" = 310) %>%
  gather(.,key, Y_value, A:Q, factor_key=TRUE)



color_label <- tibble(
  "A" = "City of Zurich",
  "B" = "Federal state",
  "C" = "Canton Zurich",
  "D" = "City of Zurich",
  "E" = "Canton Zurich",
  "F" = "City of Zurich",
  "G" = "Canton Zurich",
  "H" = "City of Zurich",
  "I" = "Federal state",
  "J" = "Federal state",
  "K" = "Canton Zurich",
  "L" = "City of Zurich",
  "M" = "City of Zurich",
  "N" = "Canton Zurich",
  "O" = "Canton Zurich",
  "P" = "Federal state",
  "Q" = "Canton Zurich") %>%
  gather(.,key, col_value, A:Q, factor_key=TRUE)

explain <- tibble(
  "A" = "School closure (vacations)",
  "B" = "Federal decree authorizes the cantons and municipalities to prohibit events that may lead to mass",
  "C" = "Prohibition of events and implementation of the influenza reporting obligation for the canton of Zürich",
  "D" = "Set of measure",
  "E" = "First relaxations for the districts of Zurich, Affoltern, Horgen, Meilen and Dielsdorf",
  "F" = "Schools open after summer vaccation",
  "G" = "Further relaxations",
  "H" = "Schools close" ,
  "I" = "Nationwide reporting obligation for influenza and Reintroduction of the same measures of 25.07.1918",
  "J" = "Partial assumption of costs by the federal government for the construction of emergency hospitals, employment of nursing staff, compensation for doctors and the unemployed.",
  "K" = "General strike 09.11.-14.11.1918",
  "L" = "Municipal authorities have, at their own discretion, retracted most of the necessary measures",
  "M" = "Regular school starts again",
  "N" = "First relaxation of the measures and the Zurich Health Department advised against holding public Christmas parties.",
  "O" = "Further relaxation",
  "P" = "All decisions issued by the federal government are repealed again.",
  "Q" = "Recommendation of prohibition of dancing and singing in large events.") %>%
  gather(.,key, Explanation, A:Q, factor_key=TRUE)

table_legend <- explain %>%
  full_join(measures) %>%
  full_join(date_label) %>%
  full_join(color_label)

# table_legend <- explain 


text_box <- table_legend %>%
  dplyr::select(key, Explanation,col_value) %>%
  mutate(key = as.character(key),
         text=paste(key,"=",Explanation, "<br>"),
         y=c(seq(0.1,4.9, by=0.3))) %>%
  dplyr::select(text,y,col_value) 

text_box2 <- text_box$text 
label_text <- paste(text_box2[1], text_box2[2], text_box2[3], text_box2[4], text_box2[5], text_box2[6],
                    text_box2[7], text_box2[8], text_box2[9], text_box2[10],text_box2[11],text_box2[12], text_box2[13], text_box2[14],text_box2[15],
                    text_box2[16],   text_box2[17])

text_plot <- data.frame(
  x = as.Date("1919-12-15"),
  y = 280,
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
source("R/Reproductive_value_nb.R") 
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