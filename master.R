.libPaths(c("H:/Documents/R/win-library/4.1", "C:/Program Files/R/R-4.2.1/library"))

library(plyr)
library(readxl)
library(openxlsx)
library(lubridate)
library(tidyverse)
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
library(tmap)
library(tmaptools)
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


conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")

# Plot parameter

lwd_size <- 1.2
text_size <- 15
legend_size <- 16
axis_legend_size <- 15
title_size <- 15


size_axis <-12
size_axis_title <- 12




col_pal <- pal_jco()(8)

lims1 <- as.POSIXct(ymd("1909-07-31"))    
lims2 <- as.POSIXct(ymd("1925-12-31"))    

datlim1 <- as.POSIXct(ymd("1918-07-06"))
datlim2 <- as.POSIXct(ymd("1918-08-17"))

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


# load scripts
source("R/Plot_Incidence.R") 
source("R/Plot_Geburten.R")
source("R/TS_Marriages.R")
source("R/TS_Birth.R")
source("R/TS_Marriage_Birth.R")
source("R/TS_stillborn.R")

render(paste0("R/City_Zuerich.Rmd"), output_file = paste0("../output/",today(),"_Report_City_Zurich.html"))