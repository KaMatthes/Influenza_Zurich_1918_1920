funtion_dis_delay <- function(type_data) {

if(type_data=="Faelle") {
data_delay_faelle <-  read_excel("../data_raw/Delay_Meldungen.xlsx", sheet="Faelle") %>%
  expand.dft(., freq="Number")  %>%
  mutate(Number= 1,
         Meldung = ymd(Meldung),
         Erkrankung = ymd(Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         weeks_delay = days_delay/7,
         Year = year(Meldung),
         iso_erk = isoweek(Erkrankung),
         iso_erk = ifelse( iso_erk == 1, 53,  iso_erk),
         iso_meld = isoweek(Meldung),
         iso_meld = ifelse( iso_meld == 1, 53,  iso_meld),
         Year_KW_e = paste0(Year,"/",iso_erk),
         Year_KW_m = paste0(Year,"/",iso_meld)) %>%
  filter(Year==1918) 


plot_delays_dis <- ggplot(data=data_delay_faelle, aes(x=as.Date(Meldung,format = "%Y-%m-%d"),y=weeks_delay)) +
  # geom_point(position=pd, size=3) +
  # geom_dotplot(binaxis = "y",stackdir = "center", aes(col=Kurs, fill=Kurs), dotsize=0.5)+
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", width = 1.5,lwd=0.5,size = 1,position=pd) +
  scale_x_date( breaks = date_breaks("2 week"),
                labels = label_date_short())+
                # limits =c(min(lims3), max(lims4)),
                # expand = c(0,0)) +
  xlab("Reporting week") +
  ylab("Mean and CI reporting delay in weeks") +
  ggtitle("Cases") +
  theme_bw() +
  theme(
        axis.text = element_text(size=text_size),
        axis.title  = element_text(size=axis_legend_size), 
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        title =element_text(size=title_size),
        legend.position = "bottom")

}

else if(type_data=="Totesfaelle") {
data_delay_deaths  <-  read_excel("../data_raw/Delay_Meldungen.xlsx", sheet="Totesfaelle") %>%
  expand.dft(., freq="Number")  %>%
  mutate(Number= 1,
         Meldung = ymd(Meldung),
         Erkrankung = ymd(Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         weeks_delay = days_delay/7,
         Year = year(Meldung),
         iso_erk = isoweek(Erkrankung),
         iso_erk = ifelse( iso_erk == 1, 53,  iso_erk),
         iso_meld = isoweek(Meldung),
         iso_meld = ifelse( iso_meld == 1, 53,  iso_meld),
         Year_KW_e = paste0(Year,"/",iso_erk),
         Year_KW_m = paste0(Year,"/",iso_meld)) %>%
  filter(Year==1918) 

plot_delays_dis <- ggplot(data=data_delay_deaths, aes(x=as.Date(Meldung,format = "%Y-%m-%d"),y=weeks_delay)) +
  # geom_point(position=pd, size=3) +
  # geom_dotplot(binaxis = "y",stackdir = "center", aes(col=Kurs, fill=Kurs), dotsize=0.5)+
  stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", width = 1.5,lwd=0.5,size = 1,position=pd) +
  scale_x_date( breaks = date_breaks("2 week"),
                labels = label_date_short())+
                # limits =c(min(lims3), max(lims4)),
                # expand = c(0,0)) +
  xlab("Reporting week") +
  ylab("Mean and CI reporting delay in weeks") +
  ggtitle("Death") +
  theme_bw() +
  theme(
    axis.text = element_text(size=text_size),
    axis.title  = element_text(size=axis_legend_size), 
    legend.text=element_text(size=15),
    legend.title=element_text(size=15),
    title =element_text(size=title_size),
    legend.position = "bottom")
}

return(plot_delays_dis)

}