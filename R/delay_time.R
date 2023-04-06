funtion_time_delay <- function() {

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
  filter(Year==1918) %>%
  group_by(Meldung,Year_KW_m) %>%
  summarise(weeks_delay_sum=mean(weeks_delay)) %>%
  ungroup() %>%
  mutate(Var= "Cases")

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
  filter(Year==1918) %>%
  group_by(Meldung,Year_KW_m) %>%
  summarise(weeks_delay_sum=mean(weeks_delay)) %>%  
  ungroup() %>%
  mutate(Var= "Deaths")


data_delay <- data_delay_faelle %>%
  rbind(data_delay_deaths)


plot_delays_time <- ggplot() +
  geom_line(data=data_delay , aes(x=as.Date(Meldung,format = "%Y-%m-%d"),y=weeks_delay_sum, col=Var), lwd=1.5) +
  scale_x_date( breaks = date_breaks("2 week"),
                    labels = label_date_short(),
                    # limits =c(min(lims3), max(lims4)),
                    expand = c(0,0)) +
  scale_color_manual("",values=c(col_pal[1], col_pal[4])) +
  xlab("Reporting week") +
  ylab("Mean reporting delay in weeks") +
  ylim(0,3) +
  ggtitle("Cases") +
  theme_bw() +
  theme(
        axis.text = element_text(size=text_size),
        axis.title  = element_text(size=axis_legend_size), 
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        title =element_text(size=title_size),
        legend.position = "bottom")



return(plot_delays_time)

}