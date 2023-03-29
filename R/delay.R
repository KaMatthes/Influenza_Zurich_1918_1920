funtion_heatmap_delay <- function(type_data) {

if(type_data=="Faelle") {
data_delay_faelle <-  read_excel("../data_raw/Delay_Meldungen.xlsx", sheet="Faelle") %>%
  mutate(Meldung = ymd(Meldung),
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
  # group_by(Meldung) %>%
  # mutate(Meldung_sum = sum(Number),
  #        Meldung_prop = Number/Meldung_sum*100) %>%
  # ungroup() %>%
  # mutate(Meldung_prop = ifelse(is.na(Meldung_prop), 0, Meldung_prop),
  #        Notification = cut(Meldung_prop, breaks=c(0,0.001,5,10,50,80),  include.lowest = TRUE))

group_by(Erkrankung) %>%
  mutate(Erkrankung_sum = sum(Number),
         Erkrankung_prop = Number/Erkrankung_sum*100) %>%
  ungroup() %>%
  mutate(Erkrankung_prop = ifelse(is.na(Erkrankung_prop), 0, Erkrankung_prop),
         # Notification = cut(Erkrankung_prop, breaks=quantile(Erkrankung_prop),  include.lowest = TRUE))
         # Notification = cut(Erkrankung_prop, breaks=c(0,0.000001,20,40,60,80,100),  include.lowest = TRUE),
         Notification = cut(Erkrankung_prop, breaks=c(0,0.000001,20,40,60,80,100),  include.lowest = TRUE))


plot_heat <- ggplot() +
  geom_tile(data=data_delay_faelle , aes(factor(iso_erk),factor(iso_meld),fill=Notification)) +
  # scale_fill_manual(labels=c("0",">0 - 20",">20-40",">40-60",">60-80",">80-100"),
  #   values=c("grey60",brewer.pal(5, "YlOrRd"))) +
  scale_fill_manual("Reports in %",
                    labels=c("0",">0 - 20",">20-40",">40-60",">60-80",">80-100"),
                    values=c("grey70",brewer.pal(5, "YlGnBu"))) +
  
  # scale_fill_manual("Reports in %",
  #                   labels=c("0",">0 - 20",">20-40",">40-60",">60-80",">80-100"),
  #                   values=c(brewer.pal(6, "YlGnBu"))) +
  xlab("Disease Week") +
  ylab("Notification Week") +
  ggtitle("Cases") +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.text = element_text(size=text_size),
        axis.title  = element_text(size=axis_legend_size), 
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        title =element_text(size=title_size),
        legend.position = "bottom")

}

else if(type_data=="Totesfaelle") {
data_delay_deaths  <-  read_excel("../data_raw/Delay_Meldungen.xlsx", sheet="Totesfaelle") %>%
  mutate(Meldung = ymd(Meldung),
         Erkrankung = ymd(Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         weeks_delay = days_delay/7,
         Year = year(Meldung),
         iso_meld = isoweek(Meldung),
         iso_meld = ifelse( iso_meld == 1, 53,  iso_meld),
         iso_erk = isoweek(Erkrankung),
         iso_erk = ifelse( iso_erk == 1, 53,  iso_erk),
         Year_KW_e = paste0(Year,"/",iso_erk),
         Year_KW_m = paste0(Year,"/",iso_meld)) %>%
  filter(Year==1918) %>%
  # group_by(Meldung) %>%
  # mutate(Meldung_sum = sum(Number),
  #        Meldung_prop = Number/Meldung_sum*100) %>%
  # ungroup() %>%
  # mutate(Meldung_prop = ifelse(is.na(Meldung_prop), 0, Meldung_prop),
  #        Notification = cut(Meldung_prop, breaks=c(0,0.001,5,10,50,80),  include.lowest = TRUE))
  
  group_by(Erkrankung) %>%
  mutate(Erkrankung_sum = sum(Number),
         Erkrankung_prop = Number/Erkrankung_sum*100) %>%
  ungroup() %>%
  mutate(Erkrankung_prop = ifelse(is.na(Erkrankung_prop), 0, Erkrankung_prop),
         # Notification = cut(Erkrankung_prop, breaks=quantile(Erkrankung_prop),  include.lowest = TRUE))
         # Notification = cut(Erkrankung_prop, breaks=c(0,0.000001,20,40,60,80,100),  include.lowest = TRUE),
         Notification = cut(Erkrankung_prop, breaks=c(0,0.000001,20,40,60,80,100),  include.lowest = TRUE))



# mean_delay <- weighted.mean(data_delay$days_delay, data_delay$Number)
# mean_delay_weeks <- weighted.mean(data_delay$weeks_delay, data_delay$Number)



plot_heat <- ggplot() +
  geom_tile(data=data_delay_deaths , aes(factor(iso_erk),factor(iso_meld),fill=Notification)) +
  # scale_fill_manual(labels=c("0",">0 - 20",">20-40",">40-60",">60-80",">80-100"),
  #   values=c("grey60",brewer.pal(5, "YlOrRd"))) +
  scale_fill_manual("Reports in %",
                    labels=c("0",">0 - 20",">20-40",">40-60",">60-80",">80-100"),
                    values=c("grey70",brewer.pal(5, "YlGnBu"))) +

  # scale_fill_manual("Reports in %",
  #                   labels=c("0",">0 - 20",">20-40",">40-60",">60-80",">80-100"),
  #                   values=c(brewer.pal(6, "YlGnBu"))) +
  xlab("Deaths Week") +
  ylab("Notification Week") +
  ggtitle("Deaths") +
  theme_bw() +
  theme(aspect.ratio=1,
        axis.text = element_text(size=text_size),
        axis.title  = element_text(size=axis_legend_size), 
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        title =element_text(size=title_size),
        legend.position = "bottom")
}

return(plot_heat)

}