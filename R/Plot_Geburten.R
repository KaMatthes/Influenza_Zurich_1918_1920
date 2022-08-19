function_plot_birth <- function() {
  
data_pop <- read.csv("../data_raw/Data_Zurich.csv") %>%
  select(Year,PopCity) %>%
  distinct(Year, .keep_all = TRUE)

data_zh <- read_excel("../data_raw/Daten_Bulletins.xlsx", sheet=1) %>%
  select(Wochenende, Eheschliessungen, Lebendgeburten_gr,Totgeburten_gr) %>%
  mutate(Wochenende = ymd(Wochenende),
         KW = isoweek(Wochenende),
         Woche= Wochenende,
         Year = as.numeric(format(Woche,'%Y'))) %>%
  left_join(data_pop)%>%
  mutate(Eheschliessungen = as.numeric(Eheschliessungen),
         Ehe = Eheschliessungen/PopCity*100000,
         Geburten_sum = Lebendgeburten_gr+Totgeburten_gr,
         Geburt_l_rate = Lebendgeburten_gr/Geburten_sum*100,
         Stillborn = Totgeburten_gr/Geburten_sum*100,
         Geburt_inc = round(Geburten_sum/PopCity*100000,2))


Figure_marriage <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=data_zh,aes(y=Ehe,x= as.POSIXct(Woche),colour="Marriages"), lwd=lwd_size )+
  # geom_line(data=data_zh,aes(y=Geburt_inc,x=as.POSIXct(Woche),colour="Births"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[5]))+
  # scale_y_continuous(
  #   name = "Marriages per 100'000 inhabitants",
  #   sec.axis = sec_axis(~.*1, name = "Birth per 100'000 inhabitants")
  # ) +
  xlab("Month/Year")+
  ylab("per 100'000 inhab.")+
  # ggtitle("marriages") +

  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.08, 0.95),
    legend.text=element_text(size=legend_size),
    # legend.key.size = unit(1.5, 'cm'),
    # legend.spacing.x = unit(1.5, 'cm'),
    axis.text.x = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size),
    legend.background = element_rect(fill='transparent'))
         


Figure_birth <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=data_zh,aes(y=Geburt_inc,x=as.POSIXct(Woche),colour="Births"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[6]))+
  # scale_y_continuous(
  #   name = "Marriages per 100'000 inhabitants",
  #   sec.axis = sec_axis(~.*1, name = "Birth per 100'000 inhabitants")
  # ) +
  xlab("Month/Year")+
  ylab("per 100'000 inhab.")+
  # ggtitle("Birth") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.05, .95),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size),
        legend.background = element_rect(fill='transparent'))


FigureStillborn <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=data_zh ,aes(y=Stillborn,x= as.POSIXct(Woche),col="Stillborn"), lwd=lwd_size )+
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                   values =col_pal[7])+
  xlab("Month/Year")+
  ylab("Proportion of stillborns")+
  # ggtitle("Stillborns") +

  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.06, .95),
    legend.text=element_text(size=legend_size),
    # legend.key.size = unit(2, 'cm'),
    # axis.text.x = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size),
    legend.background = element_rect(fill='transparent'))


# Proportion

data_zh_ehe <- read_excel("../data_raw/Daten_Bulletins.xlsx", sheet=1) %>%
  select(Wochenende, Eheschliessungen) %>%
  mutate(Wochenende_compare = ymd(Wochenende)+273) %>%
  rename(Wochenende_ehe = Wochenende)


data_compare <- read_excel("../data_raw/Daten_Bulletins.xlsx", sheet=1) %>%
  select(Wochenende, Lebendgeburten_gr,Totgeburten_gr) %>%
  mutate(Wochenende_compare = ymd(Wochenende)+273 ,
         KW_compare = isoweek(Wochenende_compare),
         Year_compare = as.numeric(format(Wochenende_compare,'%Y'))) %>%
  select(Wochenende_compare)


data_zh_birth <- read_excel("../data_raw/Daten_Bulletins.xlsx", sheet=1) %>%
  select(Wochenende, Lebendgeburten_gr,Totgeburten_gr) %>%
  mutate(Wochenende_compare = ymd(Wochenende)) %>%
  right_join(data_compare ) %>%
  left_join(data_zh_ehe) %>%
  mutate(KW = isoweek(Wochenende_compare),
         Eheschliessungen = as.numeric(Eheschliessungen),
         Geburten_sum = Lebendgeburten_gr+Totgeburten_gr,
         Marriage_Birth_Rate = round(Geburten_sum/Eheschliessungen,2),
         Marriage_Birth_Rate = ifelse(Marriage_Birth_Rate >5,2, Marriage_Birth_Rate),
         Year = as.numeric(format(Wochenende_compare,'%Y')),
         Month = as.numeric(format(Wochenende_compare,'%m'))) %>%
  arrange(Wochenende_compare) %>%
  filter(!is.na(Geburten_sum)) 
  # group_by(Year, Month) %>%
  # mutate(sum_birth = sum(Geburten_sum),
  #        sum_ehe = sum(Eheschliessungen)) %>%
  # ungroup() %>%
  # distinct(Year, Month, .keep_all = TRUE)  %>%
  # mutate(Marriage_Birth_Rate =round(sum_birth/sum_ehe,2))



FigureBirth_marriage <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=data_zh_birth,aes(y=Marriage_Birth_Rate,x= as.POSIXct(Wochenende_compare),colour="Birth per Marriage"), lwd=lwd_size )+
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[3]))+
  xlab("Month/Year")+
  ylab("Birth per Marriage")+
  # ggtitle("Birth per Marriage") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.08, .95),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size),
        legend.background = element_rect(fill='transparent'))

plot_zurich <- cowplot::plot_grid(Figure_marriage,Figure_birth,FigureBirth_marriage, 
                                  FigureStillborn,
                                  ncol=1, nrow=4, align="hv",
                                  rel_heights = c(1,1,1,1))


return(plot_zurich)
# 
# plot_zurich <- cowplot::plot_grid(FigureInc,NULL,FigureDeath,NULL,FigureHospital, 
#                                   ncol=1, nrow=5, align="hv",
#                                   rel_heights = c(1,-0.1,1,-0.1,1))


}

# cowplot::save_plot("output/plot_zurich.pdf", plot_zurich ,base_height=15,base_width=20)
