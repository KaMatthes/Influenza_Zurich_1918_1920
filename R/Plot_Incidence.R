function_plot_zurich <- function() {
  
dataZH <- read.csv("../data_raw/Data_Zurich.csv") 


dataZH_b     <- dataZH %>%
  mutate(Tag = substr(Wochenende, 1,2),
      Monat = substr(Wochenende, 4,5),
      Jahr = paste0(19,substr(Wochenende, 7,8)),
      Woche = ymd(paste0(Jahr,"-",Monat, "-", Tag)),
      KW = isoweek(Woche))%>%
  mutate(Namex= paste0(Year,"/W",KW),
         DeathsInc = Deaths/PopCity*1000,
         InfluenzaInc= Influenza_Stadt/PopCity*1000,
         # InfluenzaInc= ifelse(is.na(InfluenzaInc), 0,InfluenzaInc),
         HospInc = Total_Aufnahmen/PopCity*1000,
         AndereInc = Andere_Infekt/PopCity*1000,
         InfluenzaCantonInc = Influenza_Kanton/PopCanton*1000,
         # InfluenzaCantonInc= ifelse(is.na(InfluenzaCantonInc), 0,InfluenzaCantonInc),
         HospInf = Total_Aufnahmen - Andere_Infekt,
         HospInfInc =  HospInf/PopCity*1000 ) 


FigureInc <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=dataZH_b ,aes(y=InfluenzaInc,x= as.POSIXct(Woche),colour="City of Zurich"), lwd=lwd_size )+
  geom_line(data=dataZH_b,aes(y=InfluenzaCantonInc,x=as.POSIXct(Woche),colour="Canton Zurich"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[4],  col_pal[1]))+
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  ggtitle("Influenza cases") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.08, .8),
    legend.text=element_text(size=legend_size),
    # legend.key.size = unit(1.5, 'cm'),
    # legend.spacing.x = unit(1.5, 'cm'),
    axis.text.x = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size))
         


FigureDeath <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=dataZH_b ,aes(y=DeathsInc,x= as.POSIXct(Woche),col="Death"), lwd=lwd_size )+
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                   values =col_pal[7])+
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  ggtitle("Number of death") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.06, .8),
    legend.text=element_text(size=legend_size),
    # legend.key.size = unit(2, 'cm'),
    axis.text.x = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size))

FigureHospital <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # geom_line(data=dataZH_b ,aes(y=HospInfInc,x= as.POSIXct(Woche),colour="Total minus other infections"), lwd=lwd_size )+
  geom_line(data=dataZH_b,aes(y=AndereInc,x=as.POSIXct(Woche),colour="Infections incl. influenza"), lwd=lwd_size ) +
  geom_line(data=dataZH_b,aes(y= HospInc,x=as.POSIXct(Woche),colour="Total"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[2],col_pal[8]))+
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  ggtitle("Hospitalisations") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        axis.text.x = element_text(size=16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.2, .8),
    legend.text=element_text(size=legend_size),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size), 
    title =element_text(size=title_size))
    # legend.key.size = unit(2, 'cm'))

plot_zurich <- cowplot::plot_grid(FigureInc,FigureDeath,FigureHospital, 
                                  ncol=1, nrow=3, align="hv",
                                  rel_heights = c(1,1,1))


return(plot_zurich)
# 
# plot_zurich <- cowplot::plot_grid(FigureInc,NULL,FigureDeath,NULL,FigureHospital, 
#                                   ncol=1, nrow=5, align="hv",
#                                   rel_heights = c(1,-0.1,1,-0.1,1))


}

# cowplot::save_plot("output/plot_zurich.pdf", plot_zurich ,base_height=15,base_width=20)
