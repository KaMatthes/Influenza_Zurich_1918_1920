function_plot_Reportingly <- function() {

  load("data/dataZH.RData")

FigureInc <- ggplot() +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=dataZH ,aes(y=InfluenzaInc,x= as.POSIXct(Reporting),colour="City of Zurich"), lwd=lwd_size )+
  geom_line(data=dataZH,aes(y=InfluenzaCantonInc,x=as.POSIXct(Reporting),colour="Canton Zurich"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("12 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims3), max(lims4)),
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
    legend.position = c(.1, .8),
    legend.text=element_text(size=legend_size),
    # legend.key.size = unit(1.5, 'cm'),
    # legend.spacing.x = unit(1.5, 'cm'),
    axis.text.x = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size))
         


FigureDeath <- ggplot() +
  # annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=dataZH ,aes(y=DeathsInc,x= as.POSIXct(Reporting),col="Death"), lwd=lwd_size )+
  scale_x_datetime( breaks = date_breaks("12 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims3), max(lims4)),
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
        legend.position = "none",
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

FigureHospInfl <- ggplot() +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # geom_line(data=dataZH ,aes(y=HospInfInc,x= as.POSIXct(Reporting),colour="Total minus other infections"), lwd=lwd_size )+
  geom_line(data=dataZH,aes(y=AndereInc,x=as.POSIXct(Reporting),colour="Infections incl. influenza"), lwd=lwd_size ) +
  # geom_line(data=dataZH,aes(y= HospInc,x=as.POSIXct(Reporting),colour="Total"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("12 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims3), max(lims4)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[8]))+
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  ggtitle("Hospitalisations - Infections incl. influenza") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))
    # legend.key.size = unit(2, 'cm'))

FigureHospital <- ggplot() +
  # annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # geom_line(data=dataZH ,aes(y=HospInfInc,x= as.POSIXct(Reporting),colour="Total minus other infections"), lwd=lwd_size )+
  # geom_line(data=dataZH,aes(y=AndereInc,x=as.POSIXct(Reporting),colour="Infections incl. influenza"), lwd=lwd_size ) +
  geom_line(data=dataZH,aes(y= HospInc,x=as.POSIXct(Reporting),colour="Total"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("12 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims3), max(lims4)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[2]))+
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  ggtitle("Hospitalisations  total") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))
# legend.key.size = unit(2, 'cm'))


FigureTemp <- ggplot() +  
  # annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.4,fill="orange") +
  annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +

  # geom_line(data=dataZH  ,aes(y=mean_maxium,x= as.POSIXct(Reporting),colour="Maximum"), lwd=lwd_size )+
  geom_line(data=dataZH ,aes(y=mean_mean,x=as.POSIXct(Reporting),colour="Mean"), lwd=lwd_size ) +
  # geom_line(data=dataZH ,aes(y=mean_minimum,x=as.POSIXct(Reporting),colour="Minimum"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("12 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims3), max(lims4)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[3]))+
  xlab("Month/Year")+
  ylab("Temperatur in C")+
  ggtitle("Mean Temperatur") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        axis.text.x = element_text(size=10,angle=45,hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        legend.text=element_text(size=legend_size),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size), 
        title =element_text(size=title_size))

plot_zurich <- cowplot::plot_grid(FigureInc,FigureDeath,FigureHospInfl,FigureHospital, FigureTemp,
                                  ncol=1, nrow=5, align="hv",
                                  rel_heights = c(1,1,1,1,1))


return(plot_zurich)
# 
# plot_zurich <- cowplot::plot_grid(FigureInc,NULL,FigureDeath,NULL,FigureHospital, 
#                                   ncol=1, nrow=5, align="hv",
#                                   rel_heights = c(1,-0.1,1,-0.1,1))


}

# cowplot::save_plot("output/plot_zurich.pdf", plot_zurich ,base_height=15,base_width=20)
