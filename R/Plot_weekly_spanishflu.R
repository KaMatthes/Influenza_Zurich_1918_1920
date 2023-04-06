function_plot_weekly_spa <- function() {

  load("../data/dataZH.RData")
  
  load("../data/expected_death_inla_weekly1918.RData")
  excess1918 <-   expected_deaths
  load("../data/expected_death_inla_weekly1920.RData")
  excess1920 <-   expected_deaths
  load("../data/expected_death_inla_weekly1929.RData")
  excess1929 <-   expected_deaths
  load("../data/expected_death_inla_weekly1944.RData")
  excess1944 <-   expected_deaths

  # load("data/dataZH.RData")
  # load("data/expected_death_inla_weekly1918.RData")
  # excess1918 <-   expected_deaths
  # load("data/expected_death_inla_weekly1920.RData")
  # excess1920 <-   expected_deaths
  # load("data/expected_death_inla_weekly1929.RData")
  # excess1929 <-   expected_deaths
  # load("data/expected_death_inla_weekly1944.RData")
  # excess1944 <-   expected_deaths

  
data_excess <- rbind( excess1918,  excess1920, excess1929,  excess1944) %>%
  tibble() %>%
  mutate(Cw_year= paste0(Year,"/W",iso_cw ))

dataZH <- dataZH %>%
  left_join(data_excess) %>%
  mutate(death_inc = CityDeathsTotal/pop.weekly*10000,
         infl_inc = CityCases/pop.weekly*10000,
         infl_inc_canton = CantonCases/pop.weekly.canton*10000,
         fit_inc = fit/pop.weekly*10000,
         LL_inc = LL/pop.weekly*10000,
         UL_inc = UL/pop.weekly*10000,
         HospInfInc = HospInfInc*10,
         HospInc = HospInc*10,
         AndereInc = AndereInc*10,
         excess_death = death-fit,
         rel_excess_death = excess_death/fit*100,
         significant_dummy = ifelse(death > LL & death < UL,0,1),
         significant_dummy = as.factor( significant_dummy),
         Difference_sig =  ifelse( excess_death > 0, "More than expected", "Fewer than expected"),
         Difference_sig= replace( Difference_sig, significant_dummy==1 & Difference_sig=="More than expected", "Significant more"))
         # Difference_sig= replace(  Difference_sig,significant_dummy==1 & Difference_sig=="Fewer than expected", "Significant more"))



FigureInc <- ggplot() +
  # annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # # annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # # annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # # annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim49,xmax=datlim50,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim51,xmax=datlim52,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +

  geom_line(data=dataZH ,aes(y=infl_inc ,x= Reporting,colour="City of Zurich"), lwd=lwd_size )+
  geom_line(data=dataZH,aes(y=infl_inc_canton,x=Reporting,colour="Canton Zurich"), lwd=lwd_size ) +
  # scale_x_datetime( breaks = date_breaks("2 month"),
  #                   labels = label_date_short(),
  #                   limits =c(min(lims5), max(lims6)))+
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed") + 
  geom_label(data=table_legend, aes(y=350, x=date,  label= key)) +
  geom_textbox(data=text_plot,aes(x=x, y=y, label=label), width = grid::unit(0.55, "npc"),
               height = grid::unit(0.4, "npc"), size=5.5) +
  scale_color_manual(name = "",
                     values = c(col_pal[4],  col_pal[1]))+
  xlab("Calendar week/Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("Influenza Incidence") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.05, .85),
    legend.text=element_text(size=legend_size),
    # legend.key.size = unit(1.5, 'cm'),
    # legend.spacing.x = unit(1.5, 'cm'),
    axis.text.x = element_text(size=10,angle=45,hjust=1),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size))
         


FigureDeath <- ggplot() +
  # # annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  # annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # # annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # # annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # # annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim49,xmax=datlim50,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim51,xmax=datlim52,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=dataZH ,aes(y=death_inc,x= Reporting,col="Death"), lwd=lwd_size )+
  geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed") + 
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                   values =col_pal[7])+
  xlab("Calendar week/Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("All cause mortality") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_text(size=10,angle=45,hjust=1),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))


FigureHospInfl <- ggplot() +
  # annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # # annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # # annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # # annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # # annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim49,xmax=datlim50,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim51,xmax=datlim52,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # geom_line(data=dataZH ,aes(y=HospInfInc,x= as.POSIXct(Reporting),colour="Total minus other infections"), lwd=lwd_size )+
  geom_line(data=dataZH,aes(y=AndereInc,x=Reporting,colour="Infections incl. influenza"), lwd=lwd_size ) +
  # geom_line(data=dataZH,aes(y= HospInc,x=as.POSIXct(Reporting),colour="Total"), lwd=lwd_size ) +
  geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed") + 
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                     values = c(col_pal[8]))+
  xlab("Calendar week/Year")+
  ylab("per 10'000 inhab.")+
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
        axis.text.x = element_text(size=10,angle=45,hjust=1),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

    # legend.key.size = unit(2, 'cm'))

# FigureHospital <- ggplot() +
#   # annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
#   annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim49,xmax=datlim50,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim51,xmax=datlim52,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # geom_line(data=dataZH ,aes(y=HospInfInc,x= as.POSIXct(Reporting),colour="Total minus other infections"), lwd=lwd_size )+
#   # geom_line(data=dataZH,aes(y=AndereInc,x=as.POSIXct(Reporting),colour="Infections incl. influenza"), lwd=lwd_size ) +
#   geom_line(data=dataZH,aes(y= HospInc,x=as.POSIXct(Reporting),colour="Total"), lwd=lwd_size ) +
#   scale_x_datetime( breaks = date_breaks("12 month"), 
#                     labels = label_date_short(),
#                     limits =c(min(lims3), max(lims4)),
#                     expand = c(0,0)) +
#   scale_color_manual(name = "",
#                      values = c(col_pal[2]))+
#   xlab("Month/Year")+
#   ylab("per 10'000 inhab.")+
#   ggtitle("Hospitalisations  total") +
#   theme_bw()+
#   #theme_light(base_size = 16)+
#   theme(axis.text.y = element_text(size=text_size),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = "none",
#         legend.text=element_text(size=legend_size),
#         # legend.key.size = unit(1.5, 'cm'),
#         # legend.spacing.x = unit(1.5, 'cm'),
#     
#         axis.text.x = element_text(size=10,angle=45,hjust=1),
#         axis.title.x  = element_blank(),
#         axis.title.y  = element_text(size=axis_legend_size),
#         title =element_text(size=title_size))
# # legend.key.size = unit(2, 'cm'))
# 
# 
# FigureTemp <- ggplot() +  
#   # annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
#   annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim49,xmax=datlim50,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim51,xmax=datlim52,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
# 
#   # geom_line(data=dataZH  ,aes(y=mean_maxium,x= as.POSIXct(Reporting),colour="Maximum"), lwd=lwd_size )+
#   geom_line(data=dataZH ,aes(y=mean_mean,x=as.POSIXct(Reporting),colour="Mean"), lwd=lwd_size ) +
#   # geom_line(data=dataZH ,aes(y=mean_minimum,x=as.POSIXct(Reporting),colour="Minimum"), lwd=lwd_size ) +
#   scale_x_datetime( breaks = date_breaks("12 month"), 
#                     labels = label_date_short(),
#                     limits =c(min(lims3), max(lims4)),
#                     expand = c(0,0)) +
#   scale_color_manual(name = "",
#                      values = c(col_pal[3]))+
#   xlab("Month/Year")+
#   ylab("Temperatur in C")+
#   ggtitle("Mean Temperatur") +
#   theme_bw()+
#   #theme_light(base_size = 16)+
#   theme(axis.text.y = element_text(size=text_size),
#         axis.text.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = "none",
#         legend.text=element_text(size=legend_size),
#         axis.title.x  = element_blank(),
#         axis.title.y  = element_text(size=axis_legend_size), 
#         title =element_text(size=title_size))
# 
# 
# 
# 
# FigureExcess<- ggplot() +  
# 
#   # annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
#   annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim49,xmax=datlim50,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim51,xmax=datlim52,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   geom_col(data= dataZH,aes(x= as.POSIXct(Reporting),y = rel_excess_death, fill= Difference_sig)) +
#   # geom_ribbon(data=dataZH,aes(ymin=LL_inc, ymax=UL_inc, x=as.POSIXct(Reporting),fill="CI_area"), alpha=0.2) +
#   # geom_line(data=dataZH ,aes(y=death_inc,x=as.POSIXct(Reporting),colour="death"), lwd=lwd_size ) +
#   # geom_line(data=dataZH ,aes(y=fit_inc,x=as.POSIXct(Reporting),colour="fit"), lwd=lwd_size ) +
#   # geom_line(data=dataZH ,aes(y=mean_minimum,x=as.POSIXct(Reporting),colour="Minimum"), lwd=lwd_size ) +
#   scale_x_datetime( breaks = date_breaks("12 month"), 
#                     labels = label_date_short(),
#                     limits =c(min(lims3), max(lims4)),
#                     expand = c(0,0)) +
#   scale_fill_manual("",
#                    breaks=c("Fewer than expected","More than expected","Significant more"),
#                     values =c("#a6d96a",col_pal[2],"#ca0020")) +
#   xlab("Month/Year")+
#   ylab("Relatitve excess mortality in %")+
#   ggtitle("Relative Excess Mortality") +
#   theme_bw()+
#   #theme_light(base_size = 16)+
#   theme(axis.text.y = element_text(size=text_size),
#         axis.text.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = c(.9, .8),
#         legend.text=element_text(size=legend_size),
#         axis.title.x  = element_blank(),
#         axis.title.y  = element_text(size=axis_legend_size), 
#         title =element_text(size=title_size))
# 
# # 
# FigureExcessFit <- ggplot() +  
#   
#   # annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
#   annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # geom_col(data= dataZH,aes(x= as.POSIXct(Reporting),y = rel_excess_death, fill= Difference_sig)) +
#   geom_ribbon(data=dataZH,aes(ymin=LL_inc, ymax=UL_inc, x=as.POSIXct(Reporting)),fill="darkgrey", alpha=0.5) +
#   geom_line(data=dataZH ,aes(y=death_inc,x=as.POSIXct(Reporting)),colour=col_pal[2], lwd=lwd_size ) +
#   # geom_line(data=dataZH ,aes(y=fit_inc,x=as.POSIXct(Reporting),colour="fit"), lwd=lwd_size ) +
#   scale_x_datetime( breaks = date_breaks("12 month"), 
#                     labels = label_date_short(),
#                     limits =c(min(lims3), max(lims4)),
#                     expand = c(0,0)) +
#   xlab("Month/Year")+
#   ylab("Relative excess mortality in %")+
#   ggtitle("Relative Excess Mortality") +
#   theme_bw()+
#   #theme_light(base_size = 16)+
#   theme(axis.text.y = element_text(size=text_size),
#         axis.text.x = element_text(size=10,angle=45,hjust=1),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = c(.9, .8),
#         legend.text=element_text(size=legend_size),
#         axis.title.x  = element_blank(),
#         axis.title.y  = element_text(size=axis_legend_size), 
#         title =element_text(size=title_size))


plot_zurich <- cowplot::plot_grid(FigureInc,FigureDeath,FigureHospInfl, 
                                  ncol=1, nrow=3, align="hv",
                                  rel_heights = c(1,1,1))


return(plot_zurich)
# 
# plot_zurich <- cowplot::plot_grid(FigureInc,NULL,FigureDeath,NULL,FigureHospital, 
#                                   ncol=1, nrow=5, align="hv",
#                                   rel_heights = c(1,-0.1,1,-0.1,1))


}

# cowplot::save_plot("output/plot_zurich.pdf", plot_zurich ,base_height=15,base_width=20)
