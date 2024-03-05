function_plot_weekly_spa <- function(Plot_var) {

  load("../data/dataZH.RData")

  load("../data/expected_death_inla_weekly1918.RData")
  excess1918 <-   expected_deaths
  load("../data/expected_death_inla_weekly1920.RData")
  excess1920 <-   expected_deaths

  # load("data/dataZH.RData")
  # load("data/expected_death_inla_weekly1918.RData")
  # excess1918 <-   expected_deaths
  # load("data/expected_death_inla_weekly1920.RData")
  # excess1920 <-   expected_deaths

  
data_excess <- rbind( excess1918,  excess1920) %>%
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
         Reporting = Reporting +2,
         Difference_sig =  ifelse( excess_death > 0, "More than expected", "Fewer than expected"),
         Difference_sig= replace( Difference_sig, significant_dummy==1 & Difference_sig=="More than expected", "Significant more"))
         # Difference_sig= replace(  Difference_sig,significant_dummy==1 & Difference_sig=="Fewer than expected", "Significant more"))

CityZH_pop <- dataZH %>%
  select(Year, iso_cw,pop.weekly )

data_deaths_inf  <-  read_excel("../data_raw/Delay_Meldungen.xlsx", sheet="Totesfaelle") %>%
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
  group_by(Meldung) %>%
  mutate(Meldung_sum = sum(Number)) %>%
  ungroup() %>%
  distinct(Meldung, .keep_all = TRUE) %>%
  select( Year,iso_meld , Meldung_sum,Meldung) %>%
  rename(iso_cw=iso_meld) %>%
  rename(Reporting = Meldung) %>%
  left_join(CityZH_pop) %>%
  filter(!is.na(pop.weekly)) %>%
  mutate(death_inf_inc = Meldung_sum/pop.weekly*10000)
  
if(Plot_var=="Incidence") {
Figure_curve <- ggplot() +

  geom_line(data=dataZH ,aes(y=infl_inc ,x= Reporting,colour="City of Zurich"), lwd=lwd_size)+
  geom_line(data=dataZH,aes(y=infl_inc_canton,x=Reporting,colour="Canton Zurich"), lwd=lwd_size ) +
  # scale_x_datetime( breaks = date_breaks("2 month"),
  #                   labels = label_date_short(),
  #                   limits =c(min(lims5), max(lims6)))+
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  geom_vline(data=table_legend, aes(xintercept = date,colour = col_value), linetype = "dashed", lwd=lwd_size_vline ) + 
  # geom_vline(data=table_legend, aes(xintercept = as.Date("02.12.1918", "%d.%m.%Y")),col="green", linetype = "dashed", lwd=1) + 
  geom_label(data=table_legend, aes(y=Y_value, x=date,  label= key,colour = col_value), size=10,show.legend = FALSE) +
  # geom_textbox(data=text_plot,aes(x=x, y=y, label=label), width = grid::unit(0.45, "npc"),
  #              height = grid::unit(0.40, "npc"), size=4) +
  scale_color_manual(name = "",
                     label =c("City of Zurich","Canton Zurich","Federal state"),
                     values = c(col_pal[1],  col_pal[4], "black"))+
  xlab("Calendar week/Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("Influenza Incidence") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.07, .85),
    legend.text=element_text(size=legend_size),
    # legend.key.size = unit(1.5, 'cm'),
    # legend.spacing.x = unit(1.5, 'cm'),
    axis.text.x = element_text(size=size_axis_x,angle=45,hjust=1),
    axis.title.x  = element_text(size=axis_legend_size),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size))
}

else if(Plot_var=="Death") {
  
Figure_curve <- ggplot() +
  geom_line(data=data_deaths_inf ,aes(y=death_inf_inc,x= Reporting,col="City of Zurich"), lwd=lwd_size )+
  # geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed",lwd=lwd_size_vline) + 
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                     values =col_pal[1])+
  xlab("Calendar week/Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("Influenca death - City of Zurich") +
  theme_bw()+
  #theme_light(base_size = 16)+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_text(size=size_axis_x,angle=45,hjust=1),
        axis.title.x  = element_text(size=axis_legend_size),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))
}

else if(Plot_var=="Mortality") {
  
Figure_curve <- ggplot() +
  geom_line(data=dataZH ,aes(y=death_inc,x= Reporting,col="City of Zurich"), lwd=lwd_size )+
  # geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed",lwd=lwd_size_vline) + 
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                   values =col_pal[1])+
  xlab("Calendar week/Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("All cause mortality - City of Zurich") +
  theme_bw()+
  #theme_light(base_size = 16)+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_text(size=size_axis_x,angle=45,hjust=1),
        axis.title.x  = element_text(size=axis_legend_size),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))
}

else if(Plot_var=="Excess") {
  
Figure_curve <- ggplot() +
 geom_col(data= dataZH,aes(x= Reporting,y = rel_excess_death, fill= Difference_sig)) +
  # geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed",lwd=lwd_size_vline) + 
  # geom_ribbon(data=dataZH,aes(ymin=LL_inc, ymax=UL_inc, x=as.POSIXct(Reporting),fill="CI_area"), alpha=0.2) +
  # geom_line(data=dataZH ,aes(y=death_inc,x=as.POSIXct(Reporting),colour="death"), lwd=lwd_size ) +
  # geom_line(data=dataZH ,aes(y=fit_inc,x=as.POSIXct(Reporting),colour="fit"), lwd=lwd_size ) +
  # geom_line(data=dataZH ,aes(y=mean_minimum,x=as.POSIXct(Reporting),colour="Minimum"), lwd=lwd_size ) +
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_fill_manual("",
                   breaks=c("Fewer than expected","More than expected","Significant more"),
                    values =c("#a6d96a",col_pal[2],"#ca0020")) +
  xlab("Calendar week/Year")+
  ylab("Relatitve excess mortality in %")+
  ggtitle("Relative Excess Mortality") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_text(size=size_axis_x,angle=45,hjust=1),
        axis.title.x  = element_text(size=axis_legend_size),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))
}

else if(Plot_var=="Hospital") {
Figure_curve <- ggplot() +
  geom_line(data=dataZH,aes(y=AndereInc,x=Reporting,colour="Canton Zurich"), lwd=lwd_size ) +
  # geom_line(data=dataZH,aes(y= HospInc,x=as.POSIXct(Reporting),colour="Total"), lwd=lwd_size ) +
  # geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed",lwd=lwd_size_vline) + 
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                     values = c(col_pal[4]))+
  xlab("Calendar week/Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("Hospitalisations - Infections incl. influenza - Canton Zurich") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.1, .85),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_text(size=size_axis_x,angle=45,hjust=1),
        axis.title.x  = element_text(size=axis_legend_size),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))
}

return(Figure_curve)
# 
# plot_zurich <- cowplot::plot_grid(FigureInc,NULL,FigureDeath,NULL,FigureHospital, 
#                                   ncol=1, nrow=5, align="hv",
#                                   rel_heights = c(1,-0.1,1,-0.1,1))


}

# cowplot::save_plot("output/plot_zurich.pdf", plot_zurich ,base_height=15,base_width=20)
