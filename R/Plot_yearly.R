function_plot_yearly <- function() {

  load("../data/dataZH_month.RData")
  
  
  # load("data/dataZH_month.RData")
  
  
  dataZH_year <- dataZH_month %>%
    mutate(
      death_m = ifelse(is.na(death_m), 0,death_m),
      influenza_m = ifelse(is.na(influenza_m), 0,influenza_m))%>%
    group_by(Year,CityZurich) %>%
    summarise(death_year = sum(death_m, na.rm=TRUE),
              Inf_year = sum(influenza_m, na.rm=TRUE)) %>%
    ungroup()
              
  
  load("../data/expected_death_inla_year1918.RData")
  excess1918 <-   expected_deaths
  load("../data/expected_death_inla_year1920.RData")
  excess1920 <-   expected_deaths
  load("../data/expected_death_inla_year1929.RData")
  excess1929 <-   expected_deaths
  load("../data/expected_death_inla_year1944.RData")
  excess1944 <-   expected_deaths
  load("../data/expected_death_inla_year1961.RData")
  excess1961 <-   expected_deaths
  
  # load("data/expected_death_inla_year1918.RData")
  # excess1918 <-   expected_deaths
  # load("data/expected_death_inla_year1920.RData")
  # excess1920 <-   expected_deaths
  # load("data/expected_death_inla_year1929.RData")
  # excess1929 <-   expected_deaths
  # load("data/expected_death_inla_year1944.RData")
  # excess1944 <-   expected_deaths
  # load("data/expected_death_inla_year1961.RData")
  # excess1961 <-   expected_deaths
  # 
data_excess <- rbind( excess1918,  excess1920, excess1929, excess1944, excess1961) %>%
  tibble() 

dataZH <- data_excess %>%
  full_join(  dataZH_year)   %>%
  filter(!is.na(Year)) %>%
  mutate(death_year=replace(death_year,  Year==1910, 2514),
         death_year=replace(death_year,  Year==1911, 2665),
         death_year=replace(death_year,  Year==1912, 2555),
         death_year=replace(death_year,  Year==1913, 2536),
         death_year=replace(death_year,  Year==1914, 2447),
         death_inc = death_year/CityZurich*10000,
         InfluenzaInc = Inf_year/CityZurich*10000,
         fit_inc = fit/CityZurich*10000,
         LL_inc = LL/CityZurich*10000,
         UL_inc = UL/CityZurich*10000,
         excess_death = death_year-fit,
         rel_excess_death = excess_death/fit*100,
         significant_dummy = ifelse(death_year > LL & death_year < UL,0,1),
         significant_dummy = as.factor( significant_dummy),
         Difference_sig =  ifelse( excess_death > 0, "More than expected", "Fewer than expected"),
         Difference_sig= replace( Difference_sig, significant_dummy==1 & Difference_sig=="More than expected", "Significant more"),
         InfluenzaInc =ifelse(is.na(InfluenzaInc), 0, InfluenzaInc),
         Reporting = paste0(Year,"0101"),
         Reporting = ymd(Reporting)) %>%
  arrange(Year) %>%
  filter(!Year > 1968) 

         
         # Difference_sig= replace(  Difference_sig,significant_dummy==1 & Difference_sig=="Fewer than expected", "Significant more"))

         
FigureInc <- ggplot() +
  annotate("rect",xmin=as.POSIXct("1910-07-02"),xmax=as.POSIXct("1911-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=as.POSIXct("1917-07-02"),xmax=as.POSIXct("1918-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1919-07-02"),xmax=as.POSIXct("1920-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1928-07-02"),xmax=as.POSIXct("1929-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=as.POSIXct("1943-07-02"),xmax=as.POSIXct("1944-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1946-07-02"),xmax=as.POSIXct("1947-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=as.POSIXct("1951-07-02"),xmax=as.POSIXct("1952-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=as.POSIXct("1955-07-02"),xmax=as.POSIXct("1956-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=as.POSIXct("1957-07-02"),xmax=as.POSIXct("1958-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1962-07-02"),xmax=as.POSIXct("1963-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=as.POSIXct("1967-07-02"),xmax=as.POSIXct("1968-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # geom_line(data=dataZH ,aes(y=InfluenzaInc,x= Year,colour="City of Zurich"), lwd=lwd_size )+
  geom_bar(data=dataZH, aes(x = as.POSIXct(Reporting), y =InfluenzaInc,fill="City of Zurich"),stat="identity") +
  # geom_line(data=dataZH,aes(y=InfluenzaCantonInc,x=as.POSIXct(Reporting),colour="Canton Zurich"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("12 month"),
                    labels = label_date_short(),
                    limits =c(min(lims3), max(lims4)),
                    expand = c(0,0)) +
  scale_fill_manual(name = "",
                     values = col_pal[1])+
  # xlim(1910,1968)+
  xlab("Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("Influenza cases") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.9, .8),
    legend.text=element_text(size=legend_size),
    # legend.key.size = unit(1.5, 'cm'),
    # legend.spacing.x = unit(1.5, 'cm'),
    axis.text.x = element_text(size=10,angle=45,hjust=1),
    axis.title.x  = element_blank(),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size))
         


FigureDeath <- ggplot() +
  annotate("rect",xmin=as.POSIXct("1910-07-02"),xmax=as.POSIXct("1911-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=as.POSIXct("1917-07-02"),xmax=as.POSIXct("1918-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1919-07-02"),xmax=as.POSIXct("1920-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1928-07-02"),xmax=as.POSIXct("1929-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=as.POSIXct("1943-07-02"),xmax=as.POSIXct("1944-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1946-07-02"),xmax=as.POSIXct("1947-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=as.POSIXct("1951-07-02"),xmax=as.POSIXct("1952-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=as.POSIXct("1955-07-02"),xmax=as.POSIXct("1956-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=as.POSIXct("1957-07-02"),xmax=as.POSIXct("1958-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1962-07-02"),xmax=as.POSIXct("1963-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=as.POSIXct("1967-07-02"),xmax=as.POSIXct("1968-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_bar(data=dataZH, aes(x = as.POSIXct(Reporting), y =death_inc,fill="Death"),stat="identity") +
  scale_x_datetime( breaks = date_breaks("12 month"),
                    labels = label_date_short(),
                    limits =c(min(lims3), max(lims4)),
                    expand = c(0,0)) +
  scale_fill_manual(name = "",
                   values =col_pal[7])+
  # xlim(1910,1968)+
  xlab("Year")+
  ylab("per 10'000 inhab.")+
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
        axis.text.x = element_text(size=10,angle=45,hjust=1),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

# FigureTemp <- ggplot() +  
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
#         axis.text.x = element_text(size=10,angle=45,hjust=1),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = "none",
#         legend.text=element_text(size=legend_size),
#         axis.title.x  = element_blank(),
#         axis.title.y  = element_text(size=axis_legend_size), 
#         title =element_text(size=title_size))




FigureExcess<- ggplot() +  

  annotate("rect",xmin=as.POSIXct("1910-07-02"),xmax=as.POSIXct("1911-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=as.POSIXct("1917-07-02"),xmax=as.POSIXct("1918-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1919-07-02"),xmax=as.POSIXct("1920-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1928-07-02"),xmax=as.POSIXct("1929-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=as.POSIXct("1943-07-02"),xmax=as.POSIXct("1944-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1946-07-02"),xmax=as.POSIXct("1947-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=as.POSIXct("1951-07-02"),xmax=as.POSIXct("1952-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=as.POSIXct("1955-07-02"),xmax=as.POSIXct("1956-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=as.POSIXct("1957-07-02"),xmax=as.POSIXct("1958-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=as.POSIXct("1962-07-02"),xmax=as.POSIXct("1963-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=as.POSIXct("1967-07-02"),xmax=as.POSIXct("1968-06-30"),ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_col(data= dataZH,aes(x= as.POSIXct(Reporting),y = rel_excess_death, fill= Difference_sig)) +
  # geom_ribbon(data=dataZH,aes(ymin=LL_inc, ymax=UL_inc, x=as.POSIXct(Reporting),fill="CI_area"), alpha=0.2) +
  # geom_line(data=dataZH ,aes(y=death_inc,x=as.POSIXct(Reporting),colour="death"), lwd=lwd_size ) +
  # geom_line(data=dataZH ,aes(y=fit_inc,x=as.POSIXct(Reporting),colour="fit"), lwd=lwd_size ) +
  # geom_line(data=dataZH ,aes(y=mean_minimum,x=as.POSIXct(Reporting),colour="Minimum"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("12 month"),
                    labels = label_date_short(),
                    limits =c(min(lims3), max(lims4)),
                    expand = c(0,0)) +
  scale_fill_manual("",
                   breaks=c("Fewer than expected","More than expected","Significant more"),
                    values =c("#a6d96a",col_pal[2],"#ca0020")) +
  xlab("Year")+
  ylab("Relatitve excess mortality in %")+
  # xlim(1910,1968)+
  ggtitle("Relative Excess Mortality") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        axis.text.x = element_text(size=10,angle=45,hjust=1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.9, .8),
        legend.text=element_text(size=legend_size),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size), 
        title =element_text(size=title_size))

# 
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


plot_zurich <- cowplot::plot_grid(FigureInc,FigureDeath,FigureExcess, 
                                  ncol=1, nrow=3, align="hv",
                                  rel_heights = c(1,1,1))


return(plot_zurich)
# 
# plot_zurich <- cowplot::plot_grid(FigureInc,NULL,FigureDeath,NULL,FigureHospital, 
#                                   ncol=1, nrow=5, align="hv",
#                                   rel_heights = c(1,-0.1,1,-0.1,1))


}

# cowplot::save_plot("output/plot_zurich.pdf", plot_zurich ,base_height=15,base_width=20)
