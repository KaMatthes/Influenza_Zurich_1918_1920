load("data/dataZH.RData")

load("data/expected_death_inla_weekly1918.RData")

data_excess <- expected_deaths %>%
  tibble() %>%
  mutate(Cw_year= paste0(Year,"/W",iso_cw )) %>%
  left_join(dataZH) 

ggplot() +
  geom_line(data=data_excess ,aes(y=death ,x= Reporting,col="observed deaths"), lwd=lwd_size) +
  geom_line(data=data_excess ,aes(y=fit ,x= Reporting,col="expected deaths"), lwd=lwd_size) +
  geom_ribbon(data=data_excess,aes(ymin=LL, ymax=UL,x=Reporting),fill=col_pal[8],linetype=2, alpha=0.3) +
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("1 month"),
               limits =c(min(lims5), max(lims6))) +
  # geom_vline(data=table_legend, aes(xintercept = as.Date("02.12.1918", "%d.%m.%Y")),col="green", linetype = "dashed", lwd=1) + 
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  scale_color_manual(name = "",
                     # label =c("observed deaths","expected deaths"),
                     values = c(col_pal[8],  col_pal[2])) +
  xlab("Month/Year")+
  ylab("Number of deaths")+
  # ggtitle("A) Incidence of reported flu cases") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.62, .82),
        legend.text=element_text(size=25),
        axis.text.x = element_text(size=18),
        axis.title.x  =element_text(size=18),
        axis.title.y  = element_text(size=18),
        title =element_text(size=title_size))


ggsave("output/INLA_modelfit.png" ,h=10,w=25)