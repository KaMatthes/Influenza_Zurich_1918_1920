load("data/dataZH.RData")


dt <- dataZH %>%
# filter(Reporting >= ymd("1918-05-01")) %>%
  mutate(cases=CantonCases,
         cases_1 = rollsum(x = cases, 2, align = "right", fill = NA) - cases,
         cases_2 = rollsum(x = cases_1, 2, align = "right", fill = NA) - cases_1,
         inc = cases/pop.weekly,
         inc_1 = cases_1/pop.weekly,
         inc_2 = cases_2/pop.weekly,
         RIRR = (cases/cases_1)/(cases_1/cases_2),
         RIRR = ifelse(RIRR==Inf, 0, RIRR),
         RIRR_inc = (inc/inc_1)/(inc_1/inc_2),
         RIRR_inc = ifelse(RIRR_inc==Inf, 0, RIRR_inc),
         RIRR_Var =  1/cases + 4/cases_1 + 1/cases_2,
         RIRR_Var =  ifelse(RIRR_Var==Inf, 0, RIRR_Var),
         RIRR_SE = sqrt( RIRR_Var),
         LL =  exp(log(RIRR) - 1.96* RIRR_SE),
         UL=  exp(log(RIRR) + 1.96* RIRR_SE),
         RIRR = ifelse(Reporting > ymd("1919-03-29") &  Reporting< ymd("1920-01-01"), NA, RIRR),
         LL = ifelse(Reporting > ymd("1919-03-29") &  Reporting< ymd("1920-01-17"), NA, LL),
         UL = ifelse(Reporting > ymd("1919-03-29") &  Reporting< ymd("1920-01-17"), NA, UL)) %>%
  select(Reporting, iso_cw,cases,cases_1,cases_2,RIRR, LL, UL)

ggplot() +
  geom_hline(yintercept=1, col="darkgrey", lwd=0.8)+
  geom_line(data=dt ,aes(y=RIRR ,x= Reporting,col="RIRR (95% CI)"), lwd=lwd_size) +
  geom_ribbon(data=dt,aes(ymin=LL, ymax=UL,x=Reporting), fill=col_pal[8],linetype=2, alpha=0.3) +
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("1 month"),
               limits =c(min(lims5), max(lims6))) +
  scale_y_continuous(breaks=seq(0,10,2),
                     limits=c(0,10))+
  # geom_vline(data=table_legend, aes(xintercept = as.Date("02.12.1918", "%d.%m.%Y")),col="green", linetype = "dashed", lwd=1) + 
  # annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  scale_color_manual(name = "",
                     limits =c("RIRR (95% CI)"),
                     values = c(col_pal[8])) +
  xlab("Month/Year")+
  ylab("RIRR")+
  ggtitle("RIRR - Recorded cases") +
  theme_bw()+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.62, .82),
        legend.text=element_text(size=25),
        axis.text.x = element_text(size=18),
        axis.title.x  =element_text(size=18),
        axis.title.y  = element_text(size=18),
        title =element_text(size=title_size))



ggsave("output/RIRR.png" ,h=10,w=25)