function_ts_marriage_birth <- function() {
  
data_pop <- read.csv("../data_raw/Data_Zurich.csv") %>%
  select(Year,PopCity) %>%
  distinct(Year, .keep_all = TRUE)

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


data_zh <- read_excel("../data_raw/Daten_Bulletins.xlsx", sheet=1) %>%
  select(Wochenende, Lebendgeburten_gr,Totgeburten_gr) %>%
  mutate(Wochenende_compare = ymd(Wochenende)) %>%
  right_join(data_compare ) %>%
  left_join(data_zh_ehe) %>%
  mutate(KW = isoweek(Wochenende_compare),
         Eheschliessungen = as.numeric(Eheschliessungen),
         Geburten_sum = Lebendgeburten_gr+Totgeburten_gr,
         Marriage_Birth_Rate = round(Geburten_sum/Eheschliessungen,2),
         Year = as.numeric(format(Wochenende_compare,'%Y')),
         Month = as.numeric(format(Wochenende_compare,'%m'))) %>%
  arrange(Wochenende_compare) %>%
  filter(!is.na(Geburten_sum)) %>%
  group_by(Year, Month) %>%
  mutate(sum_birth = sum(Geburten_sum),
         sum_ehe = sum(Eheschliessungen)) %>%
  ungroup() %>%
  distinct(Year, Month, .keep_all = TRUE)  %>%
  mutate(Marriage_Birth_Rate =round(sum_birth/sum_ehe,2))



FigureInc <- ggplot() +
  geom_line(data=data_zh,aes(y=Marriage_Birth_Rate,x= as.POSIXct(Wochenende_compare),colour="Marriages"), lwd=lwd_size )+
  geom_line(data=data_zh,aes(y=Geburt_inc,x=as.POSIXct(Woche),colour="Births"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims1), max(lims2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[4],  col_pal[1]))+
  scale_y_continuous(
    name = "Marriages per 10'000 inhabitants",
    sec.axis = sec_axis(~.*1, name = "Birth per 10'000 inhabitants")
  ) +
  xlab("Month/Year")+
  ylab("per 1,000 inhab.")+
  ggtitle("Influenza cases") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
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

return(plot_ts_marriage_birth)
}

# cowplot::save_plot("output/plot_zurich.pdf", plot_zurich ,base_height=15,base_width=20)
