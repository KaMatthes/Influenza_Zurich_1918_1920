function_ts_marriage <- function() {
  
data_pop <- read.csv("../data_raw/Data_Zurich.csv") %>%
  dplyr::select(Year,PopCity) %>%
  distinct(Year, .keep_all = TRUE)

data_zh <- read_excel("../data_raw/Daten_Bulletins.xlsx", sheet=1) %>%
  dplyr::select(Wochenende, Eheschliessungen, Lebendgeburten_gr,Totgeburten_gr) %>%
  mutate(Wochenende = ymd(Wochenende),
         KW = isoweek(Wochenende),
         Woche= Wochenende,
         Year = as.numeric(format(Woche,'%Y'))) %>%
  left_join(data_pop)%>%
  mutate(Eheschliessungen = as.numeric(Eheschliessungen),
         Ehe = Eheschliessungen/PopCity*100000,
         Geburten_sum = Lebendgeburten_gr+Totgeburten_gr,
         Geburt_l_rate = Lebendgeburten_gr/Geburten_sum*100,
         Geburt_t_rate = Totgeburten_gr/Geburten_sum*100,
         Geburt_inc = round(Geburten_sum/PopCity*100000,2))


data_marriage_ts <- data_zh %>%
  filter(!KW==53) %>%
  dplyr::select(Ehe)%>%
  ts(frequency = 52, start =1910)

bp_marriage_ts <- breakpoints(data_marriage_ts ~ 1)

plot_ts_marriage <- data_marriage_ts %>%
  decompose(type = "additive") %>%
  autoplot(range.bars = FALSE) +
  geom_vline(xintercept = 1914.308, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1914.615, linetype="dashed", lwd=1, col="red") +
  geom_vline(xintercept = 1915.192, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1919.096, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1919.250, linetype="dashed",lwd=1, col="red") +
  geom_vline(xintercept = 1919.327, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1920.769, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1921.827, linetype="dashed",lwd=1, col="red") +
  geom_vline(xintercept = 1923, linetype="dotted", lwd=1, col="red") +
  annotate("rect",xmin=1914.577,xmax=1918.865,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=1918.712,xmax=1919.365,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1920.019,xmax=1920.404,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1921.981,xmax=1922.250,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1924.038,xmax=1924.365,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1925.038,xmax=1925.385,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  xlab("Year") +
  ylab("Marriages per 100'000 inhabitants") +
  ggtitle("Time series - Number of Marriages") +
  theme_bw() +
  theme( axis.text = element_text(size=  size_axis),
         axis.title = element_text(size=  size_axis_title),
         title =element_text(size=title_size))


return(plot_ts_marriage)
}

# cowplot::save_plot("output/plot_zurich.pdf", plot_zurich ,base_height=15,base_width=20)
