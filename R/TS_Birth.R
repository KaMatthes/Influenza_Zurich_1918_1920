function_ts_birth <- function() {
  
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
         Geburt_t_rate = Totgeburten_gr/Geburten_sum*100,
         Geburt_inc = round(Geburten_sum/PopCity*100000,2))


data_birth_ts <- data_zh %>%
  filter(!KW==53) %>%
  dplyr::select(Geburt_inc)%>%
  ts(frequency = 52, start =1910)

# Breakpoint 38 Wochen!!! später als bei den Eheschliessungen
# 25 Wochen später als bei den Eheschliessungen
bp_birth_ts <- breakpoints(data_birth_ts ~ 1)

plot_ts_birth <- data_birth_ts %>%
  decompose(type = "additive") %>%
  autoplot(range.bars = FALSE) +
  geom_vline(xintercept = 1912.442, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1912.635, linetype="dashed", lwd=1, col="red") +
  geom_vline(xintercept = 1912.827, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1915.308, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1915.346, linetype="dashed",lwd=1, col="red") +
  geom_vline(xintercept = 1915.404, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1919.192 , linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1919.731, linetype="dashed",lwd=1, col="red") +
  geom_vline(xintercept = 1920.154 , linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1922.923 , linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1923.500, linetype="dashed",lwd=1, col="red") +
  geom_vline(xintercept = 1924 , linetype="dotted", lwd=1, col="red") +
  annotate("rect",xmin=1914.577,xmax=1918.865,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=1918.712,xmax=1919.365,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1920.019,xmax=1920.404,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1921.981,xmax=1922.250,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1924.038,xmax=1924.365,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1925.038,xmax=1925.385,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  xlab("Year") +
  ylab("Birth rate per 100'000") +
  ggtitle("Time series - Birth rate") +
  theme_bw() +
  theme( axis.text = element_text(size=  size_axis),
         axis.title = element_text(size=  size_axis_title),
         title =element_text(size=title_size))


return(plot_ts_birth)

}


