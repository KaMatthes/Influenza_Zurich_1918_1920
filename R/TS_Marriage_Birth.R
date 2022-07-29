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


data_zh_birth <- read_excel("../data_raw/Daten_Bulletins.xlsx", sheet=1) %>%
  select(Wochenende, Lebendgeburten_gr,Totgeburten_gr) %>%
  mutate(Wochenende_compare = ymd(Wochenende)) %>%
  right_join(data_compare ) %>%
  left_join(data_zh_ehe) %>%
  mutate(KW = isoweek(Wochenende_compare),
         Eheschliessungen = as.numeric(Eheschliessungen),
         Geburten_sum = Lebendgeburten_gr+Totgeburten_gr,
         Year = as.numeric(format(Wochenende_compare,'%Y')),
         Marriage_Birth_Rate = round(Geburten_sum/Eheschliessungen,2),
         Marriage_Birth_Rate = ifelse(Marriage_Birth_Rate >5,2, Marriage_Birth_Rate),
         Month = as.numeric(format(Wochenende_compare,'%m'))) %>%
  arrange(Wochenende_compare) %>%
  filter(!is.na(Geburten_sum)) 
  # group_by(Year, Month) %>%
  # mutate(sum_birth = sum(Geburten_sum),
  #        sum_ehe = sum(Eheschliessungen)) %>%
  # ungroup() %>%
  # distinct(Year, Month, .keep_all = TRUE) %>%
  # mutate(Marriage_Birth_Rate =round(sum_birth/sum_ehe,2))


data_marriage_birth_ts <-data_zh_birth  %>%
  filter(!KW==53) %>%
  filter(!Year==1910) %>%
  dplyr::select(Marriage_Birth_Rate)%>%
  ts(frequency = 52, start =1911)

bp_marriage_birth_ts <- breakpoints(data_marriage_birth_ts  ~ 1)

plot_ts_marriage_birth <- data_marriage_birth_ts %>%
  decompose(type = "additive") %>%
  autoplot(range.bars = FALSE) +
  geom_vline(xintercept = 1916.885, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1917.423, linetype="dashed", lwd=1, col="red") +
  geom_vline(xintercept = 1918.019, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept = 1919.712, linetype="dotted", lwd=1, col="red") +
  geom_vline(xintercept =1919.981, linetype="dashed", lwd=1, col="red") +
  geom_vline(xintercept = 1920.635, linetype="dotted", lwd=1, col="red") +
  annotate("rect",xmin=1914.577,xmax=1918.865,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=1918.712,xmax=1919.365,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1920.019,xmax=1920.404,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1921.981,xmax=1922.250,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1924.038,xmax=1924.365,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=1925.038,xmax=1925.385,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  xlab("Year") +
  ylab("Birth per marriage") +
  ggtitle("Time series - Birth per marriage") +
  theme_bw() +
  theme( axis.text = element_text(size=  size_axis),
         axis.title = element_text(size=  size_axis_title),
         title =element_text(size=title_size))


return(plot_ts_marriage_birth)
}

# cowplot::save_plot("output/plot_zurich.pdf", plot_zurich ,base_height=15,base_width=20)
