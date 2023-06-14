function_plot_re <- function() {

load("../data/dataZH.RData")
load("../data/data_meldungen.RData")


begin <- ymd(19180615)
end <- ymd(19200605)

begin2 <- ymd(19180907)
end2 <- ymd(19190412)

before <- 21
after <- 7

gen_mean <- 3
gen_sd <- 1
rate <- gen_mean/gen_sd^2
shape <- gen_mean^2/gen_sd^2

# rate <- gen_rate
# shape <- gen_shape

data.wave <- dataZH  %>%
  rename(Date_week =  EndReportingPeriod) %>%
  mutate(Date_week = ymd(Date_week)) %>%
  filter(Date_week >= begin & Date_week <= end) %>%
  mutate(CantonCases = ifelse(is.na(CantonCases), 0, CantonCases),
         CantonCases = as.integer(round(CantonCases,0)),
         CantonCases_roll =rollmean(CantonCases,3, na.pad=TRUE, align="right"),
         CantonCases_roll = ifelse(is.na(CantonCases_roll),0, CantonCases_roll),
         CantonCases_roll = round(CantonCases_roll,0),
         
         CityCases = ifelse(is.na( CityCases), 0,  CityCases),
         CityCases  = ifelse(Date_week==ymd(19190614),10,CityCases ),
         CityCases  = ifelse(Date_week==ymd(19190531),6,CityCases ),
         CityCases  = ifelse(Date_week==ymd(19190607),4,CityCases ),
         CityCases = as.integer(round( CityCases,0)),
         CityCases_roll =rollmean(CityCases,3, na.pad=TRUE, align="right"),
         CityCases_roll = ifelse(is.na(CityCases_roll),0, CityCases_roll),
         CityCases_roll = round( CityCases_roll,0),
         
         CityDeathsTotal = ifelse(is.na(CityDeathsTotal), 0,  CityDeathsTotal),
         CityDeathsTotal = as.integer(round(CityDeathsTotal,0)),
         CityDeathsTotal_roll =rollmean(CityDeathsTotal,3, na.pad=TRUE, align="right"),
         CityDeathsTotal_roll = ifelse(is.na(CityDeathsTotal_roll),0, CityDeathsTotal_roll),
         CityDeathsTotal_roll = round(CityDeathsTotal_roll,0),
         
         Andere_Infekt = ifelse(is.na(Andere_Infekt), 0,Andere_Infekt),
         Andere_Infekt = as.integer(round(Andere_Infekt,0)),
         Andere_Infekt_roll =rollmean(Andere_Infekt,3, na.pad=TRUE, align="right"),
         Andere_Infekt_roll = ifelse(is.na(Andere_Infekt_roll),0, Andere_Infekt_roll),
         Andere_Infekt_roll = round(Andere_Infekt_roll,0))



data.reports <- data_meldungen %>%
  filter(Erhebung =="Notifications") %>%
  rename(Date_week = Datum) %>%
  filter(Date_week >= begin2 & Date_week <= end2) %>%
  add_row(Date_week= ymd(19180831), Faelle=0, Totesfaelle=0, iso_cw=35, Year=1918, Month=9,Cw_year="1918/W35", Erhebung="Notification") %>%
  add_row(Date_week= ymd(19180824), Faelle=0, Totesfaelle=0, iso_cw=34, Year=1918, Month=9,Cw_year="1918/W34", Erhebung="Notification") %>%
  arrange(Date_week) %>%
  mutate(Date_week = ymd(Date_week),
         Faelle  = ifelse(is.na(Faelle), 0, Faelle ),
         Faelle  = as.integer(round(Faelle ,0)),
         Faelle_roll =rollmean(Faelle ,3, na.pad=TRUE, align="right"),
         Faelle_roll = ifelse(is.na(Faelle_roll),0, Faelle_roll),
         Faelle_roll = round(Faelle_roll,0),
         
         Totesfaelle  = ifelse(is.na(Totesfaelle), 0, Totesfaelle),
         Totesfaelle  = as.integer(round(Totesfaelle ,0)),
         Totesfaelle_roll =rollmean(Totesfaelle,3, na.pad=TRUE, align="right"),
         Totesfaelle_roll = ifelse(is.na(Totesfaelle_roll),0, Totesfaelle_roll),
         Totesfaelle_roll = round(Totesfaelle_roll,0)) 
  

Re_canton <- data.frame(Date_week = seq(begin, end, 7),
                        Re = NA,
                        Re_lower = NA,
                        Re_upper = NA,
                        place = "Canton Zurich",
                        data = "Cases")

# Estimate trends and Re
for(i in 3:length(Re_canton$Date_week)) {
  set <- subset(data.wave, Date_week >= (Re_canton$Date_week[i] - before) & Date_week <= (Re_canton$Date_week[i] + after))
  fit <- MASS::glm.nb(CantonCases ~ Date_week, data = set)
  tryCatch(fit.ci <- suppressMessages(confint(fit)), error = function(e){})
  Re_canton[i, 2:4] <- c((1 + coef(fit)[2]/rate)^shape, (1 + fit.ci[2, 2]/rate)^shape, (1 + fit.ci[2, 1]/rate)^shape)
}


Re_zh <- data.frame(Date_week = seq(begin, end, 7),
                    Re = NA,
                    Re_lower = NA,
                    Re_upper = NA,
                    place = "City of Zurich",
                    data = "Cases")

for(i in 3:length(Re_zh$Date_week)) {
  set <- subset(data.wave, Date_week >= (Re_zh$Date_week[i] - before) & Date_week <= (Re_zh$Date_week[i] + after))
  fit <- MASS::glm.nb(CityCases ~ Date_week, data = set)
  tryCatch(fit.ci <- suppressMessages(confint(fit)), error = function(e){})
  Re_zh[i, 2:4] <- c((1 + coef(fit)[2]/rate)^shape, (1 + fit.ci[2, 2]/rate)^shape, (1 + fit.ci[2, 1]/rate)^shape)
}



Re_death <- data.frame(Date_week = seq(begin, end, 7),
                    Re = NA,
                    Re_lower = NA,
                    Re_upper = NA,
                    place = "City of Zurich",
                    data="Total death")

for(i in 3:length(Re_death$Date_week)) {
  set <- subset(data.wave, Date_week >= (Re_death$Date_week[i] - before) & Date_week <= (Re_death$Date_week[i] + after))
  fit <- MASS::glm.nb(CityDeathsTotal ~ Date_week, data = set)
  tryCatch(fit.ci <- suppressMessages(confint(fit)), error = function(e){})
  Re_death[i, 2:4] <- c((1 + coef(fit)[2]/rate)^shape, (1 + fit.ci[2, 2]/rate)^shape, (1 + fit.ci[2, 1]/rate)^shape)
}


Re_hosp <- data.frame(Date_week = seq(begin, end, 7),
                       Re = NA,
                       Re_lower = NA,
                       Re_upper = NA,
                       place = "City of Zurich",
                       data="Hospitalisation Infection incl.Influenza")

for(i in 3:length(Re_hosp$Date_week)) {
  set <- subset(data.wave, Date_week >= (Re_hosp$Date_week[i] - before) & Date_week <= (Re_hosp$Date_week[i] + after))
  fit <- MASS::glm.nb( Andere_Infekt ~ Date_week, data = set)
  tryCatch(fit.ci <- suppressMessages(confint(fit)), error = function(e){})
  Re_hosp[i, 2:4] <- c((1 + coef(fit)[2]/rate)^shape, (1 + fit.ci[2, 2]/rate)^shape, (1 + fit.ci[2, 1]/rate)^shape)
}

Data_plot <- rbind(Re_canton, Re_zh,Re_death, Re_hosp) %>%
  left_join(data.wave) %>%
  filter(Date_week > ymd( 19180720))



Re_cases_reports1 <- data.frame(Date_week = seq(begin2, end2, 7),
                      Re = NA,
                      Re_lower = NA,
                      Re_upper = NA,
                      place = "City of Zurich",
                      data="Report cases")


for(i in 3:20) {
  set <- subset(data.reports, Date_week >= (Re_cases_reports1$Date_week[i] - before) & Date_week <= (Re_cases_reports1$Date_week[i] + after))
  fit <- MASS::glm.nb(Faelle ~ Date_week, data = set)
  tryCatch(fit.ci <- suppressMessages(confint(fit)), error = function(e){})
  Re_cases_reports1[i, 2:4] <- c((1 + coef(fit)[2]/rate)^shape, (1 + fit.ci[2, 2]/rate)^shape, (1 + fit.ci[2, 1]/rate)^shape)
}


Re_cases_reports2 <- data.frame(Date_week = seq(begin2, end2, 7),
                                Re = NA,
                                Re_lower = NA,
                                Re_upper = NA,
                                place = "City of Zurich",
                                data="Report cases")

for(i in 32) {
  set <- subset(data.reports, Date_week >= (Re_cases_reports2$Date_week[i] - before) & Date_week <= (Re_cases_reports2$Date_week[i] + after))
  fit <- MASS::glm.nb(Faelle ~ Date_week, data = set)
  tryCatch(fit.ci <- suppressMessages(confint(fit)), error = function(e){})
  Re_cases_reports2[i, 2:4] <- c((1 + coef(fit)[2]/rate)^shape, (1 + fit.ci[2, 2]/rate)^shape, (1 + fit.ci[2, 1]/rate)^shape)
}


Re_cases_reports3  <-Re_cases_reports1 %>%
  filter(is.na(Re)) %>%
  replace(is.na(.), 1) %>%
  filter(!Date_week < ymd(19190125)) %>%
  filter(!Date_week == ymd(19190412))

Re_cases_reports4  <-Re_cases_reports1 %>%
  filter(!is.na(Re))

Re_cases_reports5  <-Re_cases_reports2 %>%
  filter(!is.na(Re))

Re_cases_reports <- rbind(Re_cases_reports3, Re_cases_reports4, Re_cases_reports5) %>%
  arrange(Date_week)

Data_plot_reports <- Re_cases_reports %>%
  left_join(data.wave) %>%
  filter(Date_week > ymd( 19180720))



Re_death_reports1 <- data.frame(Date_week = seq(begin2, end2, 7),
                                Re = NA,
                                Re_lower = NA,
                                Re_upper = NA,
                                place = "City of Zurich",
                                data="Report death")


for(i in 3:20) {
  set <- subset(data.reports, Date_week >= (Re_death_reports1$Date_week[i] - before) & Date_week <= (Re_death_reports1$Date_week[i] + after))
  fit <- MASS::glm.nb(Totesfaelle ~ Date_week, data = set)
  tryCatch(fit.ci <- suppressMessages(confint(fit)), error = function(e){})
  Re_death_reports1[i, 2:4] <- c((1 + coef(fit)[2]/rate)^shape, (1 + fit.ci[2, 2]/rate)^shape, (1 + fit.ci[2, 1]/rate)^shape)
}


Re_death_reports2 <- data.frame(Date_week = seq(begin2, end2, 7),
                                Re = NA,
                                Re_lower = NA,
                                Re_upper = NA,
                                place = "City of Zurich",
                                data="Report death")

for(i in 32) {
  set <- subset(data.reports, Date_week >= (Re_death_reports2$Date_week[i] - before) & Date_week <= (Re_death_reports2$Date_week[i] + after))
  fit <- MASS::glm.nb(Totesfaelle ~ Date_week, data = set)
  tryCatch(fit.ci <- suppressMessages(confint(fit)), error = function(e){})
  Re_death_reports2[i, 2:4] <- c((1 + coef(fit)[2]/rate)^shape, (1 + fit.ci[2, 2]/rate)^shape, (1 + fit.ci[2, 1]/rate)^shape)
}


Re_death_reports3  <-Re_death_reports1 %>%
  filter(is.na(Re)) %>%
  replace(is.na(.), 1) %>%
  filter(!Date_week < ymd(19190125)) %>%
  filter(!Date_week == ymd(19190412))

Re_death_reports4  <-Re_death_reports1 %>%
  filter(!is.na(Re))

Re_death_reports5  <-Re_death_reports2 %>%
  filter(!is.na(Re))

Re_death_reports <- rbind(Re_death_reports3, Re_death_reports4, Re_death_reports5) %>%
  arrange(Date_week)

Data_plot_reports_d <- Re_death_reports %>%
  left_join(data.wave) %>%
  filter(Date_week > ymd( 19180720))



FigureRe_cases <- ggplot(data=Data_plot[Data_plot$data=="Cases",]) +
  geom_hline(yintercept=1, col="black", lwd=1.3)+
  geom_line(aes(y=Re ,x= Reporting,col=place), lwd=lwd_size)+
  geom_ribbon(aes(ymin=Re_lower, ymax=Re_upper,x=Reporting, y=Re, fill=place),linetype=2, alpha=0.3) +
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                     values = c(col_pal[4], col_pal[1])) +
  scale_fill_manual(name = "",
                     values = c(col_pal[4], col_pal[1])) +
  # coord_cartesian(ylim=c(0, 2.5))+
  xlab("Calendar week/Year")+
  ylab("Reproduction value")+
  ggtitle("Reproduction values of influenza cases") +
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
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))


FigureRe_death <- ggplot(data=Data_plot[Data_plot$data=="Total death",]) +
  geom_hline(yintercept=1, col="black", lwd=1.3)+
  geom_line(aes(y=Re ,x= Reporting,col=place), lwd=lwd_size)+
  geom_ribbon(aes(ymin=Re_lower, ymax=Re_upper,x=Reporting, y=Re, fill=place),linetype=2, alpha=0.3) +
  
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                     values = c(col_pal[1])) +
  scale_fill_manual(name = "",
                    values = c(col_pal[1])) +
  xlab("Calendar week/Year")+
  ylab("Reproduction value")+
  ggtitle("Reproduction values of total number of death") +
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
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

FigureRe_hosp <- ggplot(data=Data_plot[Data_plot$data=="Hospitalisation Infection incl.Influenza",]) +
  geom_hline(yintercept=1, col="black", lwd=1.3)+
  geom_line(aes(y=Re ,x= Reporting,col=place), lwd=lwd_size)+
  geom_ribbon(aes(ymin=Re_lower, ymax=Re_upper,x=Reporting, y=Re, fill=place),linetype=2, alpha=0.3) +
  
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                     values = c(col_pal[1])) +
  scale_fill_manual(name = "",
                    values = c(col_pal[1])) +
  xlab("Calendar week/Year")+
  ylab("Reproduction value")+
  ggtitle("Reproduction values of hospitalisations -  infection incl.Influenza") +
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
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))


FigureRe_cases_rep <- ggplot(data=Data_plot_reports) +
  geom_hline(yintercept=1, col="black", lwd=1.3)+
  geom_line(aes(y=Re ,x= Reporting,col=place), lwd=lwd_size)+
  geom_ribbon(aes(ymin=Re_lower, ymax=Re_upper,x=Reporting, y=Re, fill=place),linetype=2, alpha=0.3) +
  
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                     values = c(col_pal[1])) +
  scale_fill_manual(name = "",
                    values = c(col_pal[1])) +
  xlab("Calendar week/Year")+
  ylab("Reproduction value")+
  ggtitle("Reproduction values of reported cases") +
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
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

FigureRe_death_rep <- ggplot(data=Data_plot_reports_d) +
  geom_hline(yintercept=1, col="black", lwd=1.3)+
  geom_line(aes(y=Re ,x= Reporting,col=place), lwd=lwd_size)+
  geom_ribbon(aes(ymin=Re_lower, ymax=Re_upper,x=Reporting, y=Re, fill=place),linetype=2, alpha=0.3) +
  
  scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
  scale_color_manual(name = "",
                     values = c(col_pal[1])) +
  scale_fill_manual(name = "",
                    values = c(col_pal[1])) +
  xlab("Calendar week/Year")+
  ylab("Reproduction value")+
  ggtitle("Reproduction values of reported death") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.15, .85),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_text(size=size_axis_x,angle=45,hjust=1),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))



plot_re <- cowplot::plot_grid(FigureRe_cases, FigureRe_death,
                              FigureRe_hosp,FigureRe_cases_rep, FigureRe_death_rep,
                                  ncol=1, nrow=5, align="hv",
                                  rel_heights = c(1,1,1))


return(plot_re)
# 

}

