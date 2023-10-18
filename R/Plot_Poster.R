

  load("data/dataZH.RData")

  load("data/expected_death_inla_weekly1918.RData")
  excess1918 <-   expected_deaths
  load("data/expected_death_inla_weekly1920.RData")
  excess1920 <-   expected_deaths


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

CityZH_pop <- dataZH %>%
  select(Year, iso_cw,pop.weekly )

data_deaths_inf  <-  read_excel("data_raw/Delay_Meldungen.xlsx", sheet="Totesfaelle") %>%
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
  

data_delay_faelle <-  read_excel("data_raw/Delay_Meldungen.xlsx", sheet="Faelle") %>%
  expand.dft(., freq="Number")  %>%
  mutate(Number= 1,
         Meldung = ymd(Meldung),
         Erkrankung = ymd(Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         weeks_delay = days_delay/7,
         Year = year(Meldung),
         iso_erk = isoweek(Erkrankung),
         iso_erk = ifelse( iso_erk == 1, 53,  iso_erk),
         iso_meld = isoweek(Meldung),
         iso_meld = ifelse( iso_meld == 1, 53,  iso_meld),
         Year_KW_e = paste0(Year,"/",iso_erk),
         Year_KW_m = paste0(Year,"/",iso_meld)) %>%
  filter(Year==1918) %>%
  mutate(Var="Cases")

data_delay_deaths  <-  read_excel("data_raw/Delay_Meldungen.xlsx", sheet="Totesfaelle") %>%
  expand.dft(., freq="Number")  %>%
  mutate(Number= 1,
         Meldung = ymd(Meldung),
         Erkrankung = ymd(Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         days_delay = as.numeric(Meldung-Erkrankung),
         weeks_delay = days_delay/7,
         Year = year(Meldung),
         iso_erk = isoweek(Erkrankung),
         iso_erk = ifelse( iso_erk == 1, 53,  iso_erk),
         iso_meld = isoweek(Meldung),
         iso_meld = ifelse( iso_meld == 1, 53,  iso_meld),
         Year_KW_e = paste0(Year,"/",iso_erk),
         Year_KW_m = paste0(Year,"/",iso_meld)) %>%
  filter(Year==1918) %>%
  mutate(Var="Deaths")

data_delay <- data_delay_faelle %>%
  rbind(data_delay_deaths)

swiss_re <-  read_excel("data_raw/Master_SwissRE.xlsx") %>%
  rename(Datum =`...1`,
         Kassa_Wert_Ar_M = `Kassa & Wertschriften`,
         Kassa_Wert_Ar_W = `...3`,
         Kassa_Wert_K_M = `...4`,
         Kassa_Wert_K_W = `...5`,
         Kassa_Wert_Ab_M = `...6`,
         Kassa_Wert_Ab_W = `...7`,
         
         Kassa_Ar_M = `Kassa`,
         Kassa_Ar_W = `...10`,
         Kassa_K_M = `...11`,
         Kassa_K_W = `...12`,
         Kassa_Ab_M = `...13`,
         Kassa_Ab_W = `...14`,
         
         Wertschriften_Ar_M = `Wertschriften`,
         Wertschriften_Ar_W = `...17`,
         Wertschriften_K_M = `...18`,
         Wertschriften_K_W = `...19`,
         Wertschriften_Ab_M = `...20`,
         Wertschriften_Ab_W = `...21`,
         
         Cont_Ar_M = `Feuer/ Cont.`,
         Cont_Ar_W = `...24`,
         Cont_K_M = `...25`,
         Cont_K_W = `...26`,
         Cont_Ab_M = `...27`,
         Cont_Ab_W = `...28`,
         Cont_Trans_Ar_M = `Feuer (Cont. u. Trans.)`,
         Cont_Trans_Ar_W = `...31`,
         Cont_Trans_K_M = `...32`,
         Cont_Trans_K_W = `...33`,
         Cont_Trans_Ab_M = `...34`,
         Cont_Trans_Ab_W = `...35`,
         
         Trans_Ar_M = `Feuer/ Trans.`,
         Trans_Ar_W = `...38`,
         Trans_K_M = `...39`,
         Trans_K_W = `...40`,
         Trans_Ab_M = `...41`,
         Trans_Ab_W = `...42`,
         
         Buchhaltung_Ar_M = `Buchhaltung`,
         Buchhaltung_Ar_W = `...45`,
         Buchhaltung_K_M = `...46`,
         Buchhaltung_K_W = `...47`,
         Buchhaltung_Ab_M = `...48`,
         Buchhaltung_Ab_W = `...49`,
         
         Transport_Ar_M = `Transport`,
         Transport_Ar_W = `...52`,
         Transport_K_M = `...53`,
         Transport_K_W = `...54`,
         Transport_Ab_M = `...55`,
         Transport_Ab_W = `...56`,
         
         Krieg_Ar_M = `Kriegs-Pool`,
         Krieg_Ar_W = `...59`,
         Krieg_K_M = `...60`,
         Krieg_K_W = `...61`,
         Krieg_Ab_M = `...62`,
         Krieg_Ab_W = `...63`,
         
         Sekretariat_Ar_M = `Sekretariat`,
         Sekretariat_Ar_W = `...66`,
         Sekretariat_K_M = `...67`,
         Sekretariat_K_W = `...68`,
         Sekretariat_Ab_M = `...69`,
         Sekretariat_Ab_W = `...70`,
         
         Unfall_Ar_M = `Unfall`,
         Unfall_Ar_W = `...73`,
         Unfall_K_M = `...74`,
         Unfall_K_W = `...75`,
         Unfall_Ab_M = `...76`,
         Unfall_Ab_W = `...77`,
         
         Leben_Ar_M = `Leben`,
         Leben_Ar_W = `...80`,
         Leben_K_M = `...81`,
         Leben_K_W = `...82`,
         Leben_Ab_M = `...83`,
         Leben_Ab_W = `...84`,
         
         Leben_Kauf_Ar_M = `Leben Kaufm.`,
         Leben_Kauf_Ar_W = `...87`,
         Leben_Kauf_K_M = `...88`,
         Leben_Kauf_K_W = `...89`,
         Leben_Kauf_Ab_M = `...90`,
         Leben_Kauf_Ab_W = `...91`,
         
         Leben_Math_Ar_M = `Leben Math.`,
         Leben_Math_Ar_W = `...94`,
         Leben_Math_K_M = `...95`,
         Leben_Math_K_W = `...96`,
         Leben_Math_Ab_M = `...97`,
         Leben_Math_Ab_W = `...98`,
         
         Revisionsbureau_Ar_M = `Revisionsbureau`,
         Revisionsbureau_Ar_W = `...101`,
         Revisionsbureau_K_M = `...102`,
         Revisionsbureau_K_W = `...103`,
         Revisionsbureau_Ab_M = `...104`,
         Revisionsbureau_Ab_W = `...105`,
         
         Unterabteilungen_Ar_M = `Unterabteilungen`,
         Unterabteilungen_Ar_W = `...108`,
         Unterabteilungen_K_M = `...109`,
         Unterabteilungen_K_W = `...110`,
         Unterabteilungen_Ab_M = `...111`,
         Unterabteilungen_Ab_W = `...112`,
         
         Einruch_Ar_M = `Einbruch  & Nebenbranchen`,
         Einruch_Ar_W = `...115`,
         Einruch_K_M = `...116`,
         Einruch_K_W = `...117`,
         Einruch_Ab_M = `...118`,
         Einruch_Ab_W = `...119`,
         
         Konzernbureau_Ar_M = `Konzernbureau`,
         Konzernbureau_Ar_W = `...122`,
         Konzernbureau_K_M = `...123`,
         Konzernbureau_K_W = `...124`,
         Konzernbureau_Ab_M = `...125`,
         Konzernbureau_Ab_W = `...126`,
         
         Zahlungsverkehr_Ar_M = `Zahlungsverkehr`,
         Zahlungsverkehr_Ar_W = `...129`,
         Zahlungsverkehr_K_M = `...130`,
         Zahlungsverkehr_K_W = `...131`,
         Zahlungsverkehr_Ab_M = `...132`,
         Zahlungsverkehr_Ab_W = `...133`,
         
  ) %>%
  select(-`...8`,-`...15`,-`...22`,-`...29`,-`...36`,-`...43`,-`...50`,-`...57`,-`...64`, -`...71`,
         -`...78`,  -`...85`,  -`...92`,  -`...99`, -`...106`, -`...113`, -`...120`, -`...127`) %>%
  slice(-1) %>%
  mutate_if(is.character, as.numeric) %>%
  replace(is.na(.), 0) %>%
  mutate(Arbeit_M = Kassa_Wert_Ar_M + Kassa_Ar_M + Wertschriften_Ar_M + Cont_Ar_M + Cont_Trans_Ar_M +
           Trans_Ar_M + Buchhaltung_Ar_M + Transport_Ar_M + Sekretariat_Ar_M+Krieg_Ar_M+ Unfall_Ar_M + Leben_Ar_M +
           Leben_Kauf_Ar_M + Leben_Math_Ar_M + Revisionsbureau_Ar_M +Unterabteilungen_Ar_M+Einruch_Ar_M + 
           Konzernbureau_Ar_M+ Zahlungsverkehr_Ar_M,
         Arbeit_W = Kassa_Wert_Ar_W + Kassa_Ar_W + Wertschriften_Ar_W + Cont_Ar_W + Cont_Trans_Ar_W +
           Trans_Ar_W + Buchhaltung_Ar_W + Transport_Ar_W + Sekretariat_Ar_W+ Krieg_Ar_W+Unfall_Ar_W + Leben_Ar_W+
           Leben_Kauf_Ar_W + Leben_Math_Ar_W +  Revisionsbureau_Ar_W +Unterabteilungen_Ar_W+Einruch_Ar_W + 
           Konzernbureau_Ar_W+ Zahlungsverkehr_Ar_W ,
         Arbeit_t = Arbeit_M + Arbeit_W,
         Krankheit_M = Kassa_Wert_K_M + Kassa_K_M + Wertschriften_K_M + Cont_K_M + Cont_K_M +
           Trans_K_M + Buchhaltung_K_M + Transport_K_M + Sekretariat_K_M + Krieg_K_M + Unfall_K_M + Leben_K_M +
           Leben_Kauf_K_M  +  Leben_Math_K_M + Revisionsbureau_K_M +Unterabteilungen_K_M+Einruch_K_M + 
           Konzernbureau_K_M+ Zahlungsverkehr_K_M,
         Krankheit_W = Kassa_Wert_K_W + Kassa_K_W + Wertschriften_K_W + Cont_K_W + Cont_K_W +
           Trans_K_W + Buchhaltung_K_W + Transport_K_W + Sekretariat_K_W+ Krieg_K_W +Unfall_K_W + Leben_K_W+
           Leben_Kauf_K_W  +  Leben_Math_K_W + Revisionsbureau_K_W +Unterabteilungen_K_W+Einruch_K_W + 
           Konzernbureau_K_W+ Zahlungsverkehr_K_W,
         Krankheit_t = Krankheit_M + Krankheit_W,
         Absenz_M = Kassa_Wert_Ab_M + Kassa_Ab_M + Wertschriften_Ab_M + Cont_Ab_M + Cont_Ab_M +
           Trans_Ab_M + Buchhaltung_Ab_M + Transport_Ab_M + Sekretariat_Ab_M +Krieg_Ab_M + Unfall_Ab_M + Leben_Ab_M +
           Leben_Kauf_Ab_M + Leben_Math_Ab_M +  Revisionsbureau_Ab_M +Unterabteilungen_Ab_M+Einruch_Ab_M + 
           Konzernbureau_Ab_M+ Zahlungsverkehr_Ab_M,
         Absenz_W = Kassa_Wert_Ab_W + Kassa_Ab_W + Wertschriften_Ab_W + Cont_Ab_W + Cont_Ab_W +
           Trans_Ab_W + Buchhaltung_Ab_W + Transport_Ab_W + Sekretariat_Ab_W + Krieg_Ab_W + Unfall_Ab_W + Leben_Ab_W +
           Leben_Kauf_Ab_W + Leben_Math_Ab_W +  Revisionsbureau_Ab_M+Unterabteilungen_Ab_W+Einruch_Ab_W + 
           Konzernbureau_Ab_W+ Zahlungsverkehr_Ab_W,
         Absenz_t = Absenz_M + Absenz_W,
         Absenz_wo_Kr_M = Absenz_M - Krankheit_M,
         Absenz_wo_Kr_W = Absenz_W - Krankheit_W,
         Absenz_wo_Kr_t = Absenz_t - Krankheit_t,
         Krank_ratio_M =  (Krankheit_M/Arbeit_M)*100,
         Krank_ratio_W =  (Krankheit_W/Arbeit_W)*100,
         Krank_ratio_t =  (Krankheit_t/Arbeit_t)*100,
         Absenz_ratio_M =  (Absenz_M /Arbeit_M)*100,
         Absenz_ratio_W =  (Absenz_W /Arbeit_W)*100,
         Absenz_ratio_t =  (Absenz_t/Arbeit_t)*100,
         Absenz_wo_ratio_M =  (Absenz_wo_Kr_M /Arbeit_M)*100,
         Absenz_wo_ratio_W =  (Absenz_wo_Kr_W /Arbeit_W)*100,
         Absenz_wo_ratio_t =  (Absenz_wo_Kr_t/Arbeit_t)*100,
         date = as.Date(Datum, origin = "1900-01-01"))



Figure_inc <- ggplot() +

  geom_line(data=dataZH ,aes(y=infl_inc ,x= Reporting,col="City of Zurich"), lwd=lwd_size)+
  geom_line(data=dataZH,aes(y=infl_inc_canton,x=Reporting,col="Canton Zurich"), lwd=lwd_size ) +
  
  # scale_x_datetime( breaks = date_breaks("2 month"),
  #                   labels = label_date_short(),
  #                   limits =c(min(lims5), max(lims6)))+
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("1 month"),limits =c(min(lims5), max(lims6))) +
  geom_vline(data=table_legend, aes(xintercept = date,colour = col_value), linetype = "dashed", lwd=lwd_size_vline ) + 
  # geom_vline(data=table_legend, aes(xintercept = as.Date("02.12.1918", "%d.%m.%Y")),col="green", linetype = "dashed", lwd=1) + 
  geom_label(data=table_legend, aes(y=Y_value, x=date,  label= key,colour = col_value), size=3,show.legend = FALSE) +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # geom_textbox(data=text_plot,aes(x=x, y=y, label=label), width = grid::unit(0.45, "npc"),
  #              height = grid::unit(0.40, "npc"), size=4) +
  scale_color_manual(name = "",
                     label =c("City of Zurich","Canton Zurich"),
                     values = c(col_pal[1],  col_pal[4]))+
  xlab("Month/Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("A) Incidence of reported flu cases") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = c(.62, .82),
    legend.text=element_text(size=legend_size),
    # legend.key.size = unit(1.5, 'cm'),
    # legend.spacing.x = unit(1.5, 'cm'),
    axis.text.x = element_blank(),
    axis.title.x  =element_blank(),
    axis.title.y  = element_text(size=axis_legend_size),
    title =element_text(size=title_size))

  
# Figure_curve <- ggplot() +
#   geom_line(data=data_deaths_inf ,aes(y=death_inf_inc,x= Reporting,col="City of Zurich"), lwd=lwd_size )+
#   # geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed",lwd=lwd_size_vline) + 
#   scale_x_date( date_labels ='%W / %y', date_breaks="2 weeks",limits =c(min(lims5), max(lims6))) +
#   scale_color_manual(name = "",
#                      values =col_pal[1])+
#   xlab("Calendar week/Year")+
#   ylab("per 10'000 inhab.")+
#   ggtitle("Flu death") +
#   theme_bw()+
#   #theme_light(base_size = 16)+
#   #theme_light(base_size = 16)+
#   theme(axis.text.y = element_text(size=text_size),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = c(.1, .85),
#         legend.text=element_text(size=legend_size),
#         # legend.key.size = unit(1.5, 'cm'),
#         # legend.spacing.x = unit(1.5, 'cm'),
#         axis.text.x = element_text(size=size_axis_x,angle=45,hjust=1),
#         axis.title.x  = element_text(size=axis_legend_size),
#         axis.title.y  = element_text(size=axis_legend_size),
#         title =element_text(size=title_size))

Figure_mort <- ggplot() +
  geom_line(data=dataZH ,aes(y=death_inc,x= Reporting,col="City of Zurich"), lwd=lwd_size )+
  # geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed",lwd=lwd_size_vline) + 
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("1 month"),limits =c(min(lims5), max(lims6))) +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  scale_color_manual(name = "",
                   values =col_pal[1])+
  xlab("Month/Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("C) All cause mortality") +
  theme_bw()+
  #theme_light(base_size = 16)+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.62, .82),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  =element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

  
Figure_excess <- ggplot() +
 geom_col(data= dataZH,aes(x= Reporting,y = rel_excess_death/100, fill= Difference_sig)) +
  # geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed",lwd=lwd_size_vline) + 
  # geom_ribbon(data=dataZH,aes(ymin=LL_inc, ymax=UL_inc, x=as.POSIXct(Reporting),fill="CI_area"), alpha=0.2) +
  # geom_line(data=dataZH ,aes(y=death_inc,x=as.POSIXct(Reporting),colour="death"), lwd=lwd_size ) +
  # geom_line(data=dataZH ,aes(y=fit_inc,x=as.POSIXct(Reporting),colour="fit"), lwd=lwd_size ) +
  # geom_line(data=dataZH ,aes(y=mean_minimum,x=as.POSIXct(Reporting),colour="Minimum"), lwd=lwd_size ) +
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("1 month"),limits =c(min(lims5), max(lims6))) +
  scale_y_continuous(labels = scales::percent)+
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  scale_fill_manual("",
                   breaks=c("Fewer than expected","More than expected","Significant more"),
                    values =c("#a6d96a",col_pal[2],"#ca0020")) +
  xlab("Month/Year")+
  ylab("Relatitve excess mortality in %")+
  ggtitle("D) Relative excess mortality - City of Zurich (all cause mortality)") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.62, .82),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  =element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

Figure_hosp <- ggplot() +
  geom_line(data=dataZH,aes(y=AndereInc,x=Reporting,colour="Canton Zurich"), lwd=lwd_size ) +
  # geom_line(data=dataZH,aes(y= HospInc,x=as.POSIXct(Reporting),colour="Total"), lwd=lwd_size ) +
  # geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed",lwd=lwd_size_vline) + 
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("1 month"),limits =c(min(lims5), max(lims6))) +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  scale_color_manual(name = "",
                     values = c(col_pal[4]))+
  xlab("Month/Year")+
  ylab("per 10'000 inhab.")+
  ggtitle("B) Hospitalisations - infections incl. flu") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.62, .82),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  =element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

plot_swiss_re <- ggplot() +
  geom_line(data=swiss_re ,aes(y=Krank_ratio_W/100,x= date,colour="Female"), lwd=lwd_size)  +
  geom_line(data=swiss_re ,aes(y=Krank_ratio_M/100,x= date,colour="Male"), lwd=lwd_size)  +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  # geom_vline(data=table_legend, aes(xintercept = date), linetype = "dashed") + 
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("1 month"),limits =c(min(lims5), max(lims6))) +
  
  scale_color_manual(name = "",
                     values = c(col_pal[8],col_pal[2]))+
  scale_y_continuous(labels = scales::percent)+
  xlab("Month/Year")+
  ylab("Percentages")+
  ggtitle("E) Swiss Re - Absence from work due to illness in relation to total working days") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.62, .82),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  =element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

plot_delays_dis <- ggplot(data=data_delay, aes(x=as.Date(Meldung,format = "%Y-%m-%d"),y=weeks_delay, col=Var)) +
  # geom_point(position=pd, size=3) +
  # geom_dotplot(binaxis = "y",stackdir = "center", aes(col=Kurs, fill=Kurs), dotsize=0.5)+
  stat_summary(fun.data = "mean_cl_boot", conf.int = .95,geom ="errorbar", width = 2,lwd=1,position=pd) +
  stat_summary(fun.y=mean, geom="point",position=pd, size=4) +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.1,fill="black") +
  scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("1 month"),limits =c(min(lims5), max(lims6))) +
  
  scale_color_manual("",values=c("#a6d96a", col_pal[8])) +
  xlab("Month/Year")+
  ylab("Mean and 95% CI reporting delay in weeks") +
  ggtitle("F) Reporting delays of reported flu cases and deaths") +
  theme_bw()+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(.62, .82),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_text(size=size_axis_x,angle=45,hjust=1),
        axis.title.x  = element_text(size=axis_legend_size),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

plot_together <- cowplot::plot_grid(Figure_inc,NULL,Figure_hosp,NULL,Figure_mort,NULL, Figure_excess,NULL,plot_swiss_re,NULL,
                                    plot_delays_dis,
                                  ncol=1, nrow=11, align="hv",
                                  rel_heights = c(1,-0.1,1,-0.1,1,-0.1,1,-0.1,1,-0.1,1))


cowplot::save_plot(paste0("output/plot_together.pdf"), plot_together,base_height=36,base_width=15)





