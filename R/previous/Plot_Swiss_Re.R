function_plot_swiss_re <- function() {

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
         Zahlungsverkehr_Ab_W = `...133`) %>%
  dplyr::select(-`...8`,-`...15`,-`...22`,-`...29`,-`...36`,-`...43`,-`...50`,-`...57`,-`...64`, -`...71`,
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
         date = as.Date(Datum, origin = "1900-01-01")) %>%
  dplyr::select(date,Krank_ratio_W,Krank_ratio_M)

write.xlsx(swiss_re,paste0("data/swiss_re.xlsx"), rowNames=FALSE, overwrite = TRUE)

plot_swiss_re <- ggplot() +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  # annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
  # annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  # annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim49,xmax=datlim50,ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  annotate("rect",xmin=datlim51,xmax=datlim52,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim55,xmax=datlim56,ymin=-Inf,ymax=Inf,alpha=0.2,fill="#15beed") +
  geom_vline(xintercept = datlim53) +
  # geom_line(data=swiss_re,aes(y=Absenz_wo_ratio_t,x=as.POSIXct(date),colour="Absence because of other reasons"), lwd=lwd_size ) +
  # geom_line(data=swiss_re ,aes(y=Krank_ratio_t,x= as.POSIXct(date),colour="Absence because of illness"), lwd=lwd_size)  +
  
  geom_line(data=swiss_re ,aes(y=Krank_ratio_W,x= as.POSIXct(date),colour="Female"), lwd=lwd_size)  +
  geom_line(data=swiss_re ,aes(y=Krank_ratio_M,x= as.POSIXct(date),colour="Male"), lwd=lwd_size)  +

  scale_x_datetime( breaks = date_breaks("12 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims3), max(lims4)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[8],col_pal[2]))+
  xlab("Year")+
  ylab("Percentages")+
  ggtitle("Swiss Re") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.9, .8),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_text(size=10,angle=45,hjust=1),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

# 
# Figure_Illness_w <- ggplot() +
#   annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim49,xmax=datlim50,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim51,xmax=datlim52,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # geom_line(data=swiss_re,aes(y=Absenz_wo_ratio_W,x=as.POSIXct(date),colour="Absence because of other reasons"), lwd=lwd_size ) +
#   geom_line(data=swiss_re ,aes(y=Krank_ratio_W,x= as.POSIXct(date),colour="Female"), lwd=lwd_size)  +
#   geom_line(data=swiss_re ,aes(y=Krank_ratio_M,x= as.POSIXct(date),colour="Male"), lwd=lwd_size)  +
# 
#   scale_x_datetime( breaks = date_breaks("12 month"), 
#                     labels = label_date_short(),
#                     limits =c(min(lims3), max(lims4)),
#                     expand = c(0,0)) +
#   scale_color_manual(name = "",
#                      values = c(col_pal[8],col_pal[2]))+
#   xlab("Year")+
#   ylab("Percentages")+
#   ggtitle("Swiss Re Women") +
#   theme_bw()+
#   #theme_light(base_size = 16)+
#   theme(axis.text.y = element_text(size=text_size),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = c(0.9, .8),
#         legend.text=element_text(size=legend_size),
#         # legend.key.size = unit(1.5, 'cm'),
#         # legend.spacing.x = unit(1.5, 'cm'),
#         axis.text.x = element_blank(),
#         axis.title.x  = element_blank(),
#         axis.title.y  = element_text(size=axis_legend_size),
#         title =element_text(size=title_size))
# 
# 
# Figure_Illness_m <- ggplot() +
#   annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim17,xmax=datlim18,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim19,xmax=datlim20,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim21,xmax=datlim22,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim23,xmax=datlim24,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim25,xmax=datlim26,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim27,xmax=datlim28,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim29,xmax=datlim30,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim31,xmax=datlim32,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim33,xmax=datlim34,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim35,xmax=datlim36,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim37,xmax=datlim38,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim39,xmax=datlim40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim41,xmax=datlim42,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim43,xmax=datlim44,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   # annotate("rect",xmin=datlim45,xmax=datlim46,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   # annotate("rect",xmin=datlim47,xmax=datlim48,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   annotate("rect",xmin=datlim49,xmax=datlim50,ymin=-Inf,ymax=Inf,alpha=0.2,fill="orange") +
#   annotate("rect",xmin=datlim51,xmax=datlim52,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
#   geom_line(data=swiss_re,aes(y=Absenz_wo_ratio_M,x=as.POSIXct(date),colour="Absence because of other reasons"), lwd=lwd_size ) +
#   geom_line(data=swiss_re ,aes(y=Krank_ratio_M,x= as.POSIXct(date),colour="Absence because of illness"), lwd=lwd_size)  +
# 
#   scale_x_datetime( breaks = date_breaks("12 month"), 
#                     labels = label_date_short(),
#                     limits =c(min(lims3), max(lims4)),
#                     expand = c(0,0)) +
#   scale_color_manual(name = "",
#                      values = c(col_pal[8],col_pal[2]))+
#   xlab("Year")+
#   ylab("Percentages")+
#   ggtitle("Swiss Re Men") +
#   theme_bw()+
#   #theme_light(base_size = 16)+
#   theme(axis.text.y = element_text(size=text_size),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.position = c(0.9, .8),
#         legend.text=element_text(size=legend_size),
#         axis.text.x = element_text(size=10,angle=45,hjust=1),
#         # legend.key.size = unit(1.5, 'cm'),
#         # legend.spacing.x = unit(1.5, 'cm'),
#         # axis.text.x = element_blank(),
#         # axis.title.x  = element_blank(),
#         axis.title.y  = element_text(size=axis_legend_size),
#         title =element_text(size=title_size))
# 
# plot_swiss_re <- cowplot::plot_grid(Figure_Illness_t ,Figure_Illness_w,Figure_Illness_m, 
#                                   ncol=1, nrow=3, align="hv",
#                                   rel_heights = c(1,1,1))


return(plot_swiss_re)

}