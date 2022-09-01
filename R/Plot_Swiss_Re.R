function_plot_swiss_re <- function() {

swiss_re <-  read_excel("../data_raw/Daten_Swiss_Re.xlsx") %>%
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
         Sekretariat_Ar_M = `Sekretariat`,
         Sekretariat_Ar_W = `...59`,
         Sekretariat_K_M = `...60`,
         Sekretariat_K_W = `...61`,
         Sekretariat_Ab_M = `...62`,
         Sekretariat_Ab_W = `...63`,
         Unfall_Ar_M = `Unfall`,
         Unfall_Ar_W = `...66`,
         Unfall_K_M = `...67`,
         Unfall_K_W = `...68`,
         Unfall_Ab_M = `...69`,
         Unfall_Ab_W = `...70`,
         Leben_Ar_M = `Leben`,
         Leben_Ar_W = `...73`,
         Leben_K_M = `...74`,
         Leben_K_W = `...75`,
         Leben_Ab_M = `...76`,
         Leben_Ab_W = `...77`,
         Leben_Kauf_Ar_M = `Leben, Kaufm.`,
         Leben_Kauf_Ar_W = `...80`,
         Leben_Kauf_K_M = `...81`,
         Leben_Kauf_K_W = `...82`,
         Leben_Kauf_Ab_M = `...83`,
         Leben_Kauf_Ab_W = `...84`,
         Leben_Math_Ar_M = `Leben, Math`,
         Leben_Math_Ar_W = `...87`,
         Leben_Math_K_M = `...88`,
         Leben_Math_K_W = `...89`,
         Leben_Math_Ab_M = `...90`,
         Leben_Math_Ab_W = `...91`,
         Revisionsbureau_Ar_M = `Revisionsbureau`,
         Revisionsbureau_Ar_W = `...94`,
         Revisionsbureau_K_M = `...95`,
         Revisionsbureau_K_W = `...96`,
         Revisionsbureau_Ab_M = `...97`,
         Revisionsbureau_Ab_W = `...98`) %>%
  select(-`...8`,-`...15`,-`...22`,-`...29`,-`...36`,-`...43`,-`...50`,-`...57`,-`...64`, -`...71`,
         -`...78`,  -`...85`,  -`...92`) %>%
  slice(-1) %>%
  mutate_if(is.character, as.numeric) %>%
  replace(is.na(.), 0) %>%
  mutate(Arbeit_M = Kassa_Wert_Ar_M + Kassa_Ar_M + Wertschriften_Ar_M + Cont_Ar_M + Cont_Trans_Ar_M +
           Trans_Ar_M + Buchhaltung_Ar_M + Transport_Ar_M + Sekretariat_Ar_M+ Unfall_Ar_M + Leben_Ar_M +
           Leben_Kauf_Ar_M + Leben_Math_Ar_M +     Revisionsbureau_Ar_M  ,
         Arbeit_W = Kassa_Wert_Ar_W + Kassa_Ar_W + Wertschriften_Ar_W + Cont_Ar_W + Cont_Trans_Ar_W +
           Trans_Ar_W + Buchhaltung_Ar_W + Transport_Ar_W + Sekretariat_Ar_W+ Unfall_Ar_W + Leben_Ar_W+
           Leben_Kauf_Ar_W + Leben_Math_Ar_W +     Revisionsbureau_Ar_W  ,
         Arbeit_t = Arbeit_M + Arbeit_W,
         Krankheit_M = Kassa_Wert_K_M + Kassa_K_M + Wertschriften_K_M + Cont_K_M + Cont_K_M +
           Trans_K_M + Buchhaltung_K_M + Transport_K_M + Sekretariat_K_M + Unfall_K_M + Leben_K_M +
           Leben_Kauf_K_M  +  Leben_Math_K_M + Revisionsbureau_K_M,
         Krankheit_W = Kassa_Wert_K_W + Kassa_K_W + Wertschriften_K_W + Cont_K_W + Cont_K_W +
           Trans_K_W + Buchhaltung_K_W + Transport_K_W + Sekretariat_K_W+ Unfall_K_W + Leben_K_W+
           Leben_Kauf_K_W  +  Leben_Math_K_W + Revisionsbureau_K_W,
         Krankheit_t = Krankheit_M + Krankheit_W,
         Absenz_M = Kassa_Wert_Ab_M + Kassa_Ab_M + Wertschriften_Ab_M + Cont_Ab_M + Cont_Ab_M +
           Trans_Ab_M + Buchhaltung_Ab_M + Transport_Ab_M + Sekretariat_Ab_M + Unfall_Ab_M + Leben_Ab_M +
           Leben_Kauf_Ab_M + Leben_Math_Ab_M +  Revisionsbureau_Ab_M,
         Absenz_W = Kassa_Wert_Ab_W + Kassa_Ab_W + Wertschriften_Ab_W + Cont_Ab_W + Cont_Ab_W +
           Trans_Ab_W + Buchhaltung_Ab_W + Transport_Ab_W + Sekretariat_Ab_W + Unfall_Ab_W + Leben_Ab_W +
           Leben_Kauf_Ab_W + Leben_Math_Ab_W +  Revisionsbureau_Ab_M,
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
         
Figure_Illness_t <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=swiss_re ,aes(y=Krank_ratio_t,x= as.POSIXct(date),colour="Absence because of illness"), lwd=lwd_size)  +
  geom_line(data=swiss_re,aes(y=Absenz_wo_ratio_t,x=as.POSIXct(date),colour="Absence because of other reasons"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims_swiss_re1), max(lims_swiss_re2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[8],col_pal[2]))+
  xlab("Month/Year")+
  ylab("Percentages")+
  ggtitle("Total") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.2, .8),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))


Figure_Illness_w <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=swiss_re ,aes(y=Krank_ratio_W,x= as.POSIXct(date),colour="Absence because of illness"), lwd=lwd_size)  +
  geom_line(data=swiss_re,aes(y=Absenz_wo_ratio_W,x=as.POSIXct(date),colour="Absence because of other reasons"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims_swiss_re1), max(lims_swiss_re2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[8],col_pal[2]))+
  xlab("Month/Year")+
  ylab("Percentages")+
  ggtitle("Women") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.2, .8),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        axis.text.x = element_blank(),
        axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))


Figure_Illness_m <- ggplot() +
  annotate("rect",xmin=datlim15,xmax=datlim16,ymin=-Inf,ymax=Inf,alpha=0.1,fill="orange") +
  annotate("rect",xmin=datlim1,xmax=datlim2,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim3,xmax=datlim4,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim5,xmax=datlim6,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim7,xmax=datlim8,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim9,xmax=datlim10,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim11,xmax=datlim12,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  annotate("rect",xmin=datlim13,xmax=datlim14,ymin=-Inf,ymax=Inf,alpha=0.2,fill="grey40") +
  geom_line(data=swiss_re ,aes(y=Krank_ratio_M,x= as.POSIXct(date),colour="Absence because of illness"), lwd=lwd_size)  +
  geom_line(data=swiss_re,aes(y=Absenz_wo_ratio_M,x=as.POSIXct(date),colour="Absence because of other reasons"), lwd=lwd_size ) +
  scale_x_datetime( breaks = date_breaks("6 month"), 
                    labels = label_date_short(),
                    limits =c(min(lims_swiss_re1), max(lims_swiss_re2)),
                    expand = c(0,0)) +
  scale_color_manual(name = "",
                     values = c(col_pal[8],col_pal[2]))+
  xlab("Month/Year")+
  ylab("Percentages")+
  ggtitle("Men") +
  theme_bw()+
  #theme_light(base_size = 16)+
  theme(axis.text.y = element_text(size=text_size),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.2, .8),
        legend.text=element_text(size=legend_size),
        # legend.key.size = unit(1.5, 'cm'),
        # legend.spacing.x = unit(1.5, 'cm'),
        # axis.text.x = element_blank(),
        # axis.title.x  = element_blank(),
        axis.title.y  = element_text(size=axis_legend_size),
        title =element_text(size=title_size))

plot_swiss_re <- cowplot::plot_grid(Figure_Illness_t ,Figure_Illness_w,Figure_Illness_m, 
                                  ncol=1, nrow=3, align="hv",
                                  rel_heights = c(1,1,1))


return(plot_swiss_re)

}