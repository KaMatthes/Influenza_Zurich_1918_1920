function_year_check <- function() {
  
load("../data/dataZH_month.RData")
  
  
  dataZH_year <- dataZH_month %>%
    mutate(
      death_m = ifelse(is.na(death_m), 0,death_m),
      influenza_m = ifelse(is.na(influenza_m), 0,influenza_m))%>%
    group_by(Year,CityZurich) %>%
    summarise(death_year = sum(death_m, na.rm=TRUE),
              Inf_year = sum(influenza_m, na.rm=TRUE)) %>%
    ungroup() 
  
  
  dat.excess <- dataZH_month  %>%
  select(Year, Month, death_m,CityZurich, pop.monthly ) %>%
  rename(death=death_m) %>%
  mutate(death = as.integer(round(death,0))) %>%
  # mutate(Year = as.factor(Year),
  #        Month = as.factor(Month)) %>%
  filter(!Year==1909) %>%
  filter(!Year==1969) %>%
  
  # mutate(YearID=Year) %>%
    filter(Year >=1912 & Year <=1918 )


year_smooth <- 5
year_from <- min(dat.excess$Year)
year_reg <- year_from + year_smooth


control.family <- inla.set.control.family.default()

hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

  # formula <- death ~ 1 + offset(log(CityZurich))  +  as.factor(Month) +
  #   f(YearID, model='iid',hyper=hyper.iid) +
  #   # f(MonthID, model='iid',hyper=hyper.iid) +
  #   f(timeID, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid)
  # # f(timeID, model='seasonal',season.length=12)
  
  # 
  # formula <- death ~ 1 + offset(log(CityZurich))  +
  #   f(YearID, model='iid',hyper=hyper.iid) 
  # f(timeID, model='seasonal',season.length=12)
  
  
  formula <- death ~ 1 + offset(log(pop.monthly)) +
    f(YearID,model='iid',hyper=hyper.iid) +
    f(seasID, model='seasonal', season.length =12) +
    f(timeID, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid)
    # f(YearID2, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid) 
    # f(YearID2,model='rw1',hyper=hyper.iid, scale.model=T)

    reg_data <-  dat.excess %>%
      filter(Year >= 1918+1 - year_smooth & Year < 1918+1)%>%
      mutate(death=ifelse (Year ==1918, NA, death))  %>%
      arrange(Year, Month) %>%
      group_by(Year, Month) %>%
      mutate(seasID = cur_group_id(),
             YearID = Year,
             YearID2 = Year,
            timeID = seasID) %>%
      ungroup() %>%
      group_by(Year) %>%
      mutate(death_year = sum(death, na.rm = TRUE)) %>%
      ungroup() 
  
    set.seed(20220421)
   
    inla.mod <- inla(formula,
                     data=reg_data,
                     # family="nbinomial",
                     family="Poisson",
                     # family = "zeroinflatednbinomial1",
                     #verbose = TRUE,
                     control.family = control.family,
                     control.compute = list(config = TRUE),
                     control.mode = list(restart = TRUE),
                      # num.threads = round(parallel::detectCores() * .2),
                     control.predictor = list(compute = TRUE, link = 1))
  
    
    # inla.mod$summary.random$t %>% 
    #   ggplot() +
    #   geom_line(aes(ID, mean)) +
    #   geom_ribbon(aes(ID, ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.3)
    
  
  
  post.samples <- inla.posterior.sample(n = 1000, result = inla.mod, seed=20220421)
  predlist <- do.call(cbind, lapply(post.samples, function(X)
    exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  rate.drawsMed<-array(unlist( predlist), dim=c(dim(reg_data)[1], 1000)); dim(rate.drawsMed) 
  dM = as.data.frame(rate.drawsMed)
  # Add to the data and save
  Data= cbind(reg_data,dM)

  
  mean.samples <- Data %>%
    select(starts_with("V"), "Year") %>%
    group_by(Year) %>% 
    summarise_all(sum) %>% 
    rowwise(Year) %>% 
    mutate(fit = median(c_across(V1:V1000)),
           LL = quantile(c_across(V1:V1000), probs= 0.025),
           UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
    select( fit, LL, UL, Year) %>%
    arrange(Year) %>%
    # left_join(dat.excess, by=c("Year")) %>%
    distinct(Year, .keep_all = TRUE) %>%
    left_join(  dataZH_year)

  
  data_cases <- mean.samples %>%
    mutate(Year = as.numeric(Year),
           inc_cases = death_year/  CityZurich*100000,
           inc_fit = fit/ CityZurich*100000,
           inc_LL = LL/ CityZurich*100000,
           inc_UL = UL/ CityZurich*100000) %>%
    ungroup()
  
  plot_check <- ggplot()+
    geom_line(data=data_cases, aes(x=Year, y=inc_cases, col="notified cases"),lwd= 1) +
    geom_line(data=data_cases, aes(x=Year, y=inc_fit, col="fitting values"), lwd=1.5) +

    geom_ribbon(data=data_cases,aes(ymin=inc_LL, ymax=inc_UL,x=Year, y=inc_fit), linetype=2, alpha=0.2) +
    # coord_cartesian(ylim=c(0, 100)) +
    xlab("Year") +
    ylab("Mortality per 100'000 inhabitants")+
    # scale_x_datetime( breaks = date_breaks("12 month"),
    #                   labels = label_date_short(),
    #                   # limits =c(min(lims3), max(lims4)),
    #                   expand = c(0,0)) +
    scale_color_manual("",
                       values=c( "red","black"))+
    theme_bw() +
    #theme_light(base_size = 16)+
    theme(axis.text.y = element_text(size=text_size),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = c(.2, .8),
          legend.text=element_text(size=legend_size),
          # legend.key.size = unit(1.5, 'cm'),
          # legend.spacing.x = unit(1.5, 'cm'),
          axis.text.x = element_text(size=10,angle=45,hjust=1),
          axis.title.x  = element_blank(),
          axis.title.y  = element_text(size=axis_legend_size),
          title =element_text(size=title_size))
  
  return(plot_check)
}
