function_inla_total <- function(Year_Pan,Year_max, Year_min) {
  
load("data/dataZH_month.RData")

dat.excess <- dataZH_month  %>%
  select(Year, Month, death_m,CityZurich ) %>%
  rename(death=death_m) %>%
  mutate(death = as.integer(round(death,0))) %>%
  # mutate(Year = as.factor(Year),
  #        Month = as.factor(Month)) %>%
  filter(!Year==1909) %>%
  filter(!Year==1969) %>%
  arrange(Year, Month) %>%
  group_by(Year,Month) %>%
  mutate(timeID = cur_group_id()) %>%
  arrange(timeID) %>%
  ungroup() %>%
  mutate(MonthID = Month,
         YearID = Year) %>%
  filter(Year >=Year_min & Year <=Year_max )


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
  
  
  formula <- death ~ 1 + offset(log(CityZurich))  +
    f(MonthID, model='iid',hyper=hyper.iid) +
    f(timeID, model='seasonal', season.length = 12)
    # f(timeID, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid)
  # f(timeID, model='seasonal',season.length=12)

  expected_deaths <- list()
  
  for (YEAR in year_reg:Year_max){
    
    print(YEAR)
    
    if (YEAR==Year_Pan) {
    reg_data <-  dat.excess %>%
      filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
      mutate(death=ifelse (Year ==YEAR, NA, death))
    }
    
    else {
      reg_data <-  dat.excess %>% 
        filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
        mutate(death=ifelse (Year ==YEAR, NA, death)) %>% 
        filter(!Year == Year_Pan) 
    }
    
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
    select(starts_with("V"), "Month", "Year") %>%
    rowwise(Month) %>%
    mutate(fit = median(c_across(V1:V1000)),
           LL = quantile(c_across(V1:V1000), probs= 0.025),
           UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
    select(Month, fit, LL, UL, Year) %>%
    filter(Year==YEAR) %>%
    arrange(Year, Month) %>%
    left_join(dat.excess, by=c("Year", "Month")) 
  
  
  expected_deaths[[YEAR]] <-  mean.samples
  expected_deaths <- expected_deaths[-which(sapply(expected_deaths, is.null))] 
  
  }
  
  expected_deaths <- expected_deaths %>%
    bind_rows(., .id = "column_label")
  
  write.xlsx(expected_deaths,paste0("data/expected_death_inla",Year_Pan,".xlsx"), row.names=FALSE, overwrite = TRUE)
  save(expected_deaths,file=paste0("data/expected_death_inla",Year_Pan,".RData"))

  # write.xlsx(expected_deaths,paste0("data/expected_death_inla_all_years.xlsx"), row.names=FALSE, overwrite = TRUE)
  # save(expected_deaths,file=paste0("data/expected_death_inla_all_years.RData"))
  }


function_inla_total(Year_Pan=1918, Year_max=1919, Year_min=1910)
function_inla_total(Year_Pan=1920, Year_max=1928, Year_min=1915)
function_inla_total(Year_Pan=1929, Year_max=1969, Year_min=1924)


