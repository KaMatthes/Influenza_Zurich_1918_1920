function_inla_total <- function(Year_Pan,Year_Pan2,Year_max, Year_min) {
  
load("data/dataZH.RData")

dat.excess <- dataZH %>%
  filter(!is.na(Year)) %>%
  filter(!is.na(CityDeathsTotal)) %>%
  filter(!iso_cw==53) %>%
  select(Year, Month,iso_cw, CityDeathsTotal,pop.weekly ) %>%
  rename(death=CityDeathsTotal) %>%
  mutate(death = as.integer(round(death,0))) %>%
  filter(!Year==1909) %>%
  filter(!Year==1961) %>%
  filter(Year >=Year_min & Year <=Year_max )


year_smooth <- 5
year_from <- min(dat.excess$Year)
year_reg <- year_from + year_smooth


control.family <- inla.set.control.family.default()

hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

formula <- death ~ -1 + offset(log(pop.weekly))  +
    f(seasID, model='seasonal', season.length =12) +
    f(timeID, model='rw1',constr = FALSE) +
    f(factor(timeID), model = "iid",hyper= hyper.iid)

  expected_deaths <- list()
  
  for (YEAR in year_reg:Year_max){
    
    print(YEAR)
    
    if (YEAR==Year_Pan) {
      reg_data <-  dat.excess %>%
        filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
        mutate(death=ifelse (Year ==YEAR, NA, death)) %>%
        # filter(!Year==1918) %>%
        arrange(Year, Month,iso_cw) %>%
        group_by(Year,Month,iso_cw) %>%
        mutate(timeID = cur_group_id()) %>%
        arrange(timeID) %>%
        ungroup() %>%
        mutate(
               # MonthID = Month,
               seasID = Month,
               YearID = Year,
               WeekID =iso_cw) 
    }

    else {
      reg_data <-  dat.excess %>% 
        filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
        mutate(death=ifelse (Year ==YEAR, NA, death)) %>% 
        filter(!Year == Year_Pan)  %>%
        filter(!Year==1918) %>%
        arrange(Year, Month,iso_cw) %>%
        group_by(Year,Month,iso_cw) %>%
        mutate(timeID = cur_group_id()) %>%
        arrange(timeID) %>%
        ungroup() %>%
        mutate(
          MonthID = Month,
          seasID = Month,
          YearID = Year,
          WeekID =iso_cw) 
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
  
  
  post.samples <- inla.posterior.sample(n = 1000, result = inla.mod, seed=20220421)
  predlist <- do.call(cbind, lapply(post.samples, function(X)
    exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
  
  rate.drawsMed<-array(unlist( predlist), dim=c(dim(reg_data)[1], 1000)); dim(rate.drawsMed) 
  dM = as.data.frame(rate.drawsMed)
  
  # Add to the data and save
  Data= cbind(reg_data,dM)
  
  mean.samples <- Data %>%
    select(starts_with("V"), "iso_cw", "Year", "pop.weekly", "timeID") %>%
    rowwise(iso_cw) %>%
    mutate(fit = median(c_across(V1:V1000)),
           LL = quantile(c_across(V1:V1000), probs= 0.025),
           UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
    select(iso_cw, fit, LL, UL, Year, pop.weekly,timeID) %>%
    filter(Year==YEAR) %>%
    arrange(Year, iso_cw) 
    # left_join(dat.excess, by=c("Year", "iso_cw")) 
  
  
  expected_deaths[[YEAR]] <-  mean.samples
  expected_deaths <- expected_deaths[-which(sapply(expected_deaths, is.null))] 
  
  }
  
  death_data <- dat.excess %>%
    select(Year, iso_cw, death)
  
  expected_deaths <- expected_deaths %>%
    bind_rows(., .id = "column_label") %>%
    left_join(  death_data)
  
  write.xlsx(expected_deaths,paste0("data/expected_death_inla_weekly",Year_Pan,".xlsx"), rowNames=FALSE, overwrite = TRUE)
  save(expected_deaths,file=paste0("data/expected_death_inla_weekly",Year_Pan,".RData"))

  }


function_inla_total(Year_Pan=1918, Year_max=1920, Year_min=1913)




