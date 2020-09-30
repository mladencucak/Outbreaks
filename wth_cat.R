
########################################################
# Libraries
########################################################
list.of.packages <-
  c(
    "tidyverse",
    "readxl",
    "data.table",
    "knitr",
    "zoo",
    "egg",
    "ggthemes",
    "here",
    "stringr",
    "lubridate"
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

if (length(new.packages))
  install.packages(new.packages, repos = c(CRAN="https://cran.r-project.org/"))


packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s", paste(list.of.packages[packages_load != TRUE]), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}

rm(packages_load, list.of.packages, new.packages)

##############################################################################
# Risk function 
##############################################################################


##############################################################################
# Weather summaries
##############################################################################


load( file= here::here("dat", "data_for_analysis.Rdata"))

# wth <- 
#   wth %>% 
#   tidyr::unite("id", c(date),remove = FALSE )


wthls <- 
  split(wth, wth$date, drop = TRUE)


wthd <- 
  lapply(wthls, function(x){
    # x <- wthls[[180]]
    y<- data.frame( 
      date= x$date[1], 
      month= x$month[1], 
      doy= x$doy[1]
    )
    
    y$temp <- mean(x$temp, na.rm = TRUE) %>% round(1)
    y$mintemp <- min(x$temp, na.rm = TRUE) %>% round(1)
    y$maxtemp <- max(x$temp, na.rm = TRUE) %>% round(1)
    y$lqtemp <- quantile(x$temp, na.rm = TRUE)[2] %>% round(1)
    y$hqtemp <- quantile(x$temp, na.rm = TRUE)[4] %>% round(1)
    
    y$soilt <- mean(x$soilt, na.rm = TRUE) %>% round(1)
    y$minsoilt <- min(x$soilt, na.rm = TRUE) %>% round(1)
    y$maxsoilt <- max(x$soilt, na.rm = TRUE) %>% round(1)
    y$lqsoilt <- quantile(x$soilt, na.rm = TRUE)[2] %>% round(1)
    y$hqsoilt <- quantile(x$soilt, na.rm = TRUE)[4] %>% round(1)
    
    y$rh <- mean(x$rh, na.rm = TRUE) %>% round(1)
    y$minrh <- min(x$rh, na.rm = TRUE) %>% round(1)
    y$maxrh <- max(x$rh, na.rm = TRUE, warning=FALSE) %>% round(1)
    y$lqrh <- quantile(x$rh, na.rm = TRUE)[2] %>% round(1)
    y$hqrh <- quantile(x$rh, na.rm = TRUE)[4] %>% round(1)
    
    y$sol_rad <- mean(x$sol_nasa, na.rm = TRUE) %>% round(1)
    y$minsol_rad <- min(x$sol_nasa, na.rm = TRUE) %>% round(1)
    y$maxsol_rad <- max(x$sol_nasa, na.rm = TRUE, warning=FALSE) %>% round(1)
    y$lqsol_rad <- quantile(x$sol_nasa, na.rm = TRUE)[2] %>% round(1)
    y$hqsol_rad <- quantile(x$sol_nasa, na.rm = TRUE)[4] %>% round(1)

    y$rain <- mean(x$rain, na.rm = TRUE) %>% round(3)
    y$minrain <- min(x$rain, na.rm = TRUE) %>% round(3)
    y$maxrain <- max(x$rain, na.rm = TRUE, warning=FALSE) %>% round(3)
    y$lqrain <- quantile(x$rain, na.rm = TRUE)[2] %>% round(3)
    y$hqrain <- quantile(x$rain, na.rm = TRUE)[4] %>% round(3)
    y$iqrrain <- y$hqrain -y$lqrain 
    
    #Calculate the proportion of rain hours of all stations
   y$proprain <- 
    group_by(x, stna) %>% 
     summarise(rain =  sum(ifelse(rain>0.1, 1,0))/length(rain)) %>% #Proportion of rainy hours in day for each station
     ungroup() %>% 
      summarise(
        proprain = mean(rain)) %>% #mean of the proportion of rain hours per station
      unlist() %>%  as.numeric()
  
   #Calculate the proportion of frost hours (temperatures < 0C) of all stations
   y$propfrost <- 
    group_by(x, stna) %>% 
      summarise(frostprop =sum(ifelse(soilt<0, 1,0))/length(soilt)) %>% 
      ungroup() %>% 
      summarise(frostprop = mean(frostprop, na.rm = TRUE)) %>% 
      unlist() %>%  as.numeric()

   
   #Calculate the number of favourable hours
   y <- 
   group_by(x, date_time) %>%
     summarise(temp = mean(temp, na.rm = TRUE),
               rh = mean(rh, na.rm = TRUE),
               rain = mean(rain, na.rm = TRUE)) %>% 
     mutate(favhour = ifelse(temp>= 10 & temp<= 26 & c(rh>88 | rain >= 0.1), 1, 0)) %>% 
     summarise(favhour = sum(favhour)) %>% 
     bind_cols(y, .)
   
   #Function to cacluate the  temperature infection factor 
   TempDev <-  function(temp){
     Tmax <- 27
     Topt <- 18
     Tmin <- 7
     Shape <- 1
     Rfact <- 1
     temp_dev <- sapply(temp, function(x) {
       temps <-
         Rfact * ((Tmax - x) / (Tmax - Topt) * ((x - Tmin) / (Topt - Tmin)) ^ ((Topt - Tmin) / (Tmax - Topt))) ^  Shape
       temps = ifelse(x < Tmin | x > Tmax, 0, temps)
     })
   }

   group_by(x, date_time) %>%
     summarise(temp = mean(temp, na.rm = TRUE),
               rh = mean(rh, na.rm = TRUE),
               rain = mean(rain, na.rm = TRUE)) %>% 
     mutate(betarisk = ifelse(rh>88 | rain >= 0.1 ,  
                              TempDev(temp), 
                              0)) %>% 
     summarise(betarisk = sum(betarisk))  %>%
     bind_cols(y, .)
   
   
   
    return(y)
  }) %>% bind_rows() 

head(wthd)
tail(wthd)
summary(wthd)
wthd$yr <- year(wthd$date)

save(wthd,file =  here::here("dat",  "wth_day.RData"))


disdf <-  read_csv(here::here("dat", "reports_summary.csv"))



# Create object for each season from august year 1 to september year 2

# The mean date of the last report is used 
meanlastrep <- 
  disdf %>% 
  mutate(doy = yday(lastrep)) %>% 
  summarise(mean(doy)) %>% pull() %>% round(0)

years <- c(2003:2017)

catls <- list()
for (i in seq_along(years)) {
  yrr <- years[i]
  dff<- dplyr::filter(wthd,yr==c(yrr-1)|yr==yrr)
  
  #Create categories after Zwankhuizen 
  dff <- 
  mutate(dff, period = ifelse(month %in% c(8:10)&yr == c(yrr-1), "foi", #Category A – factors affecting formation of inoculum
                           ifelse(month %in% c(11:12)&yr == c(yrr-1), "ooi", #Category B – factors affecting overwintering of inoculum
                                  ifelse(month %in% c(1:4)&yr ==yrr, "ooi", 
                                         ifelse(month %in% c(5:9)&yr ==yrr, "gs",NA)
                                         )
                                  )
                           )
  ) 
  
  #Remove data after the last report every year
  if(yrr %in% unique(disdf$yr)){
    dff <- dff[dff$date<=disdf[disdf$yr==yrr, "lastrep"]  %>% pull(),]
  }else{
    dff <- 
      dff[yday(dff$date) <= meanlastrep&dff$yr == yrr, ] %>%
        bind_rows(dff[dff$yr == c(yrr-1),],.)
                 
  }

  #Remove data that does not fall into categories
  dff <-dff[!is.na(dff$period),]

  
  #Assigne season ID
  dff$season <-  yrr
  catls[[i]] <- dff
  print(i)
}

names(dff)


wthcat <- 
lapply(catls, function(dff){
  
  y <- data.frame(yr = unique(dff$season))
  
  y <-
    dff %>% filter(period == "foi") %>%
    summarise(
      proprain_foi = sum(proprain),
      sumrain_foi = sum(rain)
    ) %>% 
    bind_cols(y, .)
  
  y <-
  dff %>% filter(period == "ooi") %>%
    summarise(
      proprain_ooi= sum(proprain), # sum of  mean daily rainfall 
      propfrost_ooi = sum(propfrost) # sum of mean frost day proportions   
    )%>% 
    bind_cols(y, .)
  
  # Hellmann value, Σ average daily temperatures <0°C, November–March
  # a measure of the coldness of the winter  
  y <- 
    dff %>% filter(period == "ooi") %>% 
    mutate(temp = ifelse(temp>0, 0, abs(temp))) %>% 
    summarise(hellman_ooi = sum(temp))%>% 
    bind_cols(y, .)
  
  y <- 
  dff %>% filter(period == "gs") %>% 
    mutate(temp = ifelse(temp>0, 0, abs(temp))) %>% 
    summarise(
      proprain_gs= sum(proprain), # sum of  mean daily rainfall
      sumrain_gs = sum(rain),
      sol_rad = sum(sol_rad),
      favhours = sum(favhour)
      
              )%>% 
    bind_cols(y, .)
  
  
  
  # rolling mean
  rlmean <- 10
  dff$wrh <- zoo::rollmean(dff$rh, rlmean, mean, align = 'center', fill = NA)
  dff$wtemp <- zoo::rollmean(dff$temp, rlmean, mean, align = 'center', fill = NA)
  
  
  # #conducive days
  # templine <- 10
  # dff <- 
  #   dff %>% 
  #   mutate(condday = ifelse( wtemp>=templine&rain>=.2&rh>=80& short_date<disout, 50,NA))
  # 
  # #sum of conditions prior to outbreak
  # dff <- 
  #   dff %>% 
  #   group_by(year) %>% 
  #   summarise(consum= sum(condday/50, na.rm = TRUE )) %>%
  #   left_join(dff,., by = "year")
  
  
  # dff %>% filter(period == "gs") %>% 
    


  return(y)
}) %>% bind_rows()




wthcat %>% 
  write_csv(here::here("out", "weather_categories.csv"))








