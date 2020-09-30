########################################################
#Data
########################################################
list.of.packages <-
  c(
    "tidyverse",
    "readxl",
    "broom", 
    "data.table",
    "minpack.lm",
    "knitr",
    "zoo",
    "egg",
    "ggthemes",
    "here",
    "stringr",
    "lubridate",
    "survival",
    "survminer"
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



###################################################################################
#Data  
###################################################################################

load(file =  here::here("dat",  "wth_day.RData"))
outbreaks <- read_csv(file  = here::here("dat", "outbreaks_fin.csv"))
outbreaks$date <- lubridate::mdy(outbreaks$date)
outbreaks$jday <- as.Date(outbreaks$date,format="%m/%d/%Y") %>% yday()

disdf <-  read_csv(here::here("dat", "reports_summary.csv"))


 
outdf <- 
outbreaks[outbreaks$Location!= "Oak Park",] %>%
  mutate(mon = month(date),
         day = day(date)) %>%
  group_by(mon, day) %>%
  unite(., d, mon, day, sep = "-", remove = FALSE) %>%
  mutate(d = as.Date(d, format = "%m-%d")) %>%
  ungroup()%>% 
  select(yr,date) %>% 
  group_by(yr,date) %>%
  summarise(count = n()) %>% 
  ungroup() %>% 
  group_by(yr) %>% 
  mutate(cums = cumsum(count)) %>% 
  dplyr::select(.,yr, date, cums) %>% 
  ungroup()






# rolling mean
rlmean <- 2
wthd$wrh <- zoo::rollmean(wthd$rh, rlmean, mean, align = 'center', fill = NA)
wthd$wtemp <- zoo::rollmean(wthd$temp, rlmean, mean, align = 'center', fill = NA)
wthd$wrain <- zoo::rollmean(wthd$rain, rlmean, mean, align = 'center', fill = NA)

rlmean_long <- 10
wthd$wlongtemp <- zoo::rollmean(wthd$temp, rlmean_long, mean, align = 'center', fill = NA)



linesize <- .35 #size of the line for variables

dd <- 
  wthd %>% 
  left_join(.,outdf, by = "date") %>% 
  mutate(rain = ifelse(rain == 0, NA, rain)) %>% 
  mutate(yr = year(date)) %>% 
  filter(yr %in% c(2003:2014)) %>% 
  left_join(., disdf, by = "yr")



#conducive days
templine <- 10
dd <-
  dd %>%
  mutate(
    condday = ifelse( wtemp>=templine&c(wrain>=.2|wrh>=85), 50,NA),
    conddaylong = ifelse( wlongtemp>=templine&c(wrain>=.2|wrh>=85), 40,NA)
    
    )

#sum of conditions prior to outbreak
dd <-
  dd %>%
  group_by(yr) %>%
  summarise(consum= sum(condday/50, na.rm = TRUE )) %>%
  left_join(dd,., by = "yr")

#LAbels for cumulative sum of critical days
dd <-
  dd %>%
  mutate(symptoms = paste0("Symptoms (", consum, " cond. days)"))


#Calculate dates for agronomic operations
dates_cut <- 
split(dd, dd$yr) %>% 
  lapply(., function(x){
    # split(dd, dd$yr)[[1]] ->x
    
    criteria <- as.numeric(x$soilt > 8)
    #cumulative sum days matching the criteria
    criteria_sum <-
      stats::ave(criteria, cumsum(criteria == 0), FUN = cumsum)
    pl_date <- x[match(3, criteria_sum) + 1, "date"]
    data.frame("year" = unique(year(x$date)),
               date = pl_date)
  })%>%
  bind_rows() %>%
  rename("plant_date"  = date)

dates_cut$plant_doy <- yday(dates_cut$plant_date)

write.csv(dates_cut, file = here::here("out", "cutoff_dates.csv"))

# We will assume that the full emergence takes 3 
# and additional minimum of 2 weeks for full emergence and for the first plants to start meeting in the rows.
meet_in_rows <- 5 * 7
dates_cut$prot_start <- with(dates_cut, plant_doy + meet_in_rows)
dates_cut$prot_start <-
  paste(dates_cut$year,
        dates_cut$prot_start,
        sep = "-") %>%
  strptime(format = "%Y-%j") %>%
  as.Date()
dates_cut <- 
  rename(dates_cut, yr = year)

dd <- 
  left_join(dd, dates_cut, by = "yr")


(
  p <-
    ggplot(dd) +
    geom_point(
      aes(initrep, 46),
      shape = 2,
      size = 1.5,
      fill = "black"
    ) +
    geom_point(
      aes(lastrep, 46),
      shape = 2,
      size = 1.5,
      fill = "black"
    ) +
    geom_point(aes(date, condday), shape = 3, size = .55) +
    # geom_point(aes(date, conddaylong), shape = 3, size = .55) +
    geom_line(aes(
      x = date,
      y = rh,
      colour = "Relative humidity (%)"
    ),
    size = linesize) +
    geom_line(aes(
      x = date,
      y = temp,
      colour = "Temperature (˚C)",
    ),
    size = linesize) +
    geom_line(
      aes(x = date,
          y = wtemp,
          colour = "Roll. mean T (˚C)"),
      linetype = "dashed",
      size = linesize
    ) +
    geom_line(aes(
      x = date,
      y = wlongtemp,
      colour = "Roll. mean T long. (˚C)"
    ),
    # linetype = "dashed",
    size = linesize) +
    
    geom_line(
      data = dd[complete.cases(dd[,"cums"]),],
      aes(x = date,
          y = cums,
          colour = "Outbreak reports"),
      # linetype = "solid",
      size = linesize
    ) +
    geom_col(
      aes(date,
          rain,
          fill = "Total precipitation (mm/day)"),
      size = 1.2 ,
      inherit.aes = TRUE,
      width = 0.8
    ) +
    scale_colour_manual(
      "Daily weather:",
      values = c(
        "Relative humidity (%)" = "#0EBFE9",
        "Temperature (˚C)" = "#ED2939",
        "Roll. mean T (˚C)" = "darkred",
        "Roll. mean T long. (˚C)" = "black",
        "Outbreak reports" = "darkgreen"
      )
    ) +
    scale_fill_manual(
      name = NA,
      values = c("Total precipitation (mm/day)" = "blue")
    ) +
    scale_y_continuous(
      breaks = c(0, 10, 50, 90, 100),
      limits = c(-1, 100),
      expand = c(0, 0)
    ) +
    scale_x_date(
      date_labels = "%b",
      date_breaks = "1 month",
      expand = c(.03, .04)
    ) +
    facet_wrap(
      ~ yr,
      scales = "free",
      ncol = 1,
      strip.position = "right"
    ) +
    guides(
      fill = guide_legend(
        nrow = 2,
        byrow = TRUE,
        title.position = "top"
      ),
      color = guide_legend(
        nrow = 2,
        byrow = TRUE,
        title.position = "top"
      ),
      linetype = guide_legend(
        nrow = 2,
        byrow = TRUE,
        title.position = "top"
      )
    ) +
    geom_text(aes(label= "First report", y = 39, x = initrep),size = 2.5)+
    geom_text(aes(label= "Last report", y = 39, x = lastrep-1.5),size = 2.5)+
    theme_bw() +
    theme(
      text = element_text(size = 10.8),
      # axis.text.x = element_blank(),
      # axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      legend.position = "top",
      strip.background = element_rect(colour = "black", size = 0.75),
      strip.placement = "outside",
      legend.title = element_blank(),
      panel.grid.minor =   element_blank(),
      panel.grid.major =   element_line(colour = "lightgray", size = linesize),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      panel.spacing.y = unit(0, "lines"),
      legend.box.spacing = unit(0.1, 'cm'),
      legend.margin = margin(0, 0, 0, 0, "cm"),
    )
)+

  geom_vline(xintercept = dd$plant_date)+
  geom_text(aes(label= "Est. planting", y = 39, x = dd$plant_date-8),size = 2)+
ggsave(
  # plot = p,
    here::here("out", "wth_rolmean_cumsum_reports.png"),
    width = 7.5,
    height = 12,
    dpi = 420
  )


shell.exec(here::here("out", "wth_rolmean_cumsum_reports.png"))



###################################################################################
#Analysis of rolling mean periods
###################################################################################
# For the inital disease outbreak - Temperature needs to be constantly above 10 C for a period fo time and 
# a few humid events

#After the primary disease cycles, secondaty cycles need lower conditions jus a couple of days

# Sum of  periods prior to outbreak
# 

# Curves for the range of biologically relevant threholds to determine the apr 
 names(dd)

dd %>% 
  select(yr, sumrep, inidoy, lastdoy, sumrep) %>% 
  group_by(yr) %>% 
  summarise_all(unique)



library("PerformanceAnalytics")

#calcualte summaries of conducive periods
ddd <-
  dd %>%
  select(yr, date, initrep, condday, consum, lastrep,conddaylong) %>%
  base::split(., dd$yr) %>%
  lapply(., function(x) {
    # split(dd, dd$yr)[[1]] ->x
    x <-
      x %>%
      select(yr, date, initrep, condday, consum, lastrep,conddaylong) %>%
      
      mutate(
        condday = ifelse(condday == 50, 1, 0),
        conddaylong = ifelse(conddaylong == 40, 1, 0),
      ) %>%
      mutate(
        cond_till_initrep_long = ifelse(date >= initrep , NA, conddaylong),
        cond_till_initrep = ifelse(date >= initrep , NA, condday),
        cond_during = ifelse(date >= lastrep , NA, condday),
        cond_during = ifelse(date <= initrep  , NA, cond_during),
        cond_till_lastrep = ifelse(date >= lastrep , NA, condday)
      ) %>%
      group_by(yr) %>%
      summarise(
        cond_till_initrep_long = sum(cond_till_initrep_long, na.rm = T),
        cond_till_initrep = sum(cond_till_initrep, na.rm = T),
        cond_during = sum(cond_during, na.rm = T),
        cond_till_lastrep = sum(cond_till_lastrep, na.rm = T)
        
      )
    return(x)
  }) %>%
  bind_rows() %>%
  left_join(., disdf, by = "yr") %>% 
  mutate(durrep = lastdoy - inidoy) 

save(ddd,file = here::here("out", "Conditions_pre&during.RData"))


png(file =  here::here("out", "wth_rolmean_cumsum_correlation_matrix.png"),
    width = 600, height = 600)
chart.Correlation(  dplyr::select(ddd,-c("yr", "initrep", "lastrep")))

dev.off() 
  


shell.exec(here::here("out", "wth_rolmean_cumsum_correlation_matrix.png"))

pi <- 
  ddd %>%
  dplyr::select(-c("sumrep", "lastrep",  "initrep", "lastdoy","durrep" )) %>% 
  reshape2::melt(id.vars =c("inidoy","yr") ) %>% 
  ggplot(aes(inidoy , value))+
  geom_point(aes(colour = factor(yr)))+
  geom_smooth(method = "lm")+
  facet_wrap(~variable, nrow = 1)+
  theme_article()+
  theme(legend.position = "top")


ps <- 
  ddd %>%
  dplyr::select(-c("initrep", "lastrep",  "inidoy", "lastdoy","durrep" )) %>% 
  reshape2::melt(id.vars =c("sumrep","yr") ) %>% 
  ggplot(aes(sumrep, value))+
  geom_point(aes(colour = factor(yr)))+
  geom_smooth(method = "lm")+
  facet_wrap(~variable, nrow = 1)+
  theme_article()+
  theme(legend.position = "none")


pl <- 
  ddd %>%
  dplyr::select(-c("initrep", "sumrep",  "inidoy", "lastrep","durrep" )) %>% 
  reshape2::melt(id.vars =c("lastdoy","yr") ) %>% 
  ggplot(aes(lastdoy, value))+
  geom_point(aes(colour = factor(yr)))+
  geom_smooth(method = "lm")+
  facet_wrap(~variable, nrow = 1)+
  theme_article()+
  theme(legend.position = "none")


pd <- 
  ddd %>%
  dplyr::select(-c("initrep", "sumrep",  "inidoy", "lastrep","lastdoy" )) %>% 
  reshape2::melt(id.vars =c("durrep","yr") ) %>% 
  ggplot(aes(durrep, value))+
  geom_point(aes(colour = factor(yr)))+
  geom_smooth(method = "lm")+
  facet_wrap(~variable, nrow = 1)+
  theme_article()+
  theme(legend.position = "none")


ggarrange(pi, ps, pl,pd,ncol = 1, heights =   c(1.4,1,1,1))+
  ggsave(
    here::here("out", "wth_rolmean_lms.png"),
    width = 8,
    height = 8,
    dpi = 420
  )

shell.exec(here::here("out", "wth_rolmean_lms.png"))



###################################################################################
#Survival analysis
###################################################################################

#subset station data with stations present in data set


outdat <- 
outbreaks[outbreaks$Location!= "Oak Park",] %>%
  mutate(mon = month(date),
         day = day(date)) %>%
  group_by(mon, day) %>%
  unite(., date, mon, day, sep = "-", remove = FALSE) %>%
  mutate(d = as.Date(date, format = "%m-%d")) %>%
  ungroup() %>% 
  dplyr::select(.,yr, jday) %>% 
  mutate(
    yr = as.numeric(yr-min(yr-1)),
         out = 1)

# outdat <- outdat[outdat$yr<4, ]

str(outdat)
surv_object <- Surv(time = outdat$jday, event = outdat$out )

km <- survfit(surv_object ~ yr, data = outdat)
summary(km)
print(km)
summary(km)$table
ggsurvplot(km, data = outdat, pval = TRUE, linetype = "strata",fun = "event")



cm <- coxph(Surv(time =jday, event = out ) ~ cluster(yr), data = outdat)
cm <- coxph(Surv(time =jday, event = out ) ~ yr, data = outdat)
summary(cm)

ggsurvplot(cm, data = outdat, pval = TRUE,fun = "event")



# Weibull model
wbmod <- survreg(Surv(time =jday, event = out) ~ yr, data = outdat)



# Compute the median survival from the model
predict(wbmod, type = "quantile", p = 1 -0.5, newdata = data.frame(1))


# Retrieve survival curve from model
surv <- seq(.99, .01, by = -.01)

ggsurvplot_facet(wbmod, data = outdat, pval = TRUE, facet.by = "yr")




fit <- survfit(Surv(time=Day, event=ones)~1, data=data )




data = cbind( as.data.frame(matrix (c(0,143,2,28,3,126,4,103,6,102,7,100,8,88,9,70,10,51,11,44,13,27,15,10,17,4,18,3,20,2,22,2,24,0),
                                    ncol=2, byrow = TRUE)),
              "ones"=1)
colnames(data) <- c("Day", "Survival", "ones")

fit <- survfit(Surv(time=Day, event=ones)~1, data=data )
plot(wm);



fit <- survfit(Surv(time, status) ~ sex, data = lung)
print(fit)
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))
