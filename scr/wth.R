
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


################################################333
#Load data
#####################################################

load(here::here("dat", "wth.Rdata"))

weather <- 
  weather[weather$country == "NI",]

unique(weather$stna)


wth <-  filter(weather, year>=2002)
wth <-  filter(wth, year<=2017)

# Filter wth data we will use in  to estimate treatment frequencies 
wth <- 
  wth[ , c("date_time","short_date",  "year","month","doy","day","hour","stno","temp", "rhum",
           "q10cm_soil_temp",
           "rain", "stna", "lat", "long", "sol_nasa")]

unique(wth$stna)
unique(wth$year) %>% sort()

nrow(wth)
wth <- unite(wth, id, c("stna", "year"), remove = FALSE)
wth_ls <- split(wth,  wth$id )
not_empty <-sapply(wth_ls, function(x) {nrow(x)>2}) 
wth_ls <- wth_ls[as.vector(not_empty, mode = "logical")]; rm(not_empty)

length(wth_ls)

for(x in seq_along(wth_ls)){
  df <- wth_ls[[x]]
  #sort problems with the dates
  df <- arrange(df, date_time)
  df <- df[!duplicated(df$date_time), ]
  dff <- data.frame(date_time = seq(ymd_hms(paste0(unique(df$year),"-01-01 00:00:00")),ymd_hms(paste0(unique(df$year),"-12-31 23:00:00")), by = 'hour'))
  df <- left_join(dff, df, by = "date_time")
  df$short_date <-  as.Date(df$date_time)
  df$stna <-  unique(df$stna)[!is.na(unique(df$stna))]
  df$stno <-  unique(df$stno)[!is.na(unique(df$stno))]
  df$year <- unique(df$year)[!is.na(unique(df$year))]
  df$country <-  unique(df$country)[!is.na(unique(df$country))]
  df$doy = yday(df$date_time)
  df$hour = hour(df$date_time)
  df <- df[!duplicated(df), ]
  wth_ls[[x]] <- df
  print(paste(x, "of" ,length(wth_ls)))
}



for (i in seq(wth_ls)) {
  wth_ls[[i]] -> x
  if(x$hour[1:24] != c(0:23)){print(x$id[1])}
}


# Check stations which were erected during the period of interest
wth <- bind_rows(wth_ls)
wth$sol_nasa[is.na(wth$sol_nasa)] <- 0
wth$date <- wth$short_date
wth$short_date <- NULL

# # TODO # Check if some removal of data is necessary
# todor::todor("dd")
# #Remove data with more than  1% of missing values
# wth_ls <- split(wth,  wth$stna )
# sapply(wth_ls, function(x) sum(is.na(x[,c( "temp", "rhum")])))%>% as.vector()
# nas <- 
#   sapply(wth_ls, function(x) mean(is.na(x[,c( "temp", "rhum")])))%>% as.vector() %>% round(3)
# sum(nas<0.1)
# wth_ls <- wth_ls[nas<0.05]
# 
# length(wth_ls)

summary(wth)
wth <- 
wth %>% 
  mutate(rh = ifelse(rhum<0, NA, rhum)) %>% 
  filter(rain<30) %>% 
  rename(soilt =q10cm_soil_temp) %>% 
  select(-rhum)

summary(wth)

  
  
########################################################
#Stations selected for the analysis
########################################################

#Visualisation
low_na <- 0.01
na_prop <- 0.1

ni <- 
  wth %>% 
  group_by( stna, year)  %>% 
  dplyr::summarize(sum_NA = round(mean(is.na(c(temp,rh, rain))),2)) 

#reduce the size of data by removing the metadata
wth <-
  wth%>%
  select(-c(stno,lat, lon, elev, open, closed))



ni$line_positions_y <-
  c(
    uniq_stna <- seq(.5, length(unique(ni$stna)), 1), rep(1.5, nrow(ni) - length(seq(1.5, length(unique(ni$stna)),1)))
    )

# p1 <-
ni %>% 
  mutate(perc_missing =  ifelse(sum_NA < low_na, paste("<", low_na), paste( low_na, "-", na_prop))) %>% 
  mutate(perc_missing = factor(perc_missing)) %>% 
  # mutate(line_positions_x = year + .5 ) %>% 
  ggplot(., aes(year, stna))+
  geom_tile(aes(fill = perc_missing), alpha=0.5 )+
  scale_x_continuous(breaks = (seq(2000, 2018, 2)))+
  # geom_vline(aes(xintercept = line_positions_x),
  #            color = "darkgray",
  #            size  = .2,
  #            alpha = .6)+
  # geom_hline(aes(yintercept = line_positions_y),
  #            color = "darkgray",
  #            size  = .2,
  #            alpha = .8)+
  theme_article()+
  ggtitle("Data for the treatment evaluation")+
  labs(x = "", y="",fill = "Proportion missing:")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "top", 
        panel.grid = element_blank())+
  # save the plot
  ggsave(
    file = here::here("out" , "station_years.png"),
    width = 13,
    height = 15.5,
    units = "cm",
    dpi = 320
  )

wth %>% 
  ggplot()+
  geom_histogram(aes(rh),bins = 50)+
  # scale_x_continuous(limits = c(50,100))+
  facet_wrap(~stna)

wth %>% 
  mutate(rain = ifelse(rain>0, rain, NA)) %>% 
  ggplot()+
  geom_histogram(aes(rain),bins = 50)+
  # scale_x_continuous(limits = c(50,100))+
  facet_wrap(~stna)

wth %>% 
  # mutate(rain = ifelse(rain>0, rain, NA)) %>% 
  ggplot()+
  geom_histogram(aes(temp),bins = 50)+
  # scale_x_continuous(limits = c(50,100))+
  facet_wrap(~stna)





########################################################
#Explore NAs
########################################################


library("naniar")

# gg_miss_upset(wth)
# gg_miss_var(wth,facet = stna)
# gg_miss_var(select(wth, -c(airp,wspd)),facet = stna)

gg_miss_fct(x = ., fct = stna)

wth <- 
  wth %>% 
  filter(stna != "Belfast Harbour No 2")

wth %>% 
  mutate(mon = lubridate::month(date_time)) %>% 
  # dplyr::filter(mon %in% c(9:12,1:5)) %>% 
  select(., c( temp, rh, sol_nasa, rain, stna,soilt)) %>% 
  gg_miss_fct(x = ., fct = stna)



save(wth, file= here::here("dat", "data_for_analysis.Rdata"))
# load( file= here::here("dat", "treatment_no_estim.Rdata"))


########################################################
#Map stations
########################################################

library("maps")
df_loc <- 
  wth %>%  
  group_by(stna) %>% select(stna,lat,long) %>% summarise_all(unique) 

df_loc$lab <-
  wth %>% 
  group_by(stna) %>% 
  summarise(years_available = length(unique(year))) %>% 
  unite( col = lab, c("stna", "years_available"), sep = " (") %>% 
  mutate(lab = paste0(lab, ")")) %>% 
  ungroup() %>% 
  unlist()
df_loc$open <-
  wth %>% 
  group_by(stna) %>% 
  summarise(open = min(unique(year)),
            closed = max(unique(year))) %>% 
  unite( col = lab, c("stna", "open", "closed")) %>% 
  unlist()

# ireland = fortify(map_data("world", region = "ireland"))
# ni = fortify(map_data("world", region = "uk"))
# ni <- ni[ni$subregion == "Northern Ireland",]
# ireland <- bind_rows(ireland,ni)
# 
# ggplot() + 
#   geom_polygon(data = ireland, aes(x=long, y = lat, group = group), fill = "darkolivegreen3")  +
#   coord_fixed(1.5)+
#   geom_point(data = df_loc, aes(x = long, y = lat, fill = "red", alpha = 0.8), size = 1, shape = 2) +
#   guides(fill=FALSE, alpha=FALSE, size=FALSE) +
#   ggrepel::geom_text_repel(aes(x = c(df_loc$long), y = c(df_loc$lat-0.05), label = df_loc$stna),size = 3)+ 
#   # annotate("text",x = c(df_loc$long), y = c(df_loc$lat-0.05), label = df_loc$stna, size = 2)+
#   ggthemes::theme_tufte()+
#   ggsave(file = paste0("./tmp/all_stationmap.png"), width = 15, height = 28, units = "cm")
# 
# #Plot NI 
# 
# ggplot() + 
#   geom_polygon(data = ni, aes(x=long, y = lat, group = group), fill = "darkolivegreen3")  +
#   coord_fixed(1.5)+
#   geom_point(data = df_loc[df_loc$country=="NI",], aes(x = long, y = lat, color = "red"), size = 1, shape = 2) +
#   guides(fill=FALSE, alpha=FALSE, size=FALSE) +
#   # ggrepel::geom_text_repel(aes(x = c(df_loc[df_loc$country=="NI",]$long), 
#   #                              y = c(df_loc[df_loc$country=="NI",]$lat-0.05), 
#   #                              label = df_loc[df_loc$country=="NI",]$stna),size = 3)+ 
#   annotate("text",x = c(df_loc[df_loc$country=="NI",]$long), 
#            y = c(df_loc[df_loc$country=="NI",]$lat-0.02), 
#            label = df_loc[df_loc$country=="NI",]$open, size = 2)+
#   ggthemes::theme_tufte()+
#   theme(legend.position = "none")+
#   labs(title = "Locations of wth stations in Northern Ireland")+
#   ggsave(file = paste0("./tmp/NI_stationmap.png"), width = 15, height = 15, units = "cm")
# 
# 
# 


library("ggspatial")
library("sf")
library("ggrepel")

#load the shape file
load(here::here("dat", "All_Ireland.RData"))


df_loc_sf <- 
  df_loc %>% 
  mutate(colstna = ifelse(stna %in% c("Oak Park", "Dunsany", "Moore Park", "Johnstown Castle", "Gurteen"), "Observed", "Observed and forecasted" )) %>%  
  st_as_sf( agr = "lab",coords= c( "long","lat"),remove = FALSE)


#Set coordinate reference system
st_crs(df_loc_sf) <- 
  st_crs(all_counties.sf[all_counties.sf$CountyName  == c("Tyrone","Antrim","Armagh", "Fermanagh","Londonderry","Down"),])


basemap <- 
ggplot() +
  # Plot borders (shapefile)
  geom_sf(
    data = 
      all_counties.sf[all_counties.sf$CountyName  == c("Tyrone","Antrim","Armagh", "Fermanagh","Londonderry","Down"),],
    color= "#81E8C2",
    fill = "#81E8C2"
  ) +
  geom_sf(
    data = 
      all_counties.sf[all_counties.sf$CountyName  != c("Tyrone","Antrim","Armagh", "Fermanagh","Londonderry","Down"),],
    color= "#F6F6B2",
    fill = "#F6F6B2"
  ) +
  
  #Set the theme
  theme_bw(base_family = "Roboto Condensed",
           base_size = 12 #Change the overall font size 
  ) +
  theme(panel.grid.major = element_line(color = gray(.9),   size = 0.1),
        panel.background = element_rect(fill = "aliceblue"))+
  #limit the plotting area
  coord_sf(xlim = c(-8.4, -5), ylim = c(53.8, 55.4), expand = FALSE) +
  # Define names for labs
  labs(x = "Longitude", y = "Latitude")+
  #add fancy anotation
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.35, "in"),
                         pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  annotation_scale(location = "br", width_hint = 0.4) 

pointmap <- 
basemap+
  geom_sf(
    data = df_loc_sf,
    # aes(fill = "black),
    shape = 23,
    size = 2
  ) +
  #Add names of stations
  geom_text_repel(data = df_loc_sf, 
                  aes(x = long, y = lat, label = lab),
                  size = 2.7
                  # nudge_x = c(1, -1.5, 2, 2, -1), 
                  # nudge_y = c(0.25, -0.25, 0.5, 0.5, -0.5)
  ) +
  #Change the border color. This section can be removed in single color of point is ok
  # scale_color_manual(name = "Data:",
  #                    labels = c("Observed", "Observed&\nForecast"),
  #                    values = c( "black","blue")) +
  # #change the fill manually
  # scale_fill_manual(name = "Data:",
  #                   labels = c("Observed", "Observed&\nForecast"),
  #                   values = c("black","blue")) +
  theme(
    strip.background = element_blank(),
    legend.position = c(.16, .90), #place position of the legend inside plotting area
    legend.box.background = element_rect(color = "black", size = .5),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )+
  coord_sf(xlim = c(-8.4, -5), ylim = c(53.8, 55.4), expand = FALSE) 

# save the plot
ggsave(
  file = here::here("out" , "map_points.png"),
  plot = pointmap,
  width = 15,
  height = 12.5,
  units = "cm",
  dpi = 320
)

shell.exec(here::here("out" , "map_points.png"))


# library(leaflet)
# 
# leaflet(df_loc) %>%
#   setView(mean(df_loc$long),mean(df_loc$lat),7) %>%
#   addTiles(group = "OSM (default)") %>%
#   setView(-8,53.5,6)%>%
#   addMarkers(lng = df_loc$long, lat = df_loc$lat,
#              label = ~as.character(df_loc$stna)
#              # icon=greenLeafIcon
#              # radius = 2
#   )
# 

########################################################################
# Weather data summaries
#########################################################################


library(data.table)
setDT(wth)[, list(meantemp = mean(temp, na.rm = TRUE)), 
          by = list(stno, year)] %>% 
  ggplot()+
    geom_line(aes(year, meantemp, color = factor(stno)))








library(xts)
library(hydroTSM)

# Generate random data
set.seed(2018)
date = seq(from = as.Date("2016-01-01"), to = as.Date("2018-12-31"),
           by = "days")
temperature = runif(length(date), -15, 35)
dat <- data.frame(date, temperature)

# Convert to xts object for xts & hydroTSM functions
dat_xts <- xts(dat[, -1], order.by = dat$date)

# All daily, monthly & annual series in one plot
hydroplot(dat_xts, pfreq = "dma", var.type = "Temperature")

# Weekly average
dat_weekly <- apply.weekly(dat_xts, FUN = mean)
plot(dat_weekly)

# Monthly average
dat_monthly <- daily2monthly(dat_xts, FUN = mean, na.rm = TRUE)
plot.zoo(dat_monthly, xaxt = "n", xlab = "")
axis.Date(1, at = pretty(index(dat_monthly)),
          labels = format(pretty(index(dat_monthly)), format = "%b-%Y"),
          las = 1, cex.axis = 1.1)


# Seasonal average: need to specify the months
dat_seasonal <- dm2seasonal(dat_xts, season = "DJF", FUN = mean, 
                            na.rm = TRUE)
plot(dat_seasonal)








head(wth)

library(xts)

wthxts <- 
  wth[ ,!colnames(wth) %in% 
         c("short_date", "id","year", "month", "doy", "day",  "hour", "stna", "country")] 

wthxts <- 
xts(wthxts[ , !colnames(wthxts) %in% c("date_time")],order.by = wthxts$date_time)

str(wthxts)
wthxts <- split(wthxts, wthxts$stno)


for (i in seq(wthxts)) {
  #i=1
dff <- wthxts[[i]]
  periodicity(dff)

  temps_weekly <- split(, f = "day")


# Create a list of weekly means, temps_avg, and print this list
temps_avg <- 
  
  lapply(X = temps_weekly, FUN = mean)
temps_avg
}



# Round observations in z to the next hour
z_round <- align.time(z, n = 3600)





