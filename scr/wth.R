
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
    "lubridate"
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

if (length(new.packages))
  install.packages(new.packages, repos = c(CRAN="https://cran.r-project.org/"))


if ("reconPlots" %in% installed.packages() == FALSE) devtools::install_github("andrewheiss/reconPlots")

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
########################################################
#Filter data sets with low number of missing values
########################################################

load(here::here("dat", "wth.Rdata"))
# save( wth,file = here::here("dat", "wth.Rdata"))

# Filter wth data we will use in  to estimate treatment frequencies 
wth <- 
  wth[ , c("date_time","short_date", "id", "year","month","doy","day","hour","stno","temp","dewpt","wetb", "rhum",
           "rain", "stna", "lat", "long"  , "sol_rad","height", "sol_nasa")]


nrow(wth)
wth <- unite(wth, id, c("stna", "year"), remove = FALSE)
wth_ls <- split(wth,  wth$id )
not_empty <-sapply(wth_ls, function(x) {nrow(x)>2}) 
wth_ls <- wth_ls[as.vector(not_empty, mode = "logical")]; rm(not_empty)

length(wth_ls)

for(x in seq_along(wth_ls)){
  # x <- 1
  # x <- "Roches Point.2018"
  # x <- "Roches Point.2007"
  # x <- "Roches Point.2001"
  # x <- "Roches Point.2001"
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

# Check stations which were erected during the period of interest
for (i in seq(wth_ls)) {
  wth_ls[[i]] -> x
  if(x$hour[1:24] != c(0:23)){print(x$id[1])}
}

# #Remove the data that doesnt contaill all the dates because the staion was closed/oppened 
# datetimes <-  strptime(c("30.04", "16.09"), format = "%d.%m")
# no_of_days <- difftime(datetimes[2], datetimes[1], units = "days")+1
# wth_ls <-  wth_ls[sapply(wth_ls, function(x) length(unique(x$doy))==no_of_days)] #no of days
# wth_ls <-  wth_ls[sapply(wth_ls, function(x) nrow(x)==c(no_of_days*24))] #no of hours

#Remove data with more than  1% of missing values


sapply(wth_ls, function(x) sum(is.na(x[,c( "temp", "rhum")])))%>% as.vector()
nas <- sapply(wth_ls, function(x) mean(is.na(x[,c( "temp", "rhum")])))%>% as.vector() %>% round(3)
sum(nas<0.05)
wth_ls <- wth_ls[nas<0.05]

length(wth_ls)

sapply(wth_ls, function(x) nrow(x)) %>% as.numeric()
# all(sapply(wth_ls, function(x) nrow(x)) %>% as.numeric()) 
wth_ls <- lapply(wth_ls, function(x) x[!duplicated(x),])
sapply(wth_ls, function(x) nrow(x)) %>% as.numeric() 

wth <- bind_rows(wth_ls)








# # Select Stations
# stations <- c(
#   "Katesbridge",
#   "Aldergrove",
#   "Ballykelly",
#   "Magilligan No 2",
#   "Castlederg",
#   "Derrylin",
#   "Derrylin Cornahoule",
#   "Ballywatticock"
# )
# 
# length(unique(wth$stna))
# 
# glimpse(wth)
# 
# #Remove stations whihc are not a part of the analysis
# wth <- filter(wth,stna %in% stations)

#The number of station/years
unique(wth$id) %>% length()

wth_ls <- split(wth, wth$id)


sapply(wth_ls, function(x) mean(is.na(x[,c( "temp", "rhum")])))%>% 
  as.vector() %>% round(3) %>% sort()



# Total number of stations 
unique(wth$stna) %>% length()

#length
unique(wth$id) %>% length()


# Total number of year/stations 
lapply(wth_ls, function(x) length(unique(x$year))) %>% 
  bind_cols() %>% t() %>% sum()

#Weaher data avialability per year
# lapply(wth_ls, function(x) {
#   years <-  unique(x$year)
#   stna <- rep(unique(x$stna), length(years))
#   country <-  rep(unique(x$country), length((years)))
#   data.frame(stna = stna,years = years, country = country)
# }) %>% 
#   bind_rows() %>%
#   group_by(country) %>% 
#   ggplot() + 
#   ggridges::geom_ridgeline(aes(x=years,y=as.factor(stna),fill = country,height = 0.4),stat="identity")+
#   scale_y_discrete(name = "Station Name")+
#   ggtitle("wth Data For The Treatment Evaluation")+
#   ggridges:: theme_ridges(center = TRUE, font_size = 10)

wth <- bind_rows(wth_ls)

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

########################################################
#Stations selected for the treatment analysis
########################################################

#Visualisation
low_na <- 0.01
na_prop <- 0.1

ni <- 
  wth %>% 
  group_by( stna, year)  %>% 
  dplyr::summarize(sum_NA = round(mean(is.na(c(temp,rhum, rain))),2)) 


ni$line_positions_y <-
  c(uniq_stna <- seq(.5, length(unique(ni$stna)), 1),
    rep(1.5, nrow(ni) - length(seq(
      1.5, length(unique(ni$stna)), 1
    ))))

# p1 <-
  ni %>% 
  mutate(perc_missing =  ifelse(sum_NA < low_na, paste("<", low_na), paste( low_na, "-", na_prop))) %>% 
  mutate(perc_missing = factor(perc_missing)) %>% 
  mutate(line_positions_x = year + .5 ) %>% 
  ggplot(., aes(year, stna))+
  geom_tile(aes(fill = perc_missing), alpha=0.5 )+
  scale_x_continuous(breaks = (seq(2000, 2018, 2)))+
  geom_vline(aes(xintercept = line_positions_x),
             color = "darkgray",
             size  = .2,
             alpha = .6)+
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
        panel.grid = element_blank())

wth %>% 
  mutate(rhum = ifelse(rhum<0, NA, rhum)) %>% 
  ggplot()+
  geom_histogram(aes(rhum),bins = 50)+
  scale_x_continuous(limits = c(50,100))+
  facet_wrap(~stna)



save(wth_ls, file= here::here("dat", "treatment_no_estim.Rdata"))
# load( file= here::here("dat", "treatment_no_estim.Rdata"))

