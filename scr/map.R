
ni_sf <-
all_counties.sf[all_counties.sf$CountyName  == c("Tyrone","Antrim","Armagh", "Fermanagh","Londonderry","Down"),]
basemap <- 
ggplot() +
  # Plot borders (shapefile)
  geom_sf(
    data = ni_sf
      ,
    color= "darkolivegreen3",
    fill = "darkolivegreen3"
  ) +
  #Set the theme
  theme_bw(
           
  ) +
  #limit the plotting area
  coord_sf(xlim = c(-8.4, -4.9), ylim = c(54, 55.4), expand = FALSE) +
theme(axis.text.x=element_blank(),
      axis.text.y=element_blank())
  # Define names for labs
  labs(x = "Longitude", y = "Latitude")+
  #add fancy anotation
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.35, "in"),
                         pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  annotation_scale(location = "br", width_hint = 0.4) 

plist <- list()
for(i in seq(unique(df_loc_sf$yr))){
  yrs <- unique(df_loc_sf$yr)[i]
  
  plist[[i]] <- 
  basemap+
    geom_sf(
      data = df_loc_sf[df_loc_sf$yr == unique(df_loc_sf$yr)[i],],
      aes(fill = source, color = source),
      shape = 23,
      size = 2
    ) +
    geom_point( size = .5)
}

# plotf <- 
ggpubr::ggarrange(plotlist = plist, 
                  # widths = c(2.5,2.5),
                  # heights = c(.5,.5),
                  # labels = c("a)","b)"),
                  # ncol= 3,
                  common.legend = TRUE,
                  legend = "bottom"
                  )+

ggpubr::ggexport(plotlist = plist, 
                 # widths = c(2.5,2.5),
                 # heights = c(.5,.5),
                 # labels = c("a)","b)"),
                 ncol= 3,
                 nrow = 4,
                 common.legend = TRUE,
                 legend = "bottom",
                 filename = here::here("out", "1mapeeeeeeeeeeeeeeee.png")
)
shell.exec(here::here("out", "1mapeeeeeeeeeeeeeeee.png"))

ggsave(
  # plot = plotf[1],
  filename = here::here("out", "map.png"),
  device = "png",
  width = 8,
  height = 9,
  dpi = 820
)



plotf <- 
  cowplot::plot_grid(plotlist =plist,
                     ncol = 3,
                     nrow = 4,
                     labels = as.character(unique(df_loc_sf$yr)),
                     label_size = 10)
  
  ggpubr::ggexport(
    plotlist = plist,
    filename = here::here("out", "maps.png"),
    ncol = 3,
    nrow = 4,
    # width = 8,
    # height = 9,
    dpi = 820
  )
  shell.exec(here::here("out", "maps.png"))
  


  ggsave(
    print(plotf),
    filename =  "map.png"
    # filename = here::here("out"),
    # width = 8,
    # height = 9,
    # dpi = 820
  )



