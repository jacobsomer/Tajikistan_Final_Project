#install.packages("rayshader", dependencies = TRUE)
#install.packages("rgl")
#install.packages("rayrender", dependencies = TRUE)
install.packages("rgl", repos="http://R-Forge.R-project.org",dependecies=TRUE)

remove.packages("rgl")

library(raster)
library(sf)
library(tidyverse)
library(rgl)
library(rayshader)
library(rayrender)
lbr_topo <- raster("tjk_srtm_topo_100m.tif")


setwd("~/R Project Folder/project 1/Data/Project 2 Data")

your_pop15 <- raster("TJK_ppp_2020_UNadj.tif")

your_adm2  <- read_sf("gadm36_TJK_2.shp")

your_subset_district <- your_adm2 %>%
  filter(NAME_2 == "Vanj")
combined_topo <- crop(lbr_topo, your_subset_district)

combined_matrix<-raster_to_matrix(combined_topo)

combined_matrix %>%
  sphere_shade() %>%
  add_water(detect_water(combined_matrix)) %>%
  plot_map()

ambientshadows <- ambient_shade(combined_matrix)

combined_matrix %>%
  sphere_shade() %>%
  add_water(detect_water(combined_matrix), color = "lightblue") %>%
  add_shadow(ray_shade(combined_matrix, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(combined_matrix, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0.1) %>%
  plot_3d(combined_matrix,
        zscale = 20,windowsize = c(1000,1000), 
        phi = 40, theta = 135, zoom = 0.5, 
        background = "grey30", shadowcolor = "grey5", 
        soliddepth = -50, shadowdepth = -100)


render_snapshot(title_text = "Vanj", 
                title_size = 50,
                title_color = "grey90")



         
vanj<-ggplot() +
  geom_sf(data = your_subset_district,
          size = 4.5,
          linetype = "11",
          color = "gold",
          alpha = 0) +
  theme_void() + theme(legend.position="none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y=NULL, title=NULL)

png("combined.png", width = 920, height = 1136, units = "px", bg = "transparent")
vanj
dev.off()

overlay_img <- png::readPNG("combined.png")

combined_matrix %>%
  sphere_shade() %>%
  add_water(detect_water(combined_matrix)) %>%
  add_overlay(overlay_img, alphalayer = 0.95) %>%
  plot_map()

combined_matrix %>%
  sphere_shade() %>%
  add_water(detect_water(combined_matrix), color = "lightblue") %>%
  add_shadow(ray_shade(combined_matrix, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(combined_matrix, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0.1) %>%
  add_overlay(overlay_img, alphalayer = 0.95) %>%
  plot_3d(combined_matrix, zscale = 20,windowsize = c(1000,1000), 
          phi = 40, theta = 135, zoom = 0.75, 
          background = "grey30", shadowcolor = "grey5", 
          soliddepth = -50, shadowdepth = -100)


obj <- ggplot() +
  geom_sf(data = your_subset_district,
          size = 5.0,
          linetype = "11",
          color = "gold",
          alpha = 0) +
  geom_sf(data = urban_areas,
          size = 0.75,
          color = "gray50",
          fill = "gold3",
          alpha = 0.5) +
  geom_sf(data = primary,
          size = .5,
          color = "red") +
  geom_sf(data = secondary,
          size = .3,
          color = "red") +
  geom_sf(data = tertiary,
          size = .3,
          color = "yellow") +
  geom_sf(data = primary1,
          size = 8,
          color = "red") +
  geom_sf(data = secondary1,
          size = 6,
          color = "magenta") +
  geom_sf(data = tertiary1,
          size = 4,
          color = "purple")+
  theme_void() + theme(legend.position="none") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x=NULL, y=NULL, title=NULL)

png("combined1.png", width = 920, height = 1136, units = "px", bg = "transparent")
obj
dev.off()

overlay_img1 <- png::readPNG("combined1.png")

combined_matrix %>%
  sphere_shade() %>%
  add_water(detect_water(combined_matrix)) %>%
  add_overlay(overlay_img1, alphalayer = 0.95) %>%
  plot_map()

open3d()
combined_matrix %>%
  sphere_shade() %>%
  add_water(detect_water(combined_matrix), color = "lightblue") %>%
  add_shadow(ray_shade(combined_matrix, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(combined_matrix, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0.1) %>%
  add_overlay(overlay_img1, alphalayer = 0.95) %>%
  plot_3d(combined_matrix, zscale = 20,windowsize = c(1000,1000), 
          phi = 40, theta = 135, zoom = 0.75, 
          background = "grey30", shadowcolor = "grey5", 
          soliddepth = -50, shadowdepth = -100)
M <- par3d("userMatrix")
if (!rgl.useNULL())
  play3d( par3dinterp(time = (0:2)*0.75, userMatrix = list(M,
                                                           rotate3d(M, pi/2, 1, 0, 0),
                                                           rotate3d(M, pi/2, 0, 1, 0) ) ), 
          duration = 3 )

movie3d( spin3d(), duration = 5 )
render_movie("Tajikistan")
