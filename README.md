setwd("C:/Users/Administrator/Desktop/R training")

library(sf)
library(dplyr)
library(terra)
eth <- st_read("eth_admbnda_adm3_csa_bofedb_2021.shp")
sidama <- eth %>% filter(ADM2_EN=="Sidama")
shp <- eth %>% filter(ADM2_EN=="Sidama") %>% st_transform(4326)


sidama_vect <- vect(shp)


rain  <- rast("agg_terraclimate_ppt_1958_CurrentYear_GLOBE.nc")
temax  <- rast("agg_terraclimate_tmax_1958_CurrentYear_GLOBE.nc")
temin <- rast("agg_terraclimate_tmin_1958_CurrentYear_GLOBE.nc")
ndvi  <- rast("ndvi_2025_2026_01_05_12_54_26.tif")
temean <- (temax + temin)/2 

library(elevatr)
dem <- get_elev_raster(
  locations = shp,
  z = 10,              # zoom level (8–10 recommended)
  clip = "locations"
)

rain_sidama <- crop(rain, sidama_vect)
rain_sidama <- mask(rain_sidama, sidama_vect)
time(rain_sidama)

years <- format(time(rain_sidama), "%Y")

annual_rain <- tapp(rain_sidama, index = years, sum, na.rm = TRUE)

mean_annual <- app(annual_rain, mean, na.rm = TRUE)
plot(mean_annual)
tmax_sidama <- crop(temax, sidama_vect)
tmax_sidama <- mask(tmax_sidama, sidama_vect)
tmax_mean_sidama <- app(tmax_sidama, mean, na.rm = TRUE)

plot(tmax_mean_sidama)
tmin_sidama <- crop(temin, sidama_vect)
tmin_sidama <- mask(tmin_sidama, sidama_vect)
tmin_mean_sidama <- app(tmin_sidama, mean, na.rm = TRUE)

plot(tmin_mean_sidama)
tmean_sidama <- (tmax_mean_sidama + tmin_mean_sidama) / 2




global(mean_annual, "isNA")
global(tmean_sidama, "isNA")
global(dem, "isNA")
global(ndvi, "isNA")

crs(mean_annual)
crs(tmean_sidama)
crs(dem)
crs(ndvi)

ext(temp)
ext(rain)
ext(ndvi)
ext(dem)

ndvi_sidamo <- crop(ndvi, sidama_vect)
ndvi_sidama <- mask(ndvi_sidamo, sidama_vect)

rain <- project(mean_annual, dem)
temp  <- project(tmean_sidama, dem)
ndvi <- project(ndvi, dem)

dem  <- rast(dem)

temp <- resample(tmean_sidama, ndvi, method="bilinear")
dem  <- resample(dem, ndvi, method="bilinear")
rain <- resample(mean_annual, ndvi, method="bilinear") 

plot(temp)
plot(dem)
plot(ndvi_sidama)


rasters <- list(rain, temp, dem, ndvi_sidama)

 
rain <- rasters[[1]]
temp <- rasters[[2]]
dem  <- rasters[[3]]
ndvi <- rasters[[4]]


#plot(rasters)
temp_s <- classify(temp, rcl = matrix(c(
  -Inf, 15, 0,
  15, 18, 0.5,
  18, 22, 1,
  22, 24, 0.5,
  24, Inf, 0
), ncol=3, byrow=TRUE))


plot(temp_s)

rain_s <- classify(rain, rcl = matrix(c(
  -Inf, 1200, 0,
  1200, 1400, 0.5,
  1400, 2000, 1,
  2000, 2200, 0.5,
  2200, Inf, 0
), ncol=3, byrow=TRUE))

plot(rain_s)
dem_s <- classify(dem, rcl = matrix(c(
  -Inf, 1200, 0,
  1200, 1500, 0.5,
  1500, 2200, 1,
  2200, 2500, 0.5,
  2500, Inf, 0
), ncol=3, byrow=TRUE))

plot(dem_s)
ndvi_s <- classify(ndvi, rcl = matrix(c(
  -Inf, 0.4, 0,
  0.4, 0.6, 0.5,
  0.6, Inf, 1
), ncol=3, byrow=TRUE))

ndvi_s <- crop(ndvi_s, sidama_vect)
ndvi_s <- mask(ndvi_s, sidama_vect)
plot(ndvi_s)


rain_s  <- crop(rain_s, ndvi_s)
temp_s  <- crop(temp_s, ndvi_s)
dem_s   <- crop(dem_s, ndvi_s)



coffee_suitability <-
                 0.35 * temp_s+
                 0.30 * rain_s +
                 0.20 * dem_s  +
                 0.15 * ndvi_s


coffee_class <- classify(coffee_suitability,
                         matrix(c(
                           0.0, 0.3, 1,   # Unsuitable
                           0.3, 0.6, 2,   # Moderately suitable
                           0.6, 1.0, 3    # Highly suitable
                         ), ncol=3, byrow=TRUE)
)



coffee_df <- as.data.frame(coffee_class, xy = TRUE, na.rm = TRUE)
colnames(coffee_df) <- c("x", "y", "class")


coffee_df$class <- factor(
  coffee_df$class,
  levels = c(1, 2, 3),
  labels = c("Unsuitable", "Moderately suitable", "Highly suitable")
)

suit_colors <- c(
  "Unsuitable" = "#d73027",
  "Moderately suitable" = "#fee08b",
  "Highly suitable" = "#1a9850"
)

library(ggplot2)
library(ggspatial)

coffe_plot <- ggplot() +
  geom_raster(data = coffee_df,
              aes(x = x, y = y, fill = class)) +
  scale_fill_manual(
    values = suit_colors,
    name = "Coffee suitability"
  ) +
  coord_equal() +
  labs(
    title = "Land Suitability for Coffee in Sidama Region",
    subtitle = "Based on rainfall, temperature, elevation, and NDVI",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_nautical)+
  theme(axis.text.x = element_text(face="bold", color= "black", 
                                   size=12),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=12))+
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"))+ 
  labs(caption = "\u00A9 2025 Markos Ware")+
  theme(panel.grid.major = element_line(
    color = "black", linetype = 2,
    linewidth = 0.5), panel.grid.minor = element_blank(),
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA, color = "black",
                                    linewidth = 1))+
  theme(
    legend.position = c(0.8, 0.8)) 


coffe_plot

ggsave(
  filename = "Coffee_suitability.png",
  plot = coffe_plot,        # specify the ggplot object
  width = 8,       # in inches
  height = 7,
  dpi = 300
)
