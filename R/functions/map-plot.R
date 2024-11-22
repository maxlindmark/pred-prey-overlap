library(sf)
# Packages not on CRAN
# devtools::install_github("seananderson/ggsidekick") # not on CRAN 
library(ggsidekick)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
theme_set(theme_sleek())

sf::sf_use_s2(FALSE)

# Specify map ranges
ymin = 52; ymax = 60.5; xmin = 10; xmax = 24

map_data <- rnaturalearth::ne_countries(
  scale = "large",
  returnclass = "sf", continent = "europe")

# Crop the polygon for plotting and efficiency:
# st_bbox(map_data) # find the rough coordinates
swe_coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))))

# Transform our map into UTM 33 coordinates, which is the equal-area projection we fit in:
utm_zone33 <- 32633
swe_coast_proj <- sf::st_transform(swe_coast, crs = utm_zone33)

# Define plotting theme for facet_wrap map with years
theme_facet_map <- function(base_size = 11, base_family = "") {
  theme_sleek(base_size = base_size, base_family = "") +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.direction = "horizontal",
      legend.margin = margin(1, 1, 1, 1),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.spacing.x = unit(0.1, 'cm'),
      legend.position.inside = c(0.82, 0.04),
      legend.key.width = unit(1, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 8)
    )
}

# Make default base map plot
xmin2 <- 357200
xmax2 <- 918748
xrange <- xmax2 - xmin2

ymin2 <- 5980000
ymax2 <- 6501300
yrange <- ymax2 - ymin2

plot_map <- 
  ggplot(swe_coast_proj) + 
  xlim(xmin2, xmax2) +
  ylim(ymin2, ymax2) +
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(linewidth = 0.8, color = "gray30") + 
  theme_sleek() +
  guides(colour = guide_colorbar(title.position = "top", title.hjust = 0.5),
         fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  NULL

plot_map_fc <- 
  ggplot(swe_coast_proj) + 
  xlim(xmin2, xmax2) +
  ylim(ymin2, ymax2) +
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(size = 0.3, color = "gray30") + 
  facet_wrap(~year) +
  theme_facet_map() +
  guides(colour = guide_colorbar(position = "inside", title.position = "top", title.hjust = 0.5),
         fill = guide_colorbar(position = "inside", title.position = "top", title.hjust = 0.5)) +
  NULL

