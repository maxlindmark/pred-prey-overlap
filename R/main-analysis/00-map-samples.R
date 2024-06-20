library(tidyverse)
library(devtools)
library(tidylog)
library(viridis)
library(ggspatial)
library(metR)

devtools::source_url("https://raw.githubusercontent.com/maxlindmark/pred-prey-overlap/main/R/functions/map-plot.R")

# Set path
home <- here::here()

## Read cleaned data

# We want to scale oxygen, temperature, depth and salinity using the means of the catch data
d_sum <- read_csv(paste0(home, "/data/clean/stomachs.csv")) |>
  summarise(n = n(), .by = c(year, month, day, X, Y))

# For batyhymetry
pred_grid <- 
  bind_rows(read_csv(paste0(home, "/data/clean/pred_grid_(1_2).csv")),
            read_csv(paste0(home, "/data/clean/pred_grid_(2_2).csv"))) |> 
  filter(year == 1999) |> 
  mutate(depth = round(depth)) |> 
  distinct(X, Y, depth) |> 
  summarise(depth = mean(depth), .by = c(X, Y))

plot_map +
  geom_sf(color = "gray80") + 
  geom_contour(data = pred_grid, aes(X*1000, Y*1000, z = depth, 
                                     colour = after_stat(level)),
               bins = 13, alpha = 0.6, linewidth = 0.3) +
  geom_point(data = d_sum, aes(X*1000, Y*1000, fill = year, size = n),
             alpha = 0.6, color = "gray30", shape = 21, stroke = 0.1) + 
  scale_color_viridis(option = "cividis", direction = -1) +
  scale_fill_viridis(option = "mako", direction = -1) +
  scale_radius(range = c(1, 5), name = "n stomachs") +
  theme_sleek(base_size = 9) + 
  #xlim(xmin2*0.999, xmax2) +
  ylim(ymin2*0.996, ymax2) + 
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, position = "inside"),
         size = guide_legend(title.position = "top", title.hjust = 0.5, ncol = 2, position = "inside")) +
  theme(legend.position = c(0.77, 0.09),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm")) +
  labs(shape = "Quarter", fill = "Year", color = "Depth (m)") + 
  annotate("text", label = "Sweden", x = xmin2 + 0.15*xrange, y = ymin2 + 0.75*yrange, color = "gray50", size = 2.6) +
  annotate("text", label = "Germany", x = xmin2 + 0.01*xrange, y = ymin2 + 0.01*yrange, color = "gray50", size = 2.6) +
  annotate("text", label = "Poland", x = xmin2 + 0.5*xrange, y = ymin2 + 0.005*yrange, color = "gray50", size = 2.6) +
  annotate("text", label = "Russia", x = xmin2 + 0.95*xrange, y = ymin2 + 0.18*yrange, color = "gray50", size = 2.6) +
  annotate("text", label = "Lithuania", x = xmin2 + 0.99*xrange, y = ymin2 + 0.44*yrange, color = "gray50", size = 2.6) +
  annotate("text", label = "Latvia", x = xmin2 + 0.99*xrange, y = ymin2 + 0.65*yrange, color = "gray50", size = 2.6) + 
  annotation_scale(bar_cols = c("grey30", "white"), height = unit(0.1, "cm")) + 
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(0.85, "cm"),
                         width = unit(0.85, "cm"), pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering(
                           fill = c("grey40", "white"),
                           line_col = "grey20"))

ggsave(paste0(home, "/figures/map_diet.pdf"), width = 14, height = 14, units = "cm")
