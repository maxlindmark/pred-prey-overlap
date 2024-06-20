library(tidyverse)
library(patchwork)
library(devtools)
library(tidylog)
library(viridis)
library(ggspatial)
library(metR)
library(ggtext)

devtools::source_url("https://raw.githubusercontent.com/maxlindmark/pred-prey-overlap/main/R/functions/map-plot.R")

# Set path
home <- here::here()

## Read cleaned data
pred_grid <- 
  bind_rows(read_csv(paste0(home, "/data/clean/pred_grid_(1_2).csv")),
            read_csv(paste0(home, "/data/clean/pred_grid_(2_2).csv"))) |> 
  summarise(Sprat = mean(biomass_spr),
            Herring = mean(biomass_her),
            Saduria = mean(saduria),
            .by = c(X, Y)) |>
  pivot_longer(c(-X, -Y), names_to = "Prey")

plot_map2 <- plot_map + 
  facet_wrap(~Prey) + 
  theme(legend.position = c(0.14, 0.68),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.25, "cm"),
        legend.title = element_text(size = 8)) +
  guides(fill = guide_colorbar(title.position = "top")) 

annotations <- data.frame(
  xpos = -Inf,
  ypos =  Inf,
  hjust = -0.3,
  vjust = 1.5)

her <- plot_map2 +
  geom_sf(color = "gray80") + 
  geom_raster(data = pred_grid |> filter(Prey == "Herring"),
              aes(X*1000, Y*1000, fill = value/1000)) + 
  scale_fill_viridis(option = "viridis", name = "Biomass\n(t tonnes)") + 
  theme(axis.title.x = element_blank()) + 
  geom_text(data = annotations |> mutate(annotateText = c("(a)")), size = 3.2,
            aes(x = xpos, y = ypos, hjust = hjust, vjust = vjust, label = annotateText))

# Plot map doesn't play nicely with element_markdown because of the guides... 
sad <- ggplot(swe_coast_proj) + 
  xlim(xmin2, xmax2) +
  ylim(ymin2, ymax2) +
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(size = 0.3, color = "gray80") + 
  theme_sleek() +
  scale_fill_viridis(option = "viridis") + 
  geom_sf(color = "gray80") + 
  facet_wrap(~Prey) + 
  geom_raster(data = pred_grid |> filter(Prey == "Saduria"),
              aes(X*1000, Y*1000, fill = value)) + 
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(fill = 'Biomass density<br>(mg/m<sup>2</sup>)') +
  theme(legend.title = element_markdown(size = 8),
        legend.position = c(0.22, 0.68),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.25, "cm"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) + 
  geom_text(data = annotations |> mutate(annotateText = c("(b)")), size = 3.2,
            aes(x = xpos, y = ypos, hjust = hjust, vjust = vjust, label = annotateText))

spr <- plot_map2 +
  geom_sf(color = "gray80") + 
  geom_raster(data = pred_grid |> filter(Prey == "Sprat"),
              aes(X*1000, Y*1000, fill = value/1000)) + 
  scale_fill_viridis(option = "viridis", name = "Biomass\n(t tonnes)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) + 
  geom_text(data = annotations |> mutate(annotateText = c("(c)")), size = 3.2,
            aes(x = xpos, y = ypos, hjust = hjust, vjust = vjust, label = annotateText))

her + sad + spr

ggsave(paste0(home, "/figures/supp/map_prey.pdf"), width = 19, height = 7.5, units = "cm")

