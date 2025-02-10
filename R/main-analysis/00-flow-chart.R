# flow-chart

library(RColorBrewer)
library(tidyverse)
library(igraph)
library(ggtext)
library(showtext)
library(rcartocolor)
library(ggarrow)

home <- here::here()

goldilocks <- tibble(
  from = c(
    "Q1: Changes in<br>diet?",
    "Spatiotemporal<br>diet models",
    "Predict<br>relative prey weight (*R*)<br>on spatial grid<br>[kg/kg]",
    "Predict<br>relative prey weight (*R*)<br>on spatial grid<br>[kg/kg]",
    "Q2: Changes in<br>predation intensity?",
    "Spatiotemporal<br>cod models",
    "Predict<br>cod density (*D*)<br>on spatial grid<br>[kg/km<sup>2</sup>]",
    "Predict<br>cod density (*D*)<br>on spatial grid<br>[kg/km<sup>2</sup>]",
    "Predict<br>cod density (*D*)<br>on spatial grid<br>[kg/km<sup>2</sup>]",
    "Q3: Changes in<br>spatial overlap?",
    "Spatiotemporal<br>prey data",
    "Spatiotemporal<br>predator-prey overlap<br>('local index<br>of collocation')",
    "Spatiotemporal trends in<br>population-level<br>predation intensity (*P<sub>p,i</sub>*)",
    "Spatiotemporal trends in<br>per capita<br>predation intensity (*P<sub>c,i</sub>*)"
  ),
  to = c(
    "Spatiotemporal<br>diet models",
    "Predict<br>relative prey weight (*R*)<br>on spatial grid<br>[kg/kg]",
    "Spatiotemporal trends in<br>population-level<br>predation intensity (*P<sub>p,i</sub>*)",
    "Spatiotemporal trends in<br>per capita<br>predation intensity (*P<sub>c,i</sub>*)",
    "Spatiotemporal<br>cod models",
    "Predict<br>cod density (*D*)<br>on spatial grid<br>[kg/km<sup>2</sup>]",
    "Spatiotemporal trends in<br>population-level<br>predation intensity (*P<sub>p,i</sub>*)",
    "Spatiotemporal trends in<br>per capita<br>predation intensity (*P<sub>c,i</sub>*)",
    "Spatiotemporal<br>predator-prey overlap<br>('local index<br>of collocation')",
    "Spatiotemporal<br>prey data",
    "Spatiotemporal<br>predator-prey overlap<br>('local index<br>of collocation')",
    "Q4: Correlation<br>between overlap<br>and predation",
    "Q4: Correlation<br>between overlap<br>and predation",
    "Q4: Correlation<br>between overlap<br>and predation"
  )
)


g <- graph_from_data_frame(goldilocks, directed = TRUE)
coords <- layout_as_tree(g)
colnames(coords) <- c("x", "y")
coords[9, ] <- c(1, 0)
coords[12, ] <- c(0, -1)

output_df <- as_tibble(coords) |>
  mutate(
    step = vertex_attr(g, "name"),
    label = gsub("\\d+$", "", step),
    type = factor(c(2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2))
  )

output_df |> as.data.frame()

plot_nodes <- output_df |>
  mutate(
    xmin = x + 0.45,
    xmax = x - 0.45,
    ymin = y - 0.3,
    ymax = y + 0.3
  )

plot_edges <- goldilocks |>
  mutate(id = row_number()) |>
  pivot_longer(
    cols = c("from", "to"),
    names_to = "s_e",
    values_to = "step"
  ) |>
  left_join(plot_nodes, by = "step") |>
  dplyr::select(-c(label, type, y, xmin, xmax)) |>
  mutate(y = ifelse(s_e == "from", ymin, ymax)) |>
  dplyr::select(-c(ymin, ymax))

ggplot(plot_nodes) +
  geom_rect(
    aes(
      xmin = xmin, ymin = ymin,
      xmax = xmax, ymax = ymax,
      fill = type
    ),
    color = NA, alpha = 0.4
  ) +
  geom_richtext(aes(x = x, y = y, label = label),
    label.colour = NA,
    color = "gray10", fill = NA
  ) +
  geom_arrow(
    data = plot_edges, aes(x = x, y = y, group = id),
    colour = "gray30", length = 7,
    arrow_head = arrow_head_wings(offset = 20, inset = 70)
  ) +
  theme_void() +
  guides(fill = "none", color = "none", alpha = "none") +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  scale_alpha_manual(values = c(0.2, 0.7))

ggsave(paste0(home, "/figures/flow.pdf"), width = 18, height = 18, units = "cm", device = cairo_pdf)
