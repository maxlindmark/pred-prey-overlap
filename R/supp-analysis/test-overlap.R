# Biomass weighted overlap (scaled)
thorson_overlapspfn <- function(prey, pred) {
  (prey / max(prey, na.rm = T)) * (pred / max(pred, na.rm = T)) / sum(prey / max(prey, na.rm = T), na.rm = T)
}

loc_collocspfn_tot <- function(prey, pred) {
  p_prey <- prey / sum(prey, na.rm = T)
  p_pred <- pred / sum(pred, na.rm = T)
  sum((p_prey * p_pred) / (sqrt(sum(p_prey^2, na.rm = T)) * sqrt(sum(p_pred^2, na.rm = T))))
}

# Cod aggregating
d1 <- tibble(pred = c(0, 0, 0, 0, 1),
             prey = c(1, 1, 1, 1, 1),
             cell = c(1, 2, 3, 4, 5))

# Evenly spread cod (perfect overlap)
d2 <- tibble(pred = c(0.2, 0.2, 0.2, 0.2, 0.2),
             prey = c(1, 1, 1, 1, 1),
             cell = c(1, 2, 3, 4, 5))

# Cod and prey aggregating
d3 <- tibble(pred = c(0, 0, 0, 0, 1),
             prey = c(0, 0, 0, 0, 5),
             cell = c(1, 2, 3, 4, 5))

d1 |> summarise(ovr = loc_collocspfn_tot(pred = pred, prey = prey))
d2 |> summarise(ovr = loc_collocspfn_tot(pred = pred, prey = prey))
d3 |> summarise(ovr = loc_collocspfn_tot(pred = pred, prey = prey))

d1 |> summarise(ovr = sum(thorson_overlapspfn(pred = pred, prey = prey)))
d2 |> summarise(ovr = sum(thorson_overlapspfn(pred = pred, prey = prey)))
