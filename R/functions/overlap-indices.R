# Overlap index functions
# Schoeners
schoeners_overlapspfn <- function(s1, s2) {
  p_s1 <- s1/sum(s1, na.rm = T)
  p_s2 <- s2/sum(s2, na.rm = T)
  1 - 0.5*sum(abs(p_s1 - p_s2))
}

# Calculate biomass overlap
# Local index of collocation
loc_collocspfn <- function(prey, pred) {
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  (p_prey*p_pred)/(sqrt(sum(p_prey^2, na.rm = T))* sqrt(sum(p_pred^2, na.rm = T)))
}

loc_collocspfn_tot <- function(prey, pred) {
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  sum((p_prey*p_pred)/(sqrt(sum(p_prey^2, na.rm = T))* sqrt(sum(p_pred^2, na.rm = T))))
}

# Asymmetrical alpha # ML: think this is wrong! should be a squared term in the numerator??
asymmalpha_overlapspfn <-function(prey, pred){
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  (p_pred*p_prey)/sqrt(sum(p_prey^2, na.rm = T))
}

asymmalpha_overlapspfn_tot <-function(prey, pred){
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  sum((p_pred*p_prey)/sqrt(sum(p_prey^2, na.rm = T)))
}

# Biomass weighted overlap (scaled)
thorson_overlapspfn <- function(prey, pred) {
  (prey/max(prey, na.rm = T)) * (pred/max(pred, na.rm = T))/sum(prey/max(prey, na.rm = T), na.rm = T)
}

## Hurlbert's overlap
hurlbert_overlapspfn <- function(prey, pred, area) {
  total_area <- sum(area, na.rm = T)
  p_prey <- prey/sum(prey, na.rm = T)
  p_pred <- pred/sum(pred, na.rm = T)
  (p_pred*p_prey)/(area/total_area)
}
