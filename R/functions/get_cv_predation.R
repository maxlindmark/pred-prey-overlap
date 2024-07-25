get_cv_predation <- function(.data, threshold = 1, power, prey_name, years) {
  
  .data |> 
    filter(year %in% years) |> 
    pivot_longer(cols = starts_with("V"), names_to = "sim") |> 
    dplyr::select(X, Y, year, value, sim) |> 
    mutate(value = (exp(value))^power) |> 
    left_join(pred_grid_density_sub |> dplyr::select(-saduria, -biomass_spr, -biomass_her),
              by = c("X", "Y", "year", "sim")) |> 
    drop_na(sim_density) |> # These are dropped in the density models and therefore get NA after left_joining
    filter(value < threshold) |> 
    summarise(mean = mean(value),
              sd = sd(value),
              .by = c(year, X, Y)) |>
    mutate(cv = sd / mean,
           species = prey_name)
}
