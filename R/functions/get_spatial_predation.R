get_spatial_predation <- function(.data, years, threshold = 1, power, prey_name, area = 9) {
  
  .data |> 
    filter(year %in% years) |> 
    pivot_longer(cols = starts_with("V"), names_to = "sim") |> 
    dplyr::select(X, Y, year, value, sim) |> 
    mutate(value = (exp(value))^power) |> 
    left_join(pred_grid_density_sub |> dplyr::select(-saduria, -biomass_spr, -biomass_her),
              by = c("X", "Y", "year", "sim")) |> 
    drop_na(sim_density) |> # These are dropped in the density models and therefore get NA after left_joining
    filter(value < threshold) |> 
    # Calculate predation by grid cell
    mutate(pred = sim_density*value) |> 
    # Summarise mean predation across sims per grid cell (and year)
    summarise(pred_mean = mean(pred),
              pred_sd = sd(pred),
              .by = c(year, X, Y)) |>
    mutate(cv = pred_sd / pred_mean,
           species = prey_name)
  
}
