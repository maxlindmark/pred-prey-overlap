get_annual_predation <- function(.data, threshold, power, prey_name) {
  
  .data |> 
    bind_cols(pred_grid) |> 
    pivot_longer(cols = starts_with("V"), names_to = "sim") |> 
    dplyr::select(X, Y, year, value, sim) |> 
    mutate(value = (exp(value))^power) |> 
    left_join(pred_grid_density |> dplyr::select(-saduria, -biomass_spr, -biomass_her),
              by = c("X", "Y", "year", "sim")) |> 
    drop_na(sim_density) |> # These are dropped in the density models and therefore get NA after left_joining
    filter(value < threshold) |> 
    # Calculate predation by grid cell
    mutate(pred = sim_density*area*value) |> 
    # Summarise predation across all grid cells, by year and sim
    summarise(pred = sum(pred),
              .by = c(year, sim)) |>
    left_join(pred_dens, by = c("year", "sim")) |>
    mutate(cap = pred / cod_biomass) |>
    # Summarise sim + year specific predation by year (to get variation across sims)
    summarise(pred_mean = mean(pred),
              pred_sd = sd(pred),
              cap_mean = mean(cap),
              cap_sd = sd(cap),
              .by = year) |>
    mutate(species = prey_name)
  
}
