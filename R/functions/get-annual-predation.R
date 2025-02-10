get_annual_predation <- function(.data, threshold = 1, prey_name, area = 9) {
  .data |>
    pivot_longer(cols = starts_with("V"), names_to = "sim") |>
    dplyr::select(X, Y, year, value, sim) |>
    mutate(value = exp(value)) |>
    left_join(
      pred_grid_density_sub |>
        dplyr::select(-saduria, -biomass_spr, -biomass_her),
      by = c("X", "Y", "year", "sim")
    ) |>
    drop_na(sim_density) |> # These are dropped in the density models and therefore get NA after left_joining
    filter(value < threshold) |>
    # Calculate predation by grid cell
    mutate(pred = sim_density * area * value) |>
    # Summarise predation across all grid cells, by year and sim
    summarise(
      pred = sum(pred),
      .by = c(year, sim)
    ) |>
    left_join(pred_dens, by = c("year", "sim")) |>
    mutate(cap = pred / cod_biomass) |>
    # Summarise sim + year specific predation by year (to get variation across sims)
    summarise(
      pred_median = quantile(pred, probs = 0.5),
      pred_lwr = quantile(pred, probs = 0.1),
      pred_upr = quantile(pred, probs = 0.9),
      cap_median = quantile(cap, probs = 0.5),
      cap_lwr = quantile(cap, probs = 0.1),
      cap_upr = quantile(cap, probs = 0.9),
      .by = year
    ) |>
    mutate(species = prey_name)
}
