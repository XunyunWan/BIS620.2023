#' Map Visualization of Study Count by Country
#'
#' @description Produces a choropleth world map indicating the number of studies conducted in each country.
#' @param studies A database table (tibble) containing the 'country' column with study locations.
#' @return Map plot object.
plot_countries_map = function(studies) {
  ct = studies|>
    select(country) |>
    group_by(country) |>
    summarize(n=n())

  world <- ne_countries(scale = "medium", returnclass = "sf")

  merged_data <- merge(world, ct, by.x = "name", by.y = "country", all.x = TRUE)

  ggplot() +
    geom_sf(data = merged_data, aes(fill = n)) +
    scale_fill_viridis_c() +
    labs(title = "World Map of Counts by Country") +
    theme_minimal()
}
