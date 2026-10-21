# Export the Whittaker biome polygons (plotbiomes::Whittaker_biomes: mean
# annual temperature in degrees C vs annual precipitation in cm, after Ricklefs
# 2008) to analysis-website/static/whittaker_biomes.json, for the website's
# climate-space site map.
#
# Usage: Rscript scripts/export-whittaker-biomes.R

stopifnot(requireNamespace("plotbiomes"), requireNamespace("jsonlite"))
biomes <- plotbiomes::Whittaker_biomes
out <- lapply(split(biomes, biomes$biome), function(b) {
  list(biome = as.character(b$biome[1]), temp_c = b$temp_c, precp_cm = b$precp_cm)
})
jsonlite::write_json(unname(out), file.path("analysis-website", "static", "whittaker_biomes.json"), digits = 4, auto_unbox = TRUE)
