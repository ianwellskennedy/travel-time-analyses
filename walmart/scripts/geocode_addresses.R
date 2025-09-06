# Packages ----

# Set the package names to read in
packages <- c("tidyverse", "openxlsx", "mapboxapi", "mapgl", "arcgisbinding", "sf")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Remove unneeded variables
rm(packages, installed_packages)

# File paths ----

token <- "pk.eyJ1Ijoicm9zczJpYW4iLCJhIjoiY21mN2dvbzI2MDR5ajJqb213OXZ4cXdmNSJ9.uJLcOdrlHJcNOMPJoT-BqQ"

input_file_path <- "C:/Users/ianwe/Downloads/walmart_locations.xlsx"

output_file_path_for_isochrome <- "walmart/output_shape_files/walmart_isochrome.shp"
output_file_path_for_geocode <- "walmart/output_shape_files/walmart_geocode.shp"

# Read in data ----

walmart_locations <- read.xlsx(input_file_path)

# Clean data ----

walmart_locations <- walmart_locations %>%
  mutate(full_address = paste0(address, ", ", city, ", ", state, " ", zip)) %>%
  select(full_address)

walmart_locations <- walmart_locations$full_address


# Run for loop ----

isochrome_final <- mb_isochrone(
  walmart_locations[1],
  profile = "driving",
  time = c(10, 15, 20),
  depart_at = "2025-09-06T12:00",
  access_token = token,
  geometry = "polygon",
  output = "sf",
  keep_color_cols = FALSE,
) %>%
  mutate(full_address = walmart_locations[1]) 

isochrome_final <- isochrome_final[-c(1:3),]

geocode_final <- mb_geocode(walmart_locations[1], output = "sf", access_token = token) %>%
  mutate(full_address = walmart_locations[1])

geocode_final <- geocode_final[-1,]

for (location in walmart_locations){

  geocode <- mb_geocode(location, output = "sf", access_token = token) %>%
    mutate(full_address = location)

  geocode_final <- rbind(geocode_final, geocode)
  
  isochrome <- mb_isochrone(
    geocode,
    profile = "driving",
    time = c(10, 15, 20),
    depart_at = "2025-09-06T12:00",
    access_token = token,
    geometry = "polygon",
    output = "sf",
    keep_color_cols = FALSE,
  ) %>%
    mutate(full_address = location)
  
  isochrome_final <- rbind(isochrome_final, isochrome)
  
  print(location)
}

# Plot the map ----

# map <- mapboxgl(bounds = isochrome_final, access_token = token, style = mapbox_style("satellite-streets")) |>
#   add_fill_layer(
#     "noon",
#     source = isochrome_final,
#     fill_color = match_expr(
#       column = "time",
#       values = c(10, 15, 20),
#       stops = c("red", "blue", "green")
#     ),
#     fill_opacity = 0.75
#   ) 
# 
# map

# Finalize spatial files ----

isochrome_final <- st_as_sf(isochrome_final)
geocode_final <- st_as_sf(geocode_final)


# Output files ----

arc.check_product()

arc.write(isochrome_final, path = output_file_path_for_isochrome, validate = T, overwrite = T)
arc.write(geocode_final, path = output_file_path_for_geocode, validate = T, overwrite = T)
