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

input_file_path <- "wegmans/wegmans_locations.xlsx"

output_file_path_for_isochrone <- "wegmans/wegmans_isochrone.shp"
output_file_path_for_geocode <- "wegmans/wegmans_geocode.shp"

# Read in data ----

locations <- read.xlsx(input_file_path)

# Clean data ----

locations <- locations %>%
  mutate(full_address = paste0(address, ", ", city, ", ", state, " ", zip))

addresses <- locations$full_address

# Generate templates ----

# Isochrone for the first coordinate pair
isochrone_final <- mb_isochrone(
  addresses[1],
  profile = "driving",
  time = c(10, 15, 20),
  depart_at = "2025-11-01T12:00",
  access_token = token,
  geometry = "polygon",
  output = "sf",
  keep_color_cols = FALSE
) %>%
  mutate(
    address = locations$full_address[1],
    city = locations$city[1],
    state = locations$state[1],
    zip_code = locations$zip[1]
  )

# Optionally remove first 3 rows if needed
isochrone_final <- isochrone_final[-c(1:3), ]

# Reverse geocode for first location
geocode_final <- mb_geocode(
  addresses[1],
  output = "sf",
  access_token = token
) %>%
  mutate(
    address = locations$full_address[1],
    city = locations$city[1],
    state = locations$state[1],
    zip_code = locations$zip[1]
  ) 

# Optional: remove first row if Mapbox returns multiple
geocode_final <- geocode_final[-1, ]

# Generate travel time for all locations ----

for (i in seq_along(addresses)) {
  
  # Reverse geocode
  geocode <- mb_geocode(
    addresses[i],
    output = "sf",
    access_token = token
  ) %>%
    mutate(
      address = locations$full_address[i],
      city = locations$city[i],
      state = locations$state[i],
      zip_code = locations$zip[i],
    )
  
  geocode_final <- rbind(geocode_final, geocode)
  
  # Isochrone
  isochrone <- mb_isochrone(
    addresses[i],
    profile = "driving",
    time = c(10, 15, 20),
    depart_at = "2025-11-01T12:00",
    access_token = token,
    geometry = "polygon",
    output = "sf",
    keep_color_cols = FALSE
  ) %>%
    mutate(
      address = locations$full_address[i],
      city = locations$city[i],
      state = locations$state[i],
      zip_code = locations$zip[i]
    )
  
  isochrone_final <- rbind(isochrone_final, isochrone)
  
  # Print progress
  print(paste("Processed location:", locations$full_address[i]))
}


# Plot the map ----

mapboxgl(bounds = isochrone_final, access_token = token, style = mapbox_style("satellite-streets")) %>%
  add_fill_layer(
    "noon",
    source = isochrone_final,
    fill_color = match_expr(
      column = "time",
      values = c(10, 15, 20),
      stops = c("green", "yellow", "red")
    ),
    fill_opacity = 0.75
  )


# Finalize spatial files ----

isochrone_final <- st_as_sf(isochrone_final)
geocode_final <- st_as_sf(geocode_final)

# Output files ----

arc.check_product()

arc.write(isochrone_final, path = output_file_path_for_isochrone, validate = T, overwrite = T)
arc.write(geocode_final, path = output_file_path_for_geocode, validate = T, overwrite = T)
