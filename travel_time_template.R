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

input_file_path <- "C:/Users/ianwe/Downloads/github/travel-time-analyses/scrape-hero/inputs/Hyatt_Partners_USA.csv"

output_file_path_for_isochrone <- "travel-time-analyses/scrape-hero/.shp"
output_file_path_for_geocode <- "travel-time-analyses/scrape-hero/.shp"

# Read in data ----

locations <- read.csv(input_file_path)

# Clean data ----

locations <- locations %>%
  mutate(full_address = paste0(Street, ", ", City, ", ", State, " ", Zip_Code))

addresses <- locations$full_address

coords_list <- map2(locations$Longitude, locations$Latitude, ~ c(.x, .y))

# Generate templates ----

# Isochrone for the first coordinate pair
isochrone_final <- mb_isochrone(
  coords_list[[1]],
  profile = "driving",
  time = c(10, 15, 20),
  depart_at = "2025-10-11T12:00",
  access_token = token,
  geometry = "polygon",
  output = "sf",
  keep_color_cols = FALSE
) %>%
  mutate(
    address = locations$Street[1],
    city = locations$City[1],
    county = locations$County[1],
    state = locations$State[1],
    zip_code = locations$Zip_Code[1],
    country = locations$Country[1],
    lat = locations$Latitude[1],
    lon = locations$Longitude[1],
    naics = locations$NAICS.2[1]
  )

# Optionally remove first 3 rows if needed
isochrone_final <- isochrone_final[-1, ]

# Reverse geocode for first location
geocode_final <- mb_reverse_geocode(
  coords_list[[1]],
  output = "sf",
  access_token = token
) %>%
  mutate(
    address = locations$Street[1],
    city = locations$City[1],
    county = locations$County[1],
    state = locations$State[1],
    zip_code = locations$Zip_Code[1],
    country = locations$Country[1],
    lat = locations$Latitude[1],
    lon = locations$Longitude[1],
    naics = locations$NAICS.2[1]
  )

# Optional: remove first row if Mapbox returns multiple
geocode_final <- geocode_final[-1, ]

# Generate travel time for all locations ----

for (i in seq_along(coords_list)) {
  
  # Reverse geocode
  geocode <- mb_reverse_geocode(
    coords_list[[i]],
    output = "sf",
    access_token = token
  ) %>%
    mutate(
      address = locations$Street[i],
      city = locations$City[i],
      county = locations$County[i],
      state = locations$State[i],
      zip_code = locations$Zip_Code[i],
      country = locations$Country[i],
      lat = locations$Latitude[i],
      lon = locations$Longitude[i],
      naics = locations$NAICS.2[i]
    )
  
  geocode_final <- rbind(geocode_final, geocode)
  
  # Isochrone
  isochrone <- mb_isochrone(
    coords_list[[i]],
    profile = "driving",
    time = c(10, 15, 20),
    depart_at = "2025-10-11T12:00",
    access_token = token,
    geometry = "polygon",
    output = "sf",
    keep_color_cols = FALSE
  ) %>%
    mutate(
      address = locations$Street[i],
      city = locations$City[i],
      county = locations$County[i],
      state = locations$State[i],
      zip_code = locations$Zip_Code[i],
      country = locations$Country[i],
      lat = locations$Latitude[i],
      lon = locations$Longitude[i],
      naics = locations$NAICS.2[i]
    )
  
  isochrone_final <- rbind(isochrone_final, isochrone)
  
  # Print progress
  print(paste("Processed location:", locations$Street[i]))
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