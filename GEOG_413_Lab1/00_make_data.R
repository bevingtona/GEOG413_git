# GEOG 413 Lab 1 — Prep data

library(bcdata)
library(sf)
library(dplyr)

getwd()
dir.create("data", showWarnings = FALSE)
gpkg <- "data/chilako.gpkg"
if (file.exists(gpkg)) unlink(gpkg)

keep_polys <- function(x) {
  x |> st_make_valid() |> st_collection_extract("POLYGON") |> st_cast("MULTIPOLYGON")
}
keep_lines <- function(x) {
  x |>
    filter(st_geometry_type(geometry) %in% c("LINESTRING", "MULTILINESTRING")) |>
    st_cast("MULTILINESTRING")
}

# 1. Watershed ----------------------------------------------------------

message("Downloading watershed polygon...")
ws <- bcdc_query_geodata("freshwater-atlas-named-watersheds") |>
  filter(GNIS_NAME == "Chilako River") |>
  collect() |>
  select(GNIS_NAME, AREA_HA)
st_write(ws, gpkg, layer = "watershed", quiet = TRUE, delete_layer = T)

# 2. Streams, order >= 3 ------------------------------------------------
message("Downloading stream network...")
streams <- bcdc_query_geodata("freshwater-atlas-stream-network") |>
  filter(INTERSECTS(ws), STREAM_ORDER >= 3) |>
  collect() |>
  select(GNIS_NAME, STREAM_ORDER, BLUE_LINE_KEY) |>
  st_zm() |>
  st_intersection(st_geometry(ws)) |>
  keep_lines()
st_write(streams, gpkg, layer = "streams", quiet = TRUE, delete_layer = T)

# 3. Lakes --------------------------------------------------------------
message("Downloading lakes")
lakes <- bcdc_query_geodata("freshwater-atlas-lakes") |>
  filter(INTERSECTS(ws)) |>
  collect() |>
  select(GNIS_NAME_1, AREA_HA) |>
  rename(LAKE_NAME = GNIS_NAME_1) |>
  st_intersection(st_geometry(ws)) |>
  keep_polys()
st_write(lakes, gpkg, layer = "lakes", quiet = TRUE, delete_layer = T)

# 4. Cutblocks ----------------------------------------------------------
message("Downloading stream cutblocks...")
cutblocks <- bcdc_query_geodata("harvested-areas-of-bc-consolidated-cutblocks-") |>
  filter(INTERSECTS(ws)) |>
  select(HARVEST_START_DATE) |>
  collect() |>
  select(HARVEST_START_DATE) |>
  mutate(HARVEST_YEAR = as.integer(format(HARVEST_START_DATE, "%Y"))) |>
  st_intersection(st_geometry(ws)) |>
  keep_polys()
st_write(cutblocks, gpkg, layer = "cutblocks", quiet = TRUE, delete_layer = T)

# 5. Fires --------------------------------------------------------------
message("Downloading historical cutblocks...")
fires <- bcdc_query_geodata("bc-wildfire-fire-perimeters-historical") |>
  filter(INTERSECTS(ws)) |>
  select(FIRE_NUMBER, FIRE_YEAR, FIRE_CAUSE) |> 
  collect() |>
  select(FIRE_NUMBER, FIRE_YEAR, FIRE_CAUSE) |>
  st_intersection(st_geometry(ws)) |>
  keep_polys()
st_write(fires, gpkg, layer = "fires", quiet = TRUE, delete_layer = T)

message(paste0("Download complete (file location: ", getwd(), gpkg, ")"))
st_layers(gpkg)
