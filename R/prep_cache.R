#!/usr/bin/env Rscript

# Script to preprocess and save data as RDS files for faster app loading
librarian::shelf(
  dplyr, glue, here, lubridate, markdown, purrr, readr, tbeptech/tbeptools, sf,
  StormR, terra, tibble, tidyr, units)
options(readr.show_col_types = F)
set.seed(42)

dir_cache    <- here("data/cache")
dir.create(dir_cache, showWarnings = F)

savevars_rds <- function(vars){
  for (var in vars)
    write_rds(get(var), glue("{dir_cache}/{var}.rds"))
}

# prism ----
message("processing prism ...")

dir_prism <- here("data/prism")
prism_csv <- here("data/prism.csv")

if (!dir.exists(dir_cache))
  dir.create(dir_cache, recursive = T)

d_prism_r    <- read_prism_rasters(dir_prism)
d_prism_z    <- read_csv(here("data/prism.csv"))
yrs_prism     <- range(year(d_prism_r$date))
now_prism     <- max(d_prism_r$date)

prism_zones   <- d_prism_z |>
  distinct(bay_segment) |>
  left_join(
    tbsegshed |>
      st_drop_geometry() |>
      select(long_name, bay_segment) |>
      bind_rows(
        tibble(
          bay_segment = "TB",
          long_name   = "Tampa Bay") ),
    by = "bay_segment") |>
  select(long_name, bay_segment) |>
  deframe()

savevars_rds(c(
  "d_prism_r", "d_prism_z", "yrs_prism", "now_prism", "prism_zones"))

# sst ----
message("processing sst ...")

sst_tif   <- here("data/sst/tb_sst.tif")
sst_csv   <- here("data/sst/tb_sst.csv")

sf_tb <- tbeptools::tbsegshed |>
  bind_rows(
    tbeptools::tbshed |>
      mutate(
        long_name   = "Tampa Bay",
        bay_segment = "TB") |>
      select(-Acres)) |>
  sf::st_make_valid()

r_sst     <- rast(sst_tif)
d_sst_r   <- tibble(
  lyr = names(r_sst)) |>
  separate(lyr, c("var", "date"), sep = "\\|", remove = F) |>
  mutate(
    date = as.Date(date))
yrs_sst   <- range(year(d_sst_r$date))
now_sst   <- max(d_sst_r$date)
d_sst_z   <- read_csv(sst_csv) |>
  mutate(
    date = as.Date(time))

sst_zones <- d_sst_z |>
  distinct(bay_segment) |>
  deframe()

savevars_rds(c(
  "sf_tb", "r_sst", "d_sst_r", "yrs_sst", "now_sst", "d_sst_z", "sst_zones"))

# sealevel ----
message("processing sealevel ...")
d_sl <- read_csv(here("data/slr/monthly_sea_levels.csv"))
sl_stations <- d_sl |>
  distinct(station_name, station_id) |>
  deframe()

sl_station_default <- sl_stations["St. Petersburg"]

sl_yr_rng <- d_sl |>
  filter(station_id == sl_station_default) |>
  select(year) |>
  range()

sl_yr_default <- 2000

savevars_rds(c(
  "d_sl", "sl_stations", "sl_station_default", "sl_yr_rng", "sl_yr_default"))

# hurricanes ----
message("processing hurricane data...")
h_url <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/netcdf/IBTrACS.NA.v04r01.nc"
h_nc  <- here(glue("data/storms/{basename(h_url)}"))

# get centroid point of Tampa Bay watershed
ctr <- tbeptools::tbshed |>
  suppressWarnings({
    st_centroid() })
# st_coordinates(ctr) |> as.numeric()  # -82.35778  27.86714

h_sds    <- defStormsDataset(h_nc, basin = "NA", verbose = 0) # NA: North Atlantic basin
h_buf_km <- 1000
h_ply <- ctr |>
  st_buffer(h_buf_km * 1000)
# mapview::mapView(h_ply) + mapview::mapView(ctr)

h_yrs <- c(h_sds@seasons["min"], h_sds@seasons["max"]) |> as.integer() # 1980 2024
h_st <- defStormsList(
  h_sds,
  ctr,
  maxDist     = h_buf_km,
  removeUnder = 1,
  seasons     = h_yrs,
  verbose     = 0)

h_d <- tibble(
  storm    = getNames(h_st),
  year     = getSeasons(h_st) |> as.numeric(),
  date_beg = map_chr(h_st@data, ~.@obs.all$iso.time[[1]]) |>
    as.Date(),
  yday_beg = yday(date_beg),
  scale    = getScale(h_st)   |> as.numeric())  # A tibble: 221 × 3

h_d_sum <- h_d |>
  group_by(year) |>
  summarize(
    scale_sum = sum(scale),
    scale_avg = mean(scale) |> round(1),
    storms_n  = n(),
    storms_md = glue(
      "- {storm} ({scale})") |>
      paste(collapse = "\n"),
    .groups = "drop") |>
  mutate(
    label_md   = glue(
      "<b>{year}</b>, {scale_sum} scale sum
      {storms_n} storms (avg scale: {scale_avg}):
      {storms_md}"),
    label_html = markdownToHTML(label_md, fragment.only = T) )

stopifnot(length(setdiff(h_yrs[1]:h_yrs[2], h_d_sum$year)) == 0) # TODO: add missing years

h_yr_split <- 2000

savevars_rds(c("h_yrs", "h_st", "h_d", "h_d_sum", "h_yr_split"))

# default data ----

message("processing default data...")
# default to whole Tampa Bay (vs bay_segment)

d_temp <- d_prism_z |>
  filter(
    bay_segment == "TB",
    variable    == "tdmean") |>
  rename(value = mean) |>
  mutate(
    value = set_units(value, "degree_C"))

d_rain <- d_prism_z |>
  filter(
    bay_segment == "TB",
    variable    == "pptytd") |>
  rename(value = mean) |>
  mutate(
    value = set_units(value, "mm"))

d_sst <- d_sst_z |>
  filter(
    bay_segment == "TB") |>
  rename(value = mean) |>
  mutate(
    value = set_units(value, "degree_C"))

savevars_rds(c("d_temp", "d_rain", "d_sst"))

# finish ---
message(glue(
  "Data preprocessing complete. RDS files saved in:
   {dir_cache}"))
