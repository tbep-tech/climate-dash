# TODO LATER:
# - [ ] temp: heat index using dewpoint (`tdmean` -> humidity)
# - [ ] tour: with [cicero](https://cicerone.john-coene.com)
# - [ ] download: data, static figures (plotly has option -- to highlight in About)

# devtools::install_local(here::here("../tbeptools"), force = T)
# devtools::load_all(here::here("../tbeptools"))
librarian::shelf(
  bsicons, bslib, dplyr, glue, here, htmltools, leaflet, leaflet.extras2,
  lubridate, markdown, plotly, purrr, readr, scales, sf, shiny, slider, stringr,
  tbep-tech/tbeptools,
  StormR, terra, thematic, tibble, tidyr, units)
set.seed(42)
source(here("R/functions.R"))

# themes ----
light <- bs_theme(preset = "flatly")
dark  <- bs_theme(preset = "darkly")

# cache ----
dir_cache  <- here("data/cache")
cache_vars <- c(
  "d_prism_r", "d_prism_z", "yrs_prism", "now_prism", "prism_zones",          # prism
  "sf_tb", "d_sst_r", "yrs_sst", "now_sst", "d_sst_z", "sst_zones", "r_sst", # sst
  "d_sl", "sl_stations", "sl_station_default", "sl_yr_rng", "sl_yr_default",  # sea level
  "h_yrs", "h_st", "h_d", "h_d_sum", "h_yr_split",                            # hurricanes
  "d_temp", "d_rain", "d_sst")                                                # default data

# Load cached data
var_rds <- list.files(dir_cache, pattern = "\\.rds$", full.names = T)
if (!all(glue("{cache_vars}.rds") %in% basename(var_rds))){
  vars_miss <- setdiff(glue("{cache_vars}.rds"), basename(var_rds)) |> str_replace("\\.rds", "")
  message(glue("Generating cache since missing vars stored in data/cache/*.rds: {paste(vars_miss, collapse=', ')}"))
  source(here("app/prep_cache.R"))
}
stopifnot(all(glue("{cache_vars}.rds") %in% basename(var_rds)))
for (var in cache_vars)
  assign(var, read_rds(glue("{dir_cache}/{var}.rds")))

# rasters ----
# rasters are pointers and don't cache well

sst_tif <- here("data/sst/tb_sst.tif")
r_sst   <- rast(sst_tif)

# map bounding box ----
bb <- st_bbox(tbsegshed) |> as.numeric()

# overview ----

vb <- function(...){
  value_box(
    showcase_layout = showcase_bottom(
      height     = 0.5,
      max_height = "200px"),
    max_height   = "350px",
    full_screen  = TRUE,
    theme        = "primary",
    ...)
}

# debounce reactives ----
# rx_wait <- 333  # wait 1/3 second for reactives
