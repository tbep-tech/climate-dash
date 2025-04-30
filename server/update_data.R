# Update climate data for Tampa Bay

# Need to set wd to project folder for here::here() to work
# this will only work in the container
script_path <- commandArgs(trailingOnly = FALSE)
script_path <- script_path[grep("--file=", script_path)]
script_path <- substring(script_path, 8)
proj_path <- dirname(dirname(script_path))
setwd(proj_path)

# log_txt <- "/var/log/climate_data_update.log"
log_txt <- here::here("tmp_log.txt") # DEBUG

log_message <- function(msg) {
  message   <- paste(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "~", msg)
  cat(message, "\n", file = log_txt, append = TRUE)
  cat(message, "\n") # Also print to console
}

# Load packages ----
log_message("Loading packages...")
librarian::shelf(
  curl, dplyr, gert, glue, here, lubridate, sf,
  tbep-tech/tbeptools,
  tbep-tech/extractr,
  quiet = TRUE)

# Setup variables ----
log_message("Setting up variables...")
tb_zones <- tbeptools::tbsegshed |>
  bind_rows(
    tbeptools::tbshed |>
      mutate(
        long_name   = "Tampa Bay",
        bay_segment = "TB") |>
      select(-Acres)) |>
  st_make_valid()

# Update functions ----
update_sea_level <- function() {
  sl_csv <- here("data/slr/monthly_sea_levels.csv")

  tbeptools::read_importsealevels(sl_csv)
}

update_sst <- function() {

  dir_nc <- here("data/sst/tb_sst_nc")
  if (dir.exists(dir_nc))
    unlink(dir_nc, recursive = TRUE)

  extractr::ed_extract(
    ed        = extractr::ed_info("https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html"),
    var       = "CRW_SST",
    bbox      = c(xmin = -83.0, ymin = 27.2 , xmax = -82.3, ymax= 28.5),
    sf_zones  = tb_zones,
    fld_zones = "bay_segment",
    zonal_csv = here::here("data/sst/tb_sst.csv"),
    rast_tif  = here::here("data/sst/tb_sst.tif"),
    mask_tif = F)
}

update_hurricane <- function() {
  url  <- "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/netcdf/IBTrACS.NA.v04r01.nc"
  tmp  <- tempfile(fileext = ".nc")
  nc   <- here("data/storms", basename(url))

  resp <- curl::multi_download(
    urls = url,
    destfile = tmp,
    progress = TRUE)

  if (!resp$success)
    stop(resp$error)

  file.copy(tmp, nc, overwrite = T)
  unlink(tmp)
}

update_prism <- function() {
  d <- tbeptools::read_importprism(
    vars      = c("tmin", "tmax", "tdmean", "ppt"),
    vars_ytd  = c("ppt"),
    date_beg  = as.Date("1981-01-01"),
    date_end  = as.Date('2025-04-29'),#Sys.Date(),
    bbox      = c(xmin = -82.9, ymin = 27.2, xmax = -81.7, ymax = 28.6),
    dir_tif   = here("data/prism"),
    sf_zones  = tb_zones,
    fld_zones = "bay_segment",
    zonal_csv = here("data/prism.csv"),
    verbose   = T)
}

git_update <- function() {
  # Configure git credentials
  git_config_set("user.name", Sys.getenv("GIT_USER"))
  git_config_set("user.email", Sys.getenv("GIT_EMAIL"))

  # Set up GitHub authentication
  pat <- Sys.getenv("GITHUB_PAT")
  if (pat == "")
    stop("GITHUB_PAT environment variable not set")
  git_config_set("credential.helper", "store")
  git_config_global_set("github.token", pat)

  # Add all changes in data directory
  git_add(here("data"), repo = here())

  # Commit with timestamp
  commit_msg <- glue("update_data.R: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}")
  git_commit(commit_msg, repo = here())

  # Push changes using PAT
  git_push(repo = here(), password = pat)
}

# Main Execution ----

main <- function() {
  log_message("Starting data update process")

  # List of update functions to run
  update_fns <- list(
    prism     = update_prism,
    sea_level = update_sea_level,
    sst       = update_sst,
    hurricane = update_hurricane)#,
    # git       = git_update)

  # Run each function with error handling
  lapply(names(update_fns), function(fn_name) {
    tryCatch({
      log_message(glue("Starting update: {fn_name}"))
      update_fns[[fn_name]]()
      log_message(glue("Successfully completed update: {fn_name}"))
    }, error = function(e) {
      log_message(glue("Error updating {fn_name}: {conditionMessage(e)}"))
    })
  })

  log_message("Updating app cache")
  source(here("server/prep_cache.R"))

  log_message("Data update process completed")
}

main()
