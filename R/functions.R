get_prism_r <- function(dates, var){  # dates = dates_then
  d <- d_prism_r |>
    filter(
      date     %in% !!dates,
      variable  ==  !!var) |>
    select(path_tif, lyr)

  rast(unique(d$path_tif)) |>
    subset(d$lyr) |>
    mean() |>
    project(leaflet:::epsg3857)
}

get_sst_r <- function(dates){  # dates = dates_then

  lyrs <- d_sst_r |>
    filter(
      date %in% !!dates) |>
    pull(lyr)

  r_sst |>
    subset(lyrs) |>
    mean() |>
    project(leaflet:::epsg3857)
}

h_filt_yrs <- function(st, yrs){
  st_yrs <- getSeasons(st) |> as.numeric()
  i_yrs <- st_yrs >= yrs[1] & st_yrs <= yrs[2]
  st@data <- st@data[i_yrs]
  st
}

map_sl <- function(
    dark_mode = T){

  tiles = ifelse(
    dark_mode,
    providers$CartoDB.DarkMatter,
    providers$CartoDB.Positron)

  color_tbsegshed = ifelse(
    dark_mode,
    "white",
    "black")

  leaflet() |>
    addProviderTiles(
      tiles,
      group   = "basemap",
      layerId = "basemap") |>
    addPolygons(
      data         = tbsegshed,
      label        = tbsegshed$long_name,
      labelOptions = labelOptions(
        interactive = T),
      color        = color_tbsegshed,
      weight       = 2,
      fillOpacity  = 0) |>
    addMarkers(
      data    = tbeptools::sealevelstations,
      lng     = ~longitude,
      lat     = ~latitude,
      layerId = ~station_id,
      group   = "sl_stations",
      label   = ~glue(
        "<b>{station_name}</b><br>
        sea level station") |>
        lapply(HTML),
      popup   = ~glue(
        "<b>{station_name}</b><br>
        sea level station<br>
        ID: {station_id}<br>
        est.: {date_est}"),
      options = markerOptions(

      ))
}

map_init <- function(
    dark_mode = T, is_swiping = T){

  map <- leaflet() |>
    addMapPane("pn_polys",   zIndex = 490) |>
    map_update_polys(dark_mode) |>
    fitBounds(bb[1], bb[2], bb[3], bb[4])

  if (is_swiping){
    map <- map |>
      addMapPane("pn_left",  zIndex = 450) |>
      addMapPane("pn_right", zIndex = 450) |>
      map_update_basemap(is_swiping = T)
  } else {
    map <- map |>
      map_update_basemap(dark_mode, is_swiping = F)
  }
  map
}

map_update_basemap <- function(
    map, dark_mode = T, is_swiping = T,
    tiles_dark  = providers$CartoDB.DarkMatter,
    tiles_light = providers$CartoDB.Positron,
    ...){

  tiles = ifelse(
    dark_mode,
    tiles_dark,
    tiles_light)

  if (is_swiping){
    map <- map |>
      addProviderTiles(
        tiles,
        layerId = "lyr_base_left",
        options = pathOptions(
          pane = "pn_left"),
        ...) |>
      addProviderTiles(
        tiles,
        layerId = "lyr_base_right",
        options = pathOptions(
          pane = "pn_right"),
        ...) |>
      addSidebyside(
        layerId = "swiper",
        leftId  = "lyr_base_left",
        rightId = "lyr_base_right")
  } else {
    map <- map |>
      addProviderTiles(
        tiles,
        ...)
  }

  map
}

map_update_polys <- function(
    map, dark_mode = T){

  ln_color = ifelse(
    dark_mode,
    "white",
    "black")

  map |>
    addPolygons(
      data         = tbsegshed,
      label        = tbsegshed$long_name,
      labelOptions = labelOptions(
        interactive = T),
      color        = ln_color,
      weight       = 2,
      fillOpacity  = 0,
      layerId      = ~bay_segment,
      group        = "grp_polys",
      options      = pathOptions(
        pane = "pn_polys"))
}

map_update_rasters <- function(
    map,
    var,
    md,
    yrs_now,
    yrs_then,
    data        = "prism",   # or "sst"
    palette     = "Spectral",
    palette_rev = TRUE,
    is_imperial = TRUE){

  # dates
  dates_now  <- as.Date(glue("{yrs_now}-{md}"))
  dates_then <- as.Date(glue("{yrs_then}-{md}"))
  if (any(dates_now > now_prism))
    dates_now[dates_now > now_prism] <- dates_now[dates_now > now_prism] - years(1)
  yrs_now_rng  <- year(dates_now)
  yrs_then_rng <- year(dates_then)
  if (length(yrs_now_rng) > 2)
    yrs_now_rng <- range(yrs_now_rng)
  if (length(yrs_then_rng) > 2)
    yrs_then_rng <- range(yrs_then_rng)

  # rasters
  stopifnot(data %in% c("prism", "sst"))
  stopifnot(length(data) == 1)
  if (data == "prism"){
    r_now  <- get_prism_r(dates_now,  var)
    r_then <- get_prism_r(dates_then, var)
  } else if (data == "sst"){
    r_now  <- get_sst_r(dates_now)
    r_then <- get_sst_r(dates_then)
  }

  # label
  var_lbl  <- lab_fun(var, is_imperial)

  # convert if needed
  if (is_imperial & grepl("°F", var_lbl)){
    # °C to °F
    r_then <- r_then * 9/5 + 32
    r_now  <- r_now  * 9/5 + 32
  }
  if (is_imperial & grepl("Rain \\(in\\)", var_lbl)){
    # mm to inch
    r_then <- r_then / 25.4
    r_now  <- r_now  / 25.4
  }

  # legends
  lgnd_now <- glue(
    "<b>Now</b><br>
          {format(dates_now[1], '%b %d')},
          {paste(yrs_now_rng, collapse = ' to ')}")
  lgnd_then <- glue(
    "<b>Then</b><br>
           {format(dates_then[1], '%b %d')},
           {paste(yrs_then_rng, collapse = ' to ')}")

  vals <- c(values(r_now, na.rm=T), values(r_then, na.rm=T))
  pal  <- colorNumeric(
    palette, vals, reverse = palette_rev, na.color = "transparent")

  map |>
    addRasterImage(
      r_then,
      colors  = pal,
      opacity = 0.8,
      project = F,
      group   = "grp_then",
      layerId = "lyr_then",
      options = pathOptions(
        pane = "pn_left")) |>
    addRasterImage(
      r_now,
      colors  = pal,
      opacity = 0.8,
      project = F,
      group   = "grp_now",
      layerId = "lyr_now",
      options = pathOptions(
        pane = "pn_right")) |>
    # addLayersControl(
    #   overlayGroups = c("grp_then", "grp_now", "grp_polys")) |>
    addControl(
      HTML(lgnd_then),
      position = "topleft",
      layerId  = "lyr_lgnd_then") |>
    addControl(
      HTML(lgnd_now),
      position = "topright",
      layerId  = "lyr_lgnd_now") |>
    addLegend(
      pal     = pal,
      values  = vals,
      title   = var_lbl,
      layerId =  "lyr_lgnd")
}


lab_fun <- function(var, is_imperial = T){

  # units
  var_info <- list(
    # prism
    tmin = list(
      name  = "Min Temperature",
      units = "°C"),
    tmax   = list(
      name  = "Max Temperature",
      units = "°C"),
    ppt    = list(
      name  = "Rain",
      units = "mm"),
    pptytd = list(
      name  = "Rain",
      sfx   = "year to date",
      units = "mm"),
    # sst
    sst = list(
      name  = "Sea Surface Temperature",
      units = "°C"))[[var]]

  if (is_imperial & var_info$units == "°C"){
    var_info$units <- "°F"
  }
  if (is_imperial & var_info$units == "mm"){
    var_info$units <- "in"
  }

  # label
  var_lbl  <- glue("{var_info$name} ({var_info$units})")
  if ("sfx" %in% names(var_info))
    var_lbl <- glue(
      "{var_lbl}
      {var_info$sfx}")

  return(var_lbl)

}

plot_doy <- function(
    df, # required columns: time, val
    var,
    days_smooth      = 7,
    color_thisyear   = "red",
    color_lastyear   = "orange",
    color_otheryears = "gray",
    size_thisyear    = 1.5,
    size_lastyear    = 1,
    size_otheryears  = 0.5,
    alpha_otheryears = 0.5,
    interactive      = T,
    is_imperial      = T){

  # DEBUG:
  # df = d_temp; days_smooth = 7
  # color_thisyear = "red"; color_lastyear = "orange"; color_otheryears = "gray"
  # size_thisyear = 1.5; size_lastyear = 1; size_otheryears = 0.5; alpha_otheryears = 0.5
  # interactive = T; is_imperial = T

  # get years
  yr_this <- max(year(df$time))
  yr_last <- yr_this - 1
  yrs_oth <- setdiff(
    unique(year(df$time)),
    c(yr_this, yr_last))

  # add doy
  d <- df |>
    mutate(
      year = year(time),
      doy  = yday(time),
      grp  = case_when(
        year == yr_this ~ "this",
        year == yr_last ~ "last",
        T               ~ "other"))

  # label
  var_lbl <- lab_fun(var, is_imperial)

  # convert to imperial if needed
  if (is_imperial & grepl("°F", var_lbl)){
    # °C to °F
    d$val <- d$val * 9/5 + 32
  }
  if (is_imperial & grepl("Rain \\(in\\)", var_lbl)){
    # mm to inch
    d$val <- d$val / 25.4
  }

  # get mean per doy per year
  d_doy <- d |>
    group_by(year, doy) |>
    summarize(
      val = mean(val),
      grp = first(grp),
      .groups = "drop")

  # smooth per year
  if (days_smooth > 0){
    d_doy <- d_doy |>
      group_by(year) |>
      arrange(doy) |>
      mutate(
        val = zoo::rollmean(
          val,
          k      = days_smooth,
          fill   = NA,
          align  = "center")) |>
      ungroup() |>
      filter(!is.na(val))
  }

  # get min, max per doy
  d_doy_rng <- d_doy |>
    filter(grp == "other") |>
    group_by(doy) |>
    summarize(
      min = min(val, na.rm=T),
      max = max(val, na.rm=T),
      .groups = "drop")

  # get mean per doy for other years
  d_doy_oth <- d_doy |>
    filter(grp == "other") |>
    group_by(doy) |>
    summarize(
      val = mean(val, na.rm=T),
      .groups = "drop")

  # get data for this and last year
  d_doy_tl <- d_doy |>
    filter(grp != "other")

  # plot
  g <- ggplot() +
    geom_ribbon(
      data = d_doy_rng,
      aes(x = doy, ymin = min, ymax = max),
      alpha = alpha_otheryears,
      fill  = color_otheryears) +
    geom_line(
      data = d_doy_oth,
      aes(x = doy, y = val),
      color = color_otheryears,
      linewidth  = size_otheryears) +
    geom_line(
      data = d_doy_tl |> filter(grp == "last"),
      aes(x = doy, y = val),
      color = color_lastyear,
      linewidth  = size_lastyear) +
    geom_line(
      data = d_doy_tl |> filter(grp == "this"),
      aes(x = doy, y = val),
      color = color_thisyear,
      linewidth  = size_thisyear) +
    labs(
      x = "Day of Year",
      y = var_lbl) +
    scale_x_continuous(
      breaks = seq(1, 365, 30),
      labels = function(x) format(as.Date(x, origin = "2000-01-01"), "%b %d"))

  if (!interactive)
    return(g)

  ggplotly(g)
}


