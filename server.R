function(input, output, session) {

  # sw_dark ----
  observe(session$setCurrentTheme(
    if (isTRUE(input$sw_dark)) dark else light ))

  # sw_imperial ----
  observeEvent(input$sw_imperial, {
    update_switch(
      "sw_imperial",
      label = ifelse(
        input$sw_imperial,
        "ºF, in",
        "ºC, mm"),
      value = input$sw_imperial)
  })

  # rx_vals ----
  # track which boxes are exploded
  rx_exploded <- reactiveValues(
    hurricanes  = F,
    rain        = F,
    sea_level   = F,
    sst         = F,
    temperature = F,
    map_sst_init = F)
  # track which maps initiated to observe and update rasters after
  rx_map_init <- reactiveValues(
    rain = F,
    sl   = F,
    sst  = F,
    temp = F)

  # Overview ----

  # * Air Temperature ----

  # ·· rx_temp ----
  rx_temp <- reactive({
    date_split      <- input$sld_date_split
    show_isImperial <- input$sw_imperial
    show_units      <- ifelse(show_isImperial, "°F", "°C")

    d <- anlz_splitdata(
      d_temp,
      date_split,
      date_col  = "date",
      value_col = "value") |>
      mutate(
        avg = set_units(avg, show_units, mode = "standard"))

    # calculate difference after - before for caption and value
    v <- d |>
      group_by(period) |>
      summarize(
        v = mean(avg)) |>
      pull(v) |>
      diff() |>
      round(1) |>
      drop_units()

    attr(d, "value") <- glue("{ifelse(v > 0, '+','')} {v} {show_units}")

    attr(d, "caption") <- glue(
      "The annual average temperature has
       {ifelse(v > 0, 'increased', 'decreased')} by {abs(v)} {show_units}
       since {format(date_split, '%Y')} with years split around
       {format(date_split, '%b %e')}.")

    d
  }) # |>
    # debounce(rx_wait)

  # ·· value_temp ----
  output$value_temp <- renderUI({
    attr(rx_temp(), "value")
  })

  # ·· caption_temp ----
  output$caption_temp <- renderUI({
    attr(rx_temp(), "caption")
  })

  # ·· bar_temp ----
  output$bar_temp <- renderPlotly({
    rx_temp() |>
      show_splitbarplot(
        "period", "year", "avg",
        exploded       = rx_exploded$temperature,
        source         = "T") |>
      event_register("plotly_click") |>
      layout(clickmode = "event")
  })

  # ·· click_temp ----
  observeEvent(event_data("plotly_click", "T"), {
    rx_exploded$temperature <- !rx_exploded$temperature
  })

  # * Rain ----

  # ·· rx_rain ----
  rx_rain <- reactive({
    # varies with input$sw_imperial, input$sld_date_split
    # DEBUG: input <- list(sw_imperial=T, sld_date_split=as.Date("2023-11-20"))

    date_split      <- input$sld_date_split
    show_isImperial <- input$sw_imperial
    show_units      <- ifelse(show_isImperial, "in", "mm")

    d <- anlz_splitdata(
      d_rain,
      date_split,
      date_col  = "date",
      value_col = "value") |>
      mutate(
        avg = set_units(avg, show_units, mode = "standard"))

    # calculate difference after - before for caption and value
    v <- d |>
      group_by(period) |>
      summarize(
        v = mean(avg)) |>
      pull(v) |>
      diff() |>
      round(1) |>
      drop_units()

    attr(d, "value") <- glue("{ifelse(v > 0, '+','')} {v} {show_units}")

    attr(d, "caption") <- glue(
      "The annual average rainfall has been
       {ifelse(v > 0, 'wetter', 'drier')} by {abs(v)} {show_units}
       since {format(date_split, '%Y')} with years split around
       {format(date_split, '%b %e')}.")

    d
  }) # |>
    # debounce(rx_wait)

  # ·· value_rain ----
  output$value_rain <- renderUI({
    attr(rx_rain(), "value")
  })

  # ·· caption_rain ----
  output$caption_rain <- renderUI({
    attr(rx_rain(), "caption")
  })

  # ·· bar_rain ----
  output$bar_rain <- renderPlotly({
    rx_rain() |>
      show_splitbarplot(
        "period", "year", "avg",
        exploded       = rx_exploded$rain,
        source         = "R") |>
      event_register("plotly_click") |>
      layout(clickmode = "event")
  })

  # ·· click_rain ----
  observeEvent(event_data("plotly_click", "R"), {
    rx_exploded$rain <- !rx_exploded$rain
  })

  # * Ocean Temperature ----

  # ·· rx_sst ----
  rx_sst <- reactive({

    date_split      <- input$sld_date_split
    show_isImperial <- input$sw_imperial
    show_units      <- ifelse(show_isImperial, "°F", "°C")

    d <- anlz_splitdata(
      d_sst,
      date_split,
      date_col  = "date",
      value_col = "value") |>
      mutate(
        avg = set_units(avg, show_units, mode = "standard"))

    # calculate difference after - before for caption and value
    v <- d |>
      group_by(period) |>
      summarize(
        v = mean(avg)) |>
      pull(v) |>
      diff() |>
      round(1) |>
      drop_units()

    attr(d, "value") <- glue("{ifelse(v > 0, '+','')} {v} {show_units}")

    attr(d, "caption") <- glue(
      "The annual average ocean temperature has
       {ifelse(v > 0, 'increased', 'decreased')} by {abs(v)} {show_units}
       since {format(date_split, '%Y')} with years split around
       {format(date_split, '%b %e')}.")

    d
  }) # |>
    # debounce(rx_wait)

  # ·· value_sst ----
  output$value_sst <- renderUI({
    attr(rx_sst(), "value")
  })

  # ·· caption_sst ----
  output$caption_sst <- renderUI({
    attr(rx_sst(), "caption")
  })

  # ·· bar_sst ----
  output$bar_sst <- renderPlotly({
    rx_sst() |>
      show_splitbarplot(
        "period", "year", "avg",
        exploded       = rx_exploded$sst,
        source         = "S") |>
      event_register("plotly_click") |>
      layout(clickmode = "event")
  })

  # ·· click_sst ----
  observeEvent(event_data("plotly_click", "S"), {
    rx_exploded$sst <- !rx_exploded$sst
  })

  # * Hurricanes ----

  # ·· rx_hurricanes ----
  rx_hurricanes <- reactive({

    date_split <- input$sld_date_split

    d <- anlz_splitstorms(h_d, date_split)

    # calculate difference after - before for caption and value
    v <- d |>
      group_by(period) |>
      summarize(
        v = mean(sum)) |>
      pull(v) |>
      diff() |>
      round(1)

    attr(d, "value") <- glue("{ifelse(v > 0, '+','')} {v} cat")

    attr(d, "caption") <- glue(
      "The annual average sum of hurricane categories (cat) has
      {ifelse(v > 0, 'increased','decreased')} by {v}
       since {format(date_split, '%Y')} with years split around
       {format(date_split, '%b %e')}.")

    d
  }) # |>
    # debounce(rx_wait)

  # ·· value_hurricanes ----
  output$value_hurricanes <- renderUI({
    attr(rx_hurricanes(), "value")
  })

  # ·· caption_hurricanes ----
  output$caption_hurricanes <- renderUI({
    attr(rx_hurricanes(), "caption")
  })

  # ·· bar_hurricanes ----
  output$bar_hurricanes <- renderPlotly({
    rx_hurricanes() |>
      show_splitbarplot(
        "period", "year", "sum",
        exploded       = rx_exploded$hurricanes,
        source         = "H") |>
      event_register("plotly_click") |>
      layout(clickmode = "event")
  })

  # ·· click_hurricanes ----
  observeEvent(event_data("plotly_click", "H"), {
    rx_exploded$hurricanes <- !rx_exploded$hurricanes
  })

  # * Sea Level ----

  # ·· rx_sl ----
  rx_sl <- reactive({

    date_split <- input$sld_date_split

    # Split data by date and default station
    d <- d_sl |>
      filter(
        station_id == sl_station_default,
        !is.na(msl)) |>
      mutate(
        date_grp = factor(
          case_when(
          date > date_split ~ "after",
          TRUE              ~ "before"),
          ordered = T,
          levels = c("before", "after")))
    # table(d$date_grp)
    # after before
    #    10    923

    # Convert to imperial if needed
    if (input$sw_imperial){
      d$msl <- d$msl * 100 / 2.54  # m to in
    } else {
      d$msl <- d$msl * 100         # m to cm
    }

    units <- ifelse(input$sw_imperial, "in/decade", "cm/decade")

    # Calculate trends and stats for each period
    d <- d |>
      group_by(date_grp) |>
      group_modify(~ {

        mdl  <- lm(msl ~ date, data = .)
        rsq  <- summary(mdl)$r.squared
        rate <- coef(mdl)[["date"]] * 365 * 10  # convert to [cm|in]/decade

        # Create annotation text
        text <- sprintf(
          "%s: %0.2f %s\n(R² = %0.2f)",
          .$date_grp[1], rate, units, rsq)

        tibble(
          date  = median(.$date),
          msl   = predict(mdl, newdata = data.frame(date = median(.$date))),
          rate  = rate,
          units = units,
          text  = text,
          line  = list(tibble(
            date = range(.$date),
            msl  = predict(mdl, newdata = data.frame(date = range(.$date))) )) )
      }, .keep = T)

    # calculate difference after - before for caption and value
    v <- d |>
      arrange(date_grp) |>
      pull(rate) |>
      diff() |>
      round(1)

    attr(d, "value") <- glue("{ifelse(v > 0, '+','')} {v} {units}")

    attr(d, "caption") <- glue(
      "Sea level rise ({units}) has
      {ifelse(v > 0, 'increased','decreased')} by {v}
       since {format(date_split, '%Y-%m-%d')} at {names(sl_station_default)}.")

    d
  }) # |>
    # debounce(rx_wait)

  # ·· value_sl ----
  output$value_sl <- renderUI({
    attr(rx_sl(), "value")
  })

  # ·· caption_sl ----
  output$caption_sl <- renderUI({
    attr(rx_sl(), "caption")
  })

  # ·· bar_sl ----
  output$bar_sl <- renderPlotly({

    rx_sl() |>
      ggplot(
        aes(
          x    = date_grp,
          y    = rate,
          fill = date_grp)) +
      geom_col(alpha = 0.7) +
      scale_fill_manual(
        values = c("#00BFC4", "#F8766D")) +
      ggplot2::theme(
        axis.text       = element_text(size = 14),
        axis.title      = ggplot2::element_blank(),
        legend.position = "none")

  })

  # Air Temperature [t] ----

  # * map_temp ----
  output$map_temp <- renderLeaflet({
    rx_map_init$temp <- T
    map_init()
  })

  # * ∆ map_temp rasters ----
  observe({
    req(rx_map_init$temp)

    leafletProxy("map_temp") |>
      map_update_basemap(input$sw_dark) |>
      map_update_polys(input$sw_dark) |>
      map_update_rasters(
        var         = input$sel_t_var,
        md          = format(input$sld_t_md, "%m-%d"),
        yrs_now     = input$sld_t_yrs_now[1]:input$sld_t_yrs_now[2],
        yrs_then    = input$sld_t_yrs_then[1]:input$sld_t_yrs_then[2],
        is_imperial = input$sw_imperial)
  })

  # * plot_temp ----
  output$plot_temp <- renderPlotly({

    d_prism_z |>
      filter(
        variable    == input$sel_t_var,
        bay_segment == input$sld_t_seg) |>
      mutate(
        time = as.POSIXct(date)) |>
      select(time, val = mean) |>
      plot_doy(
        days_smooth = input$sld_t_days_smooth)
  }) # |>
    # debounce(rx_wait)

  # Rain [r] ----

  # * map_rain ----
  output$map_rain <- renderLeaflet({
    rx_map_init$rain <- T
    map_init()
  })

  # * ∆ map_rain ----
  observe({
    req(rx_map_init$rain)

    leafletProxy("map_rain") |>
      map_update_basemap(input$sw_dark) |>
      map_update_polys(input$sw_dark) |>
      map_update_rasters(
        var         = "pptytd",
        md          = format(input$sld_r_md, "%m-%d"),
        yrs_now     = input$sld_r_yrs_now[1]:input$sld_r_yrs_now[2],
        yrs_then    = input$sld_r_yrs_then[1]:input$sld_r_yrs_then[2],
        is_imperial = input$sw_imperial)
  })

  # * plot_rain ----
  output$plot_rain <- renderPlotly({

    d_prism_z |>
      filter(
        variable    == "pptytd",
        bay_segment == input$sld_r_seg) |>
      mutate(
        time = as.POSIXct(date)) |>
      select(time, val = mean) |>
      plot_doy(
        days_smooth    = input$sld_r_days_smooth,
        color_thisyear = "purple",
        color_lastyear = "darkblue",
        ylab           = "Rain, year to date (mm)")
  })

  # Sea Level [l] ----

  # * map_sl ----
  output$map_sl <- renderLeaflet({
    rx_map_init$sl <- T
    map_sl()
  })

  # * ∆ map_sl ----
  observe({
    req(rx_map_init$sl)
    req(input$sel_l_stn)

    leafletProxy("map_sl")  |>
      clearGroup("highlighted") |>
      addCircleMarkers(
        data = tbeptools::sealevelstations |>
          filter(
            station_id == input$sel_l_stn),
        lng     = ~longitude,
        lat     = ~latitude,
        layerId = ~station_id,
        group = "highlighted",
        color = "yellow",
        fillColor = "yellow",
        radius = 8,
        weight = 2,
        opacity = 1,
        fillOpacity = 0.5)
  })

  # * observe map_sl clicks ----
  observeEvent(input$map_sl_marker_click, {
    click <- input$map_sl_marker_click
    if (!is.null(click$id)) {
      updateSelectInput(session, "sel_l_stn", selected = click$id)
    }
  })

  # * plot_sl ----
  output$plot_sl <- renderPlotly({

    # Split data by year and calculate trends
    d <- d_sl |>
      filter(
        station_id == input$sel_l_stn,
        !is.na(msl)) |>
      mutate(
        yr_grp = case_when(
          year > input$sld_l_yr_split  ~ "after",
          TRUE                         ~ "before"))
    if (input$sw_imperial){
      d$msl <- d$msl * 100 / 2.54  # m to in
    } else {
      d$msl <- d$msl * 100         # m to cm
    }

    # Calculate trends and stats for each period
    trends <- d |>
      group_by(yr_grp) |>
      group_modify(~ {

        mdl  <- lm(msl ~ date, data = .)
        rsq  <- summary(mdl)$r.squared
        rate <- coef(mdl)[["date"]] * 365 * 10  # convert to [cm|in]/decade

        # Convert units if imperial
        unit <- ifelse(input$sw_imperial, "in/decade", "cm/decade")

        # Create annotation text
        text <- sprintf(
          "%s: %0.2f %s\n(R² = %0.2f)",
          .$yr_grp[1], rate, unit, rsq)

        tibble(
          date = median(.$date),
          msl  = predict(mdl, newdata = data.frame(date = median(.$date))),
          text = text,
          line = list(tibble(
            date = range(.$date),
            msl  = predict(mdl, newdata = data.frame(date = range(.$date))) )) )
      }, .keep = T)

    # Station name
    stn <- names(sl_stations)[sl_stations == input$sel_l_stn]

    # Create ggplot
    p <- d |>
      ggplot(aes(date, msl, color = yr_grp)) +
      geom_point(alpha = 0.3) +
      geom_line(
        data = trends |>
          select(yr_grp, line) |>
          unnest(cols = line), linewidth = 1) +
      scale_color_manual(
        values = c("before" = "#00BFC4", "after" = "#F8766D"),
        guide = "none") +
      labs(
        x = "Date",
        y = ifelse(input$sw_imperial, "Mean Sea Level (inches)", "Mean Sea Level (cm)"),
        title = glue("Sea Level Rise at {stn}"))

    # Create plotly
    p <- ggplotly(p) |>
      style(showlegend = F) |>
      add_annotations(
        x         = as.numeric(trends$date),
        y         = trends$msl,
        text      = trends$text)

    p
  })

  # Ocean Temperature [o] ----

  # * map_sst ----
  output$map_sst <- renderLeaflet({
    rx_map_init$sst <- T
    map_init()
  })

  # * ∆ map_sst ----
  observe({
    req(rx_map_init$sst)

    leafletProxy("map_sst") |>
      map_update_basemap(input$sw_dark) |>
      map_update_polys(input$sw_dark) |>
      map_update_rasters(
        var         = "sst",
        data        = "sst",
        md          = format(input$sld_o_md, "%m-%d"),
        yrs_now     = input$sld_o_yrs_now[1]:input$sld_o_yrs_now[2],
        yrs_then    = input$sld_o_yrs_then[1]:input$sld_o_yrs_then[2],
        is_imperial = input$sw_imperial)
  })

  # * plot_sst ----
  output$plot_sst <- renderPlotly({

    d_sst_z |>
      rename(val = mean) |>
      filter(
        bay_segment == input$sld_o_seg) |>
      plot_doy(
        days_smooth = input$sld_o_days_smooth)
  })

  # Hurricanes [h] ----

  # * map_h ----
  output$map_h <- renderLeaflet({

    h_st |>
      h_filt_yrs(input$sld_h_yrs) |>
      plotStorms(dynamicPlot = T)

  })

  # * plot_h ----
  output$plot_h <- renderPlotly({

    d <- h_d_sum |>
      select(year, scale_sum, label_md) |>
      mutate(
        yr_grp = case_when(
          year > input$sld_h_yr_split  ~ "after",
          TRUE                         ~ "before"))

    d_g <- d |>
      group_by(yr_grp) |>
      summarise(
        yr_min = min(year),
        yr_max = max(year),
        avg    = mean(scale_sum))

    p <- d |>
      ggplot(aes(x = year, y = scale_sum, text = label_md)) +
      geom_line(aes(group = 1)) +
      geom_point() +
      scale_x_continuous(
        limits = h_yrs,
        expand = c(0, 0)) +
      geom_segment(
        aes(
          x     = yr_min,
          xend  = yr_max,
          y     = avg,
          yend  = avg,
          text  = NULL,
          group = yr_grp),
        data = d_g,
        linetype = "dashed")

    ggplotly(p, tooltip = list("text"))
  })

}
