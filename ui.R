thematic_shiny()
page_navbar(
  title = "Tampa Bay Climate Change Indicators",
  theme = light,

  # Info [i] ----
  nav_panel(
    title = tagList(
      bs_icon("info-circle-fill"), "Info"),
    p("Welcome to the Tampa Bay Climate Change Indicators dashboard. This dashboard
    provides an overview of climate change indicators for Tampa Bay, Florida. The
    dashboard is intended to provide a snapshot of climate change indicators in
    Tampa Bay and to help inform decision-making and planning for climate change
    adaptation and mitigation."),
    p("To learn more about each of the data sources, click on the descriptions
      below... "),
    accordion(
      accordion_panel(
        "Air Temperature",
        icon = bs_icon("thermometer-half"),
        includeMarkdown("doc/temp.md")
      ),
      accordion_panel(
        "Rainfall",
        icon = bs_icon("cloud-rain-fill"),
        includeMarkdown("doc/rain.md")
      ),
      accordion_panel(
        "Sea Level",
        icon = bs_icon("water"),
        includeMarkdown("doc/sl.md")
      ),
      accordion_panel(
        "Ocean Temperature",
        icon = bs_icon("thermometer-low"),
        includeMarkdown("doc/sst.md")
      ),
      accordion_panel(
        "Hurricanes",
        icon = bs_icon("tornado"),
        includeMarkdown("doc/hurricanes.md")
      )
    )
  ),

  # Overview [v] ----
  nav_panel(
    title = tagList(
      bs_icon("compass-fill"), "Overview"),
    value = "nav_overview",

    sliderInput(
      "sld_date_split",
      "Date to split comparison",
      min        = as.Date("1980-01-01"),
      value      = Sys.Date() - years(1),
      max        = Sys.Date(),
      timeFormat = "%F",
      step       = 1,
      animate    = T,
      width      = "100%"),

    layout_column_wrap(
      width = "400px",

      # * Air Temperature ----
      vb(
        title    = span(bs_icon("thermometer-half"), "Air Temperature"),
        value    = uiOutput("value_temp"),
        showcase = plotlyOutput("bar_temp"),
        uiOutput("caption_temp")),

      # * Rain ----
      vb(
        title    = span(bs_icon("cloud-rain-fill"), "Rain"),
        value    = uiOutput("value_rain"),
        showcase = plotlyOutput("bar_rain"),
        uiOutput("caption_rain")),

      # * Ocean Temperature ----
      vb(
        title    = span(bs_icon("thermometer-low"), "Ocean Temperature"),
        value    = uiOutput("value_sst"),
        showcase = plotlyOutput("bar_sst"),
        uiOutput("caption_sst")),

      # * Hurricanes ----
      vb(
        title    = span(bs_icon("tornado"), "Hurricanes"),
        value    = uiOutput("value_hurricanes"),
        showcase = plotlyOutput("bar_hurricanes"),
        uiOutput("caption_hurricanes")),

      # * Sea Level ----
      vb(
        title    = span(bs_icon("water"), "Sea Level"),
        value    = uiOutput("value_sl"),
        showcase = plotlyOutput("bar_sl"),
        uiOutput("caption_sl"))

    )),

  # Air Temperature [t] ----
  nav_panel(
    title = tagList(
      bs_icon("thermometer-half"), "Air Temperature"),
    navset_card_underline(

    # * Map ----
    nav_panel(
      span(
        bs_icon("map"), " Map"),
      card(
        full_screen = T,
        card_header(
          class = "d-flex",       # r-align gear icon
          span(
            "Air Temperature - Map of Then vs Now",
            class = "me-auto"),   # r-align gear icon

          popover(
            title = "Settings",
            bs_icon("gear"),

            selectInput(
              "sel_t_var",
              "Variable",
              c("Min"     = "tmin",
                # TODO: Average = tmean = mean(tmin, tmax)
                "Max"     = "tmax"),
              selected = "tmax"),

            sliderInput(
              "sld_t_yrs_then",
              "Year(s), Then",
              min     = yrs_prism[1],
              value   = c(yrs_prism[1], yrs_prism[1]+20),
              max     = yrs_prism[2] - 1,
              step    = 1,
              animate = T,
              sep     = ""),

            sliderInput(
              "sld_t_yrs_now",
              "Year(s), Now",
              min     = yrs_prism[1] + 1,
              value   = c(yrs_prism[2], yrs_prism[2]),
              max     = yrs_prism[2],
              step    = 1,
              animate = T,
              sep     = "")) ),

        leafletOutput("map_temp"),

        absolutePanel(
          id        = "pnl_t_md",
          bottom    = "5%", left = "5%", right = "5%",
          width     = "90%",

          sliderInput(
            "sld_t_md",
            "Month and day of year",
            min        = as.Date(glue("{year(now_prism)}-01-01")),
            value      = now_prism, # TODO: c(now_prism, now_prism)
            max        = as.Date(glue("{year(now_prism)}-12-31")),
            timeFormat = "%b %d",
            animate    = T,
            width     = "100%") ) ) ),

    # * Plot ----
    nav_panel(
      span(
        bs_icon("graph-up-arrow"), " Plot"),
      card(
        full_screen = T,
        card_header(
          class = "d-flex",       # r-align gear icon
          span(
            "Air Temperature - Day of year for all years",
            class = "me-auto"),   # r-align gear icon

          popover(
            title = "Settings",
            bs_icon("gear"),

            sliderInput(
              "sld_t_days_smooth",
              "# of days for smoothing average",
              min        = 0,
              value      = 0,
              max        = 90,
              animate    = T) ) ),

        selectInput(
          "sld_t_seg",
          "Bay Segment",
          prism_zones,
          selected = "TB"),

        plotlyOutput("plot_temp") ) ) ) ),

  # Rain [r] ----
  nav_panel(
    title = span(
      bs_icon("cloud-rain-fill"), " Rain"),
    navset_card_underline(

      # * Map ----
      nav_panel(
        span(
          bs_icon("map"), " Map"),

        card(
          full_screen = T,
          card_header(
            class = "d-flex",       # r-align gear icon
            span(
              "Precipitation - Map of Then vs Now",
              class = "me-auto"),   # r-align gear icon

            popover(
              title = "Settings",
              bs_icon("gear"),

              sliderInput(
                "sld_r_yrs_then",
                "Year(s), Then",
                min     = yrs_prism[1],
                value   = c(yrs_prism[1], yrs_prism[1]+20),
                max     = yrs_prism[2] - 1,
                step    = 1,
                animate = T,
                sep     = ""),

              sliderInput(
                "sld_r_yrs_now",
                "Year(s), Now",
                min     = yrs_prism[1] + 1,
                value   = c(yrs_prism[2], yrs_prism[2]),
                max     = yrs_prism[2],
                step    = 1,
                animate = T,
                sep     = "")) ),

          leafletOutput("map_rain"),

          absolutePanel(
            id        = "pnl_r_md",
            bottom    = "5%", left = "5%", right = "5%",
            width     = "90%",

            sliderInput(
              "sld_r_md",
              "Month and day of year",
              min        = as.Date(glue("{year(now_prism)}-01-01")),
              value      = now_prism,
              max        = as.Date(glue("{year(now_prism)}-12-31")),
              timeFormat = "%b %d",
              animate    = T,
              width     = "100%") ) ) ),

      # * Plot ----
      nav_panel(
        span(bs_icon("graph-up-arrow"), " Plot"),
        card(
          full_screen = T,
          card_header(
            class = "d-flex",       # r-align gear icon
            span(
              "Rain, year to date - Day of year for all years",
              class = "me-auto"),   # r-align gear icon

            popover(
              title = "Settings",
              bs_icon("gear"),

              sliderInput(
                "sld_r_days_smooth",
                "# of days for smoothing average",
                min        = 0,
                value      = 0,
                max        = 90,
                animate    = T) ) ),

          selectInput(
            "sld_r_seg",
            "Bay Segment",
            prism_zones,
            selected = "TB"),

          plotlyOutput("plot_rain") ) ) ) ),

  # Sea Level [l] ----
  nav_panel(
    title = span(bs_icon("water"), " Sea Level"),

      # * map ----
        card(
          full_screen = T,
          card_header(
            span( bs_icon("map"), " Map of sea level stations") ),
          # TODO: helpText("Click on a different station to see the data."),
          leafletOutput("map_sl") ),

      # * plot ----
        card(
          full_screen = T,
          card_header(
            class = "d-flex",       # r-align gear icon
            span(
              bs_icon("graph-up-arrow"), " Plot of sea levels",
              class = "me-auto"),   # r-align gear icon
              popover(
                title = "Settings",
                bs_icon("gear"),

                sliderInput(
                  "sld_l_yr_split",
                  "Year Split",
                  min     = sl_yr_rng[1],
                  value   = sl_yr_default,
                  max     = max(sl_yr_rng),
                  step    = 1,
                  animate = TRUE,
                  sep     = "") ) ),

          selectInput(
            "sel_l_stn",
            "Sea level station",
            sl_stations,
            sl_station_default),
          plotlyOutput("plot_sl") ) ),

  # Ocean Temperature [o] ----
  nav_panel(
    title = tagList(
      bs_icon("thermometer-low"), " Ocean Temperature"),
    navset_card_underline(

      # * Map ----
      nav_panel(
        span(bs_icon("map"), " Map"),
        card(
          full_screen = T,
          card_header(
            class = "d-flex",       # r-align gear icon
            span(
              "Sea Surface Temperate - Map of Then vs Now",
              class = "me-auto"),   # r-align gear icon

            popover(
              title = "Settings",
              placement = "right",
              bs_icon("gear"),

              sliderInput(
                "sld_o_yrs_then",
                "Year(s), Then",
                min     = yrs_sst[1],
                value   = c(yrs_sst[1], yrs_sst[1]+20),
                max     = yrs_sst[2] - 1,
                step    = 1,
                animate = T,
                sep     = ""),

              sliderInput(
                "sld_o_yrs_now",
                "Year(s), Now",
                min     = yrs_sst[1] + 1,
                value   = c(yrs_sst[2], yrs_sst[2]),
                max     = yrs_sst[2],
                step    = 1,
                animate = T,
                sep     = "")) ),

          leafletOutput("map_sst"),

          absolutePanel(
            id        = "pnl_o_md",
            bottom    = "5%", left = "5%", right = "5%",
            width     = "90%",

            sliderInput(
              "sld_o_md",
              "Month and day of year",
              min        = as.Date(glue("{year(now_sst)}-01-01")),
              value      = now_sst,
              max        = as.Date(glue("{year(now_sst)}-12-31")),
              timeFormat = "%b %d",
              animate    = T,
              width     = "100%") ) ) ),

      # * Plot ----
      nav_panel(
        tagList(
          bs_icon("graph-up-arrow"), " Plot"),
        card(
          full_screen = T,
          card_header(
            class = "d-flex",       # r-align gear icon
            span(
              "Sea Surface Temperature - Day of year for all years",
              class = "me-auto"),   # r-align gear icon

            popover(
              title = "Settings",
              bs_icon("gear"),

              sliderInput(
                "sld_o_days_smooth",
                "# of days for smoothing average",
                min        = 0,
                value      = 0,
                max        = 90,
                animate    = T) ) ),

          selectInput(
            "sld_o_seg",
            "Bay Segment",
            sst_zones),

          plotlyOutput("plot_sst") ) )
    )
  ),

  # Hurricanes [h] ----
  nav_panel(
    title = span(bs_icon("tornado"), " Hurricanes"),

    # * map ----
    card(
      full_screen = T,
      card_header(
        tagList(
          bs_icon("map"), " Map of hurricane tracks") ),
      # TODO: configure option to limit map of hurricane tracks by year
      leafletOutput("map_h"),

      absolutePanel(
        id        = "pnl_h_yrs",
        bottom    = "5%", left = "5%", right = "5%",
        width     = "90%",

        sliderInput(
          "sld_h_yrs",
          "Years",
          min        = h_yrs[1],
          value      = c(h_yrs[2] - 1, h_yrs[2]),
          max        = h_yrs[2],
          sep        = "",
          round      = T,
          step       = 1,
          animate    = T,
          width      = "100%") )

      ),

    # * plot ----
    card(
      full_screen = T,
      card_header(
        class = "d-flex",       # r-align gear icon
        span(
          bs_icon("graph-up-arrow"), " Plot of hurricanes over time",
          class = "me-auto"),   # r-align gear icon
        popover(
          title     = "Settings",
          bs_icon("gear"),

          sliderInput(
            "sld_h_yr_split",
            "Year Split",
            min     = min(h_d_sum$year),
            value   = h_yr_split,
            max     = max(h_d_sum$year),
            step    = 1,
            animate = T,
            sep     = "") ) ),
      plotlyOutput("plot_h") ) ),

  # More ----
  nav_spacer(),
  nav_menu(
    title = "More",
    align = "right",
    nav_item(
      tags$a(
        shiny::icon("info-circle"),
        "Source",
        href   = "https://github.com/tbep-tech/climate-dash",
        target = "_blank") ),
    nav_item(
      tags$a(
        shiny::icon("envelope", class = "fa-solid"),
        "Contact",
        href   = "https://tbep.org/about-tbep/contact/",
        target = "_blank") ) ),

  # sw_dark ----
  nav_item(
    input_switch("sw_dark", bs_icon("moon-stars-fill"), FALSE) ),

  # sw_imperial ----
  nav_item(
    input_switch("sw_imperial", "ºF, in", TRUE) ),

)
