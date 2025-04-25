FROM rocker/geospatial:latest

# Install shiny server
RUN /rocker_scripts/install_shiny_server.sh

# Install system dependencies for Git
RUN apt-get update && apt-get install -y \
    git \
    cron \
    libgit2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN install2.r --error \
    bsicons bslib curl dplyr gert glue here htmltools leaflet leaflet.extras2 lubridate \
    purrr RColorBrewer readr sf stringr terra tidyr webshot2

# Install GitHub packages
RUN installGithub.r tbep-tech/extractr # original from marinebon/extractr, our version includes a fix
RUN installGithub.r tbep-tech/tbeptools # R -e "remotes::install_github('tbep-tech/tbeptools')"

# Create required directories with appropriate permissions
RUN mkdir -p /share/github/tbep-tech/climate-change-indicators/data \
    /share/github/tbep-tech/climate-change-indicators/server \
    /var/log \
    && chown -R shiny:shiny /share \
    && chown -R shiny:shiny /var/log

# Copy environment variables if .env exists
COPY --chown=shiny:shiny .env* /share/github/tbep-tech/climate-change-indicators/server/.env*

# Switch to shiny user
USER shiny

# Add cron job for data updates
RUN echo "0 0 * * * /usr/bin/Rscript /share/github/tbep-tech/climate-change-indicators/server/update_data.R >> /share/log/climate_data_update.log 2>&1" | crontab -

EXPOSE 3838

CMD ["/init"]
