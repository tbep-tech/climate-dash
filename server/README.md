# Climate Change Indicators Server

This directory contains the Docker configuration for running the Climate Change Indicators Shiny application with automated daily data updates.

## Prerequisites

-   Docker installed on your system
-   Docker Compose installed on your system

## Environment Setup

The container requires certain environment variables for Git authentication and operations. Create a `.env` file in the server directory with the following variables:

``` bash
# Git Authentication
GITHUB_PAT=your_github_personal_access_token
GIT_USER=your_git_username
GIT_EMAIL=your_email@example.com
```

To create a GitHub Personal Access Token: 1. Go to GitHub Settings \> Developer settings \> Personal access tokens 2. Generate a new token with 'repo' scope 3. Copy the token and add it to your `.env` file

Note: The `.env` file is included in `.gitignore` to prevent exposing sensitive information.

## Building and Running

1.  Create the shared volume directory:

``` bash
sudo mkdir -p /share
sudo chown $USER:$USER /share
```

2.  Clone the repository into the shared volume:

``` bash
mkdir -p /share/github/tbep-tech
cd /share/github/tbep-tech
git clone https://github.com/tbep-tech/climate-change-indicators.git
```

3.  Build and start the containers:

``` bash
cd climate-change-indicators/server
docker pull --platform linux/x86_64 rocker/geospatial  # for Mac
docker compose up -d
```

The services will be available at:

-   Shiny app: https://climate.tbep.org/shiny/app/
-   RStudio: https://climate.tbep.org/rstudio/ (credentials: rstudio/rstudio)

## Development Workflow

The repository is mounted at `/share/github/tbep-tech/climate-change-indicators` inside the container. Any changes made to the files in this directory will be immediately reflected in the running container.

### Data Updates

Data updates are scheduled to run daily at midnight (UTC). The update process includes:

-   PRISM climate data
-   Sea level data
-   Sea Surface Temperature (SST) data
-   Hurricane tracking data

To manually trigger a data update:

``` bash
docker exec rstudio Rscript /share/github/tbep-tech/climate-change-indicators/server/update_data.R
```

### Monitoring and Maintenance

View container logs:

``` bash
docker compose logs
```

View data update logs:

``` bash
docker exec rstudio cat /var/log/climate_data_update.log
```

Stop the containers:

``` bash
docker compose down
```

## Security

-   All services are protected by Caddy's automatic HTTPS
-   RStudio access requires authentication
-   Sensitive information is stored in `.env` file
-   Git operations use secure token authentication

## Monitoring

Using Claude.ai...

PROMPT: What's an easy way to setup email or text notification upon getting any errors in an update_data.R script from a cron job inside a docker container?

RESPONSE:

I'll create a simple monitoring solution using R and the `gmailr` package for email notifications.

To implement this:

1.  Save this as `monitor.R` in your Docker container
2.  Setup Gmail OAuth credentials and save as `credentials.json`
3.  Update the cron job to run `Rscript monitor.R` instead of `update_data.R`
4.  For SMS, you can use email-to-SMS gateways (e.g., number\@vtext.com for Verizon)

Alternative solutions include:

-   Using AWS SNS for notifications
-   Slack webhooks
-   Monitoring services like Healthchecks.io

Let me know if you'd like details on any of these alternatives.
