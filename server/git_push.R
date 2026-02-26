# Weekly git commit and push for climate-dash data updates
# Intended to be run by a weekly cron job inside the Docker container.
# Requires environment variables: GITHUB_PAT, GIT_USER, GIT_EMAIL

librarian::shelf(gert, here, glue, quiet = TRUE)

log_txt <- "/var/log/shiny-server/climate_git_push.log"

log_message <- function(msg) {
  message <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "~", msg)
  cat(message, "\n", file = log_txt, append = TRUE)
  cat(message, "\n")
}

pat   <- Sys.getenv("GITHUB_PAT")
user  <- Sys.getenv("GIT_USER")
email <- Sys.getenv("GIT_EMAIL")

if (pat == "")
  stop("GITHUB_PAT environment variable not set")

log_message("Starting weekly git push")

# Configure git identity
git_config_global_set("user.name",  user)
git_config_global_set("user.email", email)

repo <- here()

# Stage all changes in data/
git_add("data", repo = repo)

# Check if there is anything to commit
status <- git_status(repo = repo)
staged <- status[status$staged, ]

if (nrow(staged) == 0) {
  log_message("No changes to commit — skipping push")
  quit(status = 0)
}

commit_msg <- glue("weekly data update {format(Sys.Date(), '%Y-%m-%d')}")
git_commit(commit_msg, repo = repo)
log_message(glue("Committed: {commit_msg}"))

# Push using PAT as password (username can be anything for token auth)
git_push(repo = repo, password = pat, verbose = TRUE)
log_message("Push complete")
