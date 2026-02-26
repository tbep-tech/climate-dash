# Weekly git commit and push for climate-dash data updates
# Intended to be run by a weekly cron job inside the Docker container.
# Requires environment variables: GITHUB_PAT, GITHUB_USERNAME, GIT_USER, GIT_EMAIL

librarian::shelf(gert, here, glue, quiet = TRUE)

log_txt <- "/var/log/shiny-server/climate_git_push.log"

log_message <- function(msg) {
  message <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "~", msg)
  cat(message, "\n", file = log_txt, append = TRUE)
  cat(message, "\n")
}

pat      <- Sys.getenv("GITHUB_PAT")
gh_user  <- Sys.getenv("GITHUB_USERNAME")
user     <- Sys.getenv("GIT_USER")
email    <- Sys.getenv("GIT_EMAIL")

if (pat == "")
  stop("GITHUB_PAT environment variable not set")
if (gh_user == "")
  stop("GITHUB_USERNAME environment variable not set")

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
  log_message("No new changes to commit — will still attempt push for any unpushed commits")
} else {
  commit_msg <- glue("weekly data update {format(Sys.Date(), '%Y-%m-%d')}")
  git_commit(commit_msg, repo = repo)
  log_message(glue("Committed: {commit_msg}"))
}

# Embed credentials in remote URL (required by libgit2 for PAT auth)
original_url <- git_remote_info("origin", repo = repo)$url
auth_url <- glue("https://{gh_user}:{pat}@github.com/tbep-tech/climate-dash.git")
git_remote_set_url(repo, "origin", auth_url)

tryCatch({
  git_push(repo = repo, verbose = TRUE)
  log_message("Push complete")
}, error = function(e) {
  log_message(glue("Push failed: {conditionMessage(e)}"))
  stop(e)
}, finally = {
  # Always restore clean URL (no credentials) regardless of success/failure
  git_remote_set_url(repo, "origin", original_url)
})
