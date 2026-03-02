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

# Allow git to operate on volume-mounted repos owned by a different user
system2("git", c("config", "--global", "--add", "safe.directory", repo))

# Write credentials to git credential store, then clean up after all git ops
creds_file <- path.expand("~/.git-credentials")
writeLines(glue("https://{gh_user}:{pat}@github.com"), creds_file)
Sys.chmod(creds_file, "0600")
git_config_global_set("credential.helper", "store")

tryCatch({
  # Pull first to incorporate any remote changes before committing
  pull_result <- system2("git", c("-C", repo, "pull", "--rebase", "--autostash", "origin", "main"),
                         stdout = TRUE, stderr = TRUE)
  pull_exit <- attr(pull_result, "status")
  if (!is.null(pull_exit) && pull_exit != 0) {
    stop(paste(pull_result, collapse = "\n"))
  }
  log_message(paste("Pull complete:", paste(pull_result, collapse = " ")))

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

  result <- system2("git", c("-C", repo, "push", "origin", "main"),
                    stdout = TRUE, stderr = TRUE)
  exit_code <- attr(result, "status")
  if (!is.null(exit_code) && exit_code != 0) {
    stop(paste(result, collapse = "\n"))
  }
  log_message(paste("Push complete:", paste(result, collapse = " ")))
}, error = function(e) {
  log_message(glue("Failed: {conditionMessage(e)}"))
  stop(e)
}, finally = {
  file.remove(creds_file)
})
