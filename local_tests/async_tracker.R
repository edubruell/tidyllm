# local_tests/async_tracker.R
# Persist async job state (batch IDs, research jobs) across sessions.
# Allows submit-in-one-session, check-in-another workflows.

.async_state_file <- "local_tests/async_state/jobs.json"

.load_jobs_index <- function() {
  if (!file.exists(.async_state_file)) return(list())
  jsonlite::read_json(.async_state_file)
}

.write_jobs_index <- function(jobs) {
  dir.create(dirname(.async_state_file), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(jobs, .async_state_file, auto_unbox = TRUE, pretty = TRUE)
}

#' Save an async job for later retrieval
save_async_job <- function(.job, .name, .provider, .type = "batch") {
  dir.create("local_tests/async_state", showWarnings = FALSE, recursive = TRUE)
  rds_path <- file.path("local_tests/async_state", paste0(.name, ".rds"))
  saveRDS(.job, rds_path)

  jobs <- .load_jobs_index()
  jobs <- Filter(function(j) j$name != .name, jobs)
  jobs <- c(jobs, list(list(
    name      = .name,
    provider  = .provider,
    type      = .type,
    status    = "pending",
    submitted = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    rds_path  = rds_path
  )))
  .write_jobs_index(jobs)
  invisible(.job)
}

#' List all async jobs as a tibble
list_async_jobs <- function() {
  jobs <- .load_jobs_index()
  if (length(jobs) == 0) {
    return(tibble::tibble(name = character(), provider = character(),
                          type = character(), status = character(),
                          submitted = as.POSIXct(character())))
  }
  tibble::tibble(
    name      = sapply(jobs, `[[`, "name"),
    provider  = sapply(jobs, `[[`, "provider"),
    type      = sapply(jobs, `[[`, "type"),
    status    = sapply(jobs, `[[`, "status"),
    submitted = as.POSIXct(sapply(jobs, `[[`, "submitted"))
  )
}

#' Load a saved job object by name
load_async_job <- function(.name) {
  jobs <- .load_jobs_index()
  entry <- Filter(function(j) j$name == .name, jobs)
  if (length(entry) == 0) stop(paste("No async job found with name:", .name))
  readRDS(entry[[1]]$rds_path)
}

#' Mark a job as complete and remove from index
complete_async_job <- function(.name) {
  jobs <- .load_jobs_index()
  jobs <- Filter(function(j) j$name != .name, jobs)
  .write_jobs_index(jobs)

  rds_path <- file.path("local_tests/async_state", paste0(.name, ".rds"))
  if (file.exists(rds_path)) unlink(rds_path)

  invisible(NULL)
}

#' Update the status of a job in the index
update_async_job_status <- function(.name, .status) {
  jobs <- .load_jobs_index()
  jobs <- lapply(jobs, function(j) {
    if (j$name == .name) j$status <- .status
    j
  })
  .write_jobs_index(jobs)
  invisible(NULL)
}

#' Return TRUE if a job with this name exists and is not yet completed
job_is_pending <- function(.name) {
  jobs <- .load_jobs_index()
  existing <- Filter(function(j) j$name == .name, jobs)
  length(existing) > 0 && existing[[1]]$status != "completed"
}

#' Check all pending batch jobs and update their status
check_all_async_jobs <- function() {
  devtools::load_all(quiet = TRUE)
  jobs <- .load_jobs_index()
  pending <- Filter(function(j) j$status == "pending", jobs)

  if (length(pending) == 0) {
    cat("No pending async jobs.\n")
    return(invisible(NULL))
  }

  for (j in pending) {
    cat(sprintf("Checking %s (%s/%s)... ", j$name, j$provider, j$type))
    tryCatch({
      job_obj <- readRDS(j$rds_path)
      provider_fn <- switch(j$provider,
        claude  = claude,
        openai  = openai,
        gemini  = gemini,
        mistral = mistral,
        groq    = groq,
        stop(paste("Unknown provider for async check:", j$provider))
      )
      status_tbl <- check_batch(job_obj, provider_fn())
      is_done <- !isTRUE(status_tbl$in_progress) &&
                 status_tbl$status %in% c("completed", "ended", "succeeded")
      new_status <- if (is_done) "completed" else "pending"
      update_async_job_status(j$name, new_status)
      cat(new_status, "\n")
    }, error = function(e) {
      cat(sprintf("error: %s\n", conditionMessage(e)))
    })
  }
  invisible(list_async_jobs())
}
