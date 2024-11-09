#' Batch process a function that takes command arguments.
#'
#' This function is meant for batch processing of any function that accepts
#' command line arguments.  It can be nested within an \code{_apply} style
#' function and passed a list of command args for repeated batch processing. To
#' operate, \code{batch} wraps any function specified as \code{fun} and, in
#' doing so, add error trapping, recovery, and the potential to either log error
#' and warning messages or report to a webhook.
#'
#' For local batch processing (e.g., using mclapply), set report = F.
#'
#' Setting \code{msg_log_dir} = NULL is primarily for debugging a single call to
#' batch, as reporting to the console isn't ideal for a batched function.
#'
#' A note on parallel processing... \code{batch} has been tested extensively
#' with \code{mclapply} for parallel processing. The safest approach is to embed
#' \code{mclapply} in a script called from the command prompt using
#' \code{Rscript} (Google "Rscript command line" for more.) Parallel processing
#' using \code{mclapply} to call \code{batch} from within Rstudio often works,
#' but can cause Rstudio to freeze (perhaps only if screen saver engages??).  In
#' this case, batch processing usually proceeds successfully even after RStudio
#' is frozen, but RStudio must be terminated manually, potentially leading to
#' loss of code.
#'
#' @param args() Character vector of command line arguments to simulate
#'   operation from the command line.  Must be in the form
#'   `--<parameter>=<argument>` (or `--<parameter>` for boolean switch). By
#'   default, the values returned from commandArgs().
#' @param fun The function to be called.  Must accept command line arguments in
#'   a parameter called 'args' and should return a named list of values to be
#'   reported to the web hook.
#' @param key_arg The name of an argument that is to be included in
#'   error/warning messages to identify the specific run causing the
#'   error/warning.
#' @param ... Other arguments passed to fun.
#' @param report Determines whether or not the results from the function are
#'   written to the webhook specified by environmental variable called specified
#'   in \code{webhook_var}.  When TRUE, messages are reported to webhook, so
#'   local logging is disabled (used for server-based processing).  Generally,
#'   set to FALSE for local processing. Must be FALSE to log messages locally or
#'   report messages to console.
#' @param error_report A named list of values that is appended to the webhook
#'   report.  Used for reporting errors in lieu of the values that would have
#'   been returned from `fun` if there had not been an error.  Set to NULL or
#'   list() if no argument is to be included in the error messages.
#' @param webhook_var Name of environment variable containing the URL for the
#'   reporting webhook.
#' @param msg_log_dir local directory where messages will be logged.  Directory
#'   must exist.  Set to NULL to report messages to console rather than log to
#'   files (useful for debugging). NOTE: \code{report} argument must be FALSE
#'   for any logging to occur (see \code{report} argument).

#' @export
batch <- function(args = commandArgs(), fun, key_arg, ...,
                  report = FALSE, error_report = list(),
                  webhook_var = "BATCH_WEBHOOK", msg_log_dir = "batch_message_log") {

  if(any(grepl("^--verbose(=|$)", args))) message("Begin batch processing")

  withCallingHandlers(
    {
      # tryCatch catches error and terminates gracefully
      tryCatch(
        {
          # these three variables are globals needed for makeMessage() and
          # reporting
          original_args <- args
          batch_result <- "Success"
          batch_messages <- character(0)

          if(is.null(key_arg)) {
            key_arg <- ""
          } else {
            key_arg <- get_arg_value(original_args, key_arg)
          }
          script_file <- get_arg_value(original_args, "file")

          fun_name <- as.character(substitute(fun))
          if(fun_name[1] == "::") fun_name <- fun_name[3]

          if(!is.null(msg_log_dir) & !report)
            if(!dir.exists(msg_log_dir)) stop("Message log directory does not exist.")

          # list(
          #   resultType = result_type,
          #   warningAndErrorMessages = messages,
          #   time = paste(as.POSIXct(Sys.time(), tz="UTC"), "UTC"),
          #   responseType = response_type,
          #   data = file_path
          # )

          if(any(grepl("^--verbose(=|$)", original_args))) message(paste0("Calling ", fun_name, "()..."))
          report_values <- fun(args, ...)
          if(any(grepl("^--verbose(=|$)", original_args))) message(paste0("Reporting results from ", fun_name, "()..."))
          report_values <-
            c(
              list(
                resultType = batch_result,
                source = script_file,
                warningAndErrorMessages = batch_messages,
                time = paste(as.POSIXct(Sys.time(), tz="UTC"), "UTC"),
                commandLineArgs = original_args),
              report_values)
          # if we are reporting to a webhook, don't send the results to the console.
          if(report) {
            report_result(report_values, webhook_var)
            invisible(report_values)
          } else {
            # otherwise send the results to the console
            return(report_values)
          }
        },
        error = function(err_obj) {
          if(any(grepl("^--verbose(=|$)", original_args))) {
            message("ERROR ENCOUNTERED...")
            message(err_obj)
          }

          batch_messages[length(batch_messages)+1] <<- makeMessage(err_obj, "Error", key_arg)
          batch_result <<- "Error"
          report_values <-
            c(
              list(
                resultType = batch_result,
                source = script_file,
                warningAndErrorMessages = batch_messages,
                time = paste(as.POSIXct(Sys.time(), tz="UTC"), "UTC"),
                commandLineArgs = original_args),
              error_report)
          if(report) {
            if(any(grepl("^--verbose(=|$)", original_args))) message("Reporting error results to webhook...")
            report_result(report_values, webhook_var)
          } else if (!is.null(msg_log_dir)) {
            if(any(grepl("^--verbose(=|$)", original_args))) message("Logging error message...")
            log_message(tail(batch_messages, 1), msg_log_dir, paste0("Error in batch(", fun_name, ")"))
          } else {
            print(report_values)
            stop(tail(batch_messages, 1))
          }
          report_values
        }
      )
    },
    warning = function(warning_obj) {
      if(any(grepl("^--verbose(=|$)", original_args))) {
        message("WARNING ISSUED...")
        message(warning_obj)
      }

      batch_result <<- "Warning"
      warn_message <- makeMessage(warning_obj, "Warning", key_arg)
      batch_messages[length(batch_messages)+1] <<- warn_message
      if(!report) {
        if (!is.null(msg_log_dir)) {
          if(any(grepl("^--verbose(=|$)", original_args))) message("Logging warning...")
          log_message(warn_message, msg_log_dir, paste0("Warning in batch(", fun_name, ")"))
        } else {
          message(warn_message)
        }
      }
      invokeRestart("muffleWarning")
    }
  )

}

get_arg_value <- function(args, key_arg) {
  arg_matches <- grepl(paste0("^--", key_arg, "(=|$)"), args)
  if(sum(arg_matches) > 1) stop("More than one argument matches '", key_arg, "'")
  if(sum(arg_matches) == 0) return("FALSE")

  key_arg <- strsplit(args[arg_matches], "=")[[1]]
  if(length(key_arg) > 2) stop("More than one '=' associated with argument '", key_arg[1], "'")
  if(length(key_arg) == 1) return("TRUE")
  return(key_arg[2])
}

log_message <- function(message, log_dir, type) {
  if(!dir.exists(log_dir)) dir.create(log_dir)
  readr::write_lines(
    message,
    file.path(
      log_dir,
      paste0(gsub(":", ".", as.character(Sys.time())), " ", type, ".txt"))
  )
}

# helper function that add a message from warning or error object `x`
makeMessage <- function(x, type, key_arg) {
  paste0(
    type, " in '", paste(deparse(x$call), collapse = ""),
    "'",
    ifelse(key_arg == "", ": ", paste0(" ", key_arg, ": ")),
    x$message)
}

report_result <- function(body_list, webhook_var) {
  url <- Sys.getenv(webhook_var)
  req <-
    httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body_list)
  resp <- httr2::req_perform(req)
}
