#' Convert command arguments into a named list.
#'
#' Converts results from commandArgs() into a named character vector where the
#' names are parameter names and the values are the argument values from the
#' command line.
#'
#' `process_args` needs to know all allowable parameter names from the command
#' line.  Given the names of allowable parameters, the function:
#'
#' \itemize{
#'   \item Checks that all parameter=arguments pairs are formatted correctly
#'   (`--<parameter>=<argument>` or `--<parameter>`);
#'   \item Checks to be sure that all required parameter names are present;
#'   \item Checks to all parameters that need argument values have them;
#'   \item Checks to be sure that no unrecognized parameters exist;
#'   \item Inserts any missing parameters, assigning an argument value of "FALSE"
#'   \item Strips out the ignored parameters;
#'   \item Returns the complete parameter-argument pair values as a character
#'   vector of argument values named by parameter.
#' }
#'
#' @return A named character vector where the names are parameters names.  The
#'   values are "TRUE" if the parameter names was present, "FALSE" if the
#'   parameter name was absent, or the value after the `=` if assigned a value
#'   on in `args`.
#'
#'   If `args` contains a value `--verbose` the return vector is printed to the
#'   console unless `quietly` is set to TRUE.
#'
#' @param args Character vector in form of `--<parameter>`  or
#'   `--<parameter>=<value>`, usually derived using `commandArgs()[-1]` (note
#'   that the first value returned from commandArgs() is the location of the R
#'   executable, so it is removed from `args` using `[-1]`)
#' @param required_value_args A character vector of parameter names that are
#'   required on the command line and which must be assigned arguments values.
#' @param required_flag_args A character vector of parameter names that are
#'   required on the command line; these parameters may have, but do not require
#'   argument values.
#' @param optional_value_args A character vector of parameter names that are not
#'   required.  When present, these parameters must have argument values.
#' @param optional_value_args A character vector of parameter names that are not
#'   required.  When present, these parameters may have, but do not require,
#'   argument values.
#' @param ignored_args A character vector of parameter names that will be
#'   removed from the return value.
#' @param quietly Set to TRUE to suppress printing of arguments to console even
#'   if `args` contains '--verbose'
#'
#' @export
process_args <- function(args, required_value_args = character(0), required_flag_args = character(0),
                         optional_value_args = character(0), optional_flag_args = character(0),
                         ignored_args = c("no-echo", "no-restore", "file", "args", "no-save"),
                         quietly = FALSE)
{

  args <- args_to_named_vector(args)

  required_args <- c(required_value_args, required_flag_args)
  optional_args <- c(optional_value_args, optional_flag_args)
  need_value_args <- c(required_value_args, optional_value_args)
  valid_args <- c(required_args, optional_args, ignored_args)

  # check to be sure the required args are there...
  missing_args <- required_args[!required_args %in% names(args)]
  if(length(missing_args) > 0)
    stop(
      paste0(
        "The following required command line arguments are missing: ",
        paste0(missing_args, collapse = ", ")))

  invalid_args <- names(args)[!names(args) %in% valid_args]
  if(length(invalid_args) > 0 )
    stop("The following command line arguments are not recognized: ",
         paste0(invalid_args, collapse = ", "))

  # If optional and ignored args are NA, force to "TRUE"; missing, force to
  # "FALSE" (remember, args are of type character...)
  for(arg_name in valid_args) {
    if(arg_name %in% names(args)) {
      if(is.na(args[arg_name])) args[arg_name] = "TRUE"
    } else {
      args[arg_name] = "FALSE"
    }
  }

  badArgs <-
    sapply(
     need_value_args,
      function(x) args[x] == "TRUE", USE.NAMES = F)

  if(any(badArgs))
    stop("When present, the following command line args must have values: '",
         paste(need_value_args[badArgs], collapse = "', '"), "'.")

  args <- args[!names(args) %in% ignored_args]

  if(!is.na(args["verbose"])) {
    if(args["verbose"] == "TRUE" & !quietly) {
      print("## ARGUMENTS")
      print(args)
    }
  }

  args
}

args_to_named_vector <- function(args) {
  # check that all args are formatted correctly
  badArgs = !grepl("(^--[^[:space:]]+=[^[:space:]]+.+$)|(^--[^[:space:]]+$)", args)
  if(any(badArgs)) stop("Command line arguments must be in the form '--name=value' with no spaces around the '='")

  # split args on '='; get name of arg from before '=' and value from after
  args <- structure(sapply(strsplit(args, "="), `[`, 2), names = sapply(strsplit(args, "="), `[`, 1))
  # get rid of '--' from before arg name
  names(args) <- sub("^-+", "", names(args))
  # get rid of any args that are to be ignored.
  # args <- args[!names(args) %in% ignored_args]
  args
}
