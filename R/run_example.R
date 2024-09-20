#' Run package examples
#'
#' @param example_name Name of the example file to run
#' @return No return value, called for side effects.
#'
#' @examples
#' \donttest{
#' run_example("sequence_multidyads_examples.R")
#' run_example("main_functions_examples.R")
#' }
#'
#' @export
run_example <- function(example_name) {
  example_dir <- system.file("examples", package = "conversim")
  example_file <- file.path(example_dir, example_name)

  if (!dir.exists(example_dir)) {
    stop("Examples directory not found in the package.")
  }

  available_examples <- list.files(example_dir)

  if (!file.exists(example_file)) {
    stop("Example file '", example_name, "' not found. ",
         "Available examples: ", paste(available_examples, collapse = ", "))
  }

  source(example_file, local = TRUE)
}
