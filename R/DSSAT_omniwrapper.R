#' @title Registry-Driven DSSAT Omniwrapper
#'
#' @description
#' Public entry point for the multi-family DSSAT wrapper. This file keeps the
#' user-facing API small and readable by sourcing the internal helper modules
#' that handle registry discovery, output parsing, and run orchestration.

dssat_omni_this_file <- function() {
  for (frame_index in rev(seq_len(sys.nframe()))) {
    frame <- sys.frame(frame_index)
    if (!is.null(frame$ofile)) {
      return(normalizePath(frame$ofile, winslash = "/", mustWork = TRUE))
    }
  }
  normalizePath("R/DSSAT_omniwrapper.R", winslash = "/", mustWork = FALSE)
}

dssat_omni_source_helpers <- function() {
  current_file <- dssat_omni_this_file()
  current_dir <- dirname(current_file)
  helper_files <- c(
    "dssat_omni_registry.R",
    "dssat_omni_outputs.R",
    "dssat_omni_run.R"
  )

  for (helper in helper_files) {
    helper_path <- file.path(current_dir, helper)
    if (!file.exists(helper_path)) {
      stop("Required omniwrapper helper file not found: ", helper_path)
    }
    source(helper_path, local = parent.frame())
  }
}

if (!exists("DSSAT_omniwrapper", mode = "function") || !exists("DSSAT_omni_read_obs", mode = "function")) {
  dssat_omni_source_helpers()
}
