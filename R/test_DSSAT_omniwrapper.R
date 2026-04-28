# Simple integration checks for the registry-driven DSSAT omniwrapper prototype.
# These tests exercise multiple DSSAT model families using shipped examples.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 0) {
  stop("Unable to determine script location from commandArgs().")
}
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
source(file.path(script_dir, "DSSAT_omniwrapper.R"))

resolve_dssat_path <- function() {
  env_path <- Sys.getenv("DSSAT_PATH", unset = "")
  candidates <- c(
    env_path,
    "C:/DSSAT48"
  )
  for (candidate in candidates) {
    if (nzchar(candidate) && dir.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Could not locate a DSSAT installation. Set the DSSAT_PATH environment variable.")
}

resolved_dssat_path <- resolve_dssat_path()

run_case <- function(label, project_file, situation, var_name, dssat_path = NULL) {
  if (is.null(dssat_path)) {
    dssat_path <- resolved_dssat_path
  }
  cat("\n===", label, "===\n")
  model_options <- list(
    DSSAT_path = dssat_path,
    DSSAT_exe = "DSCSM048.EXE",
    project_file = project_file,
    suppress_output = TRUE
  )

  result <- DSSAT_omniwrapper(
    model_options = model_options,
    situation = situation,
    var = var_name
  )

  sim <- result$sim_list[[situation]]
  cat("Rows:", nrow(sim), "\n")
  cat("Columns:", paste(names(sim), collapse = ", "), "\n")
  if (nrow(sim) > 0) {
    cat("First date:", as.character(sim$Date[1]), "\n")
    cat("Last date:", as.character(sim$Date[nrow(sim)]), "\n")
  }
  cat("Error flag:", result$error, "\n")

  if (result$error || nrow(sim) == 0 || !(var_name %in% names(sim))) {
    stop("Prototype check failed for ", label, ".")
  }

  invisible(result)
}

run_case(
  label = "CERES wheat",
  project_file = file.path(resolved_dssat_path, "Wheat", "KSAS8101.WHX"),
  situation = "KSAS8101_1",
  var_name = "GSTD"
)

run_case(
  label = "SUBSTOR potato",
  project_file = file.path(resolved_dssat_path, "Potato", "AUCB7001.PTX"),
  situation = "AUCB7001_1",
  var_name = "TWAD"
)

run_case(
  label = "CROPGRO soybean",
  project_file = file.path(resolved_dssat_path, "Soybean", "CLMO8501.SBX"),
  situation = "CLMO8501_1",
  var_name = "CWAD"
)

run_case(
  label = "CROPGRO hemp",
  project_file = file.path(resolved_dssat_path, "Hemp", "UFDSS0301.HMX"),
  situation = "UFDSS0301_1",
  var_name = "CWAD"
)

cat("\nAll omniwrapper prototype checks completed successfully.\n")
