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

resolve_dssat_csm_data <- function() {
  env_path <- Sys.getenv("DSSAT_CSM_DATA", unset = "")
  candidates <- c(
    env_path,
    normalizePath(file.path(script_dir, "..", "examples", "dssat-csm-data"), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(script_dir, "..", "..", "dssat-csm-data"), winslash = "/", mustWork = FALSE)
  )
  for (candidate in candidates) {
    if (nzchar(candidate) && dir.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Could not locate dssat-csm-data. Set the DSSAT_CSM_DATA environment variable.")
}

dssat_path <- resolve_dssat_path()
dssat_csm_data <- resolve_dssat_csm_data()
project_file <- file.path(dssat_csm_data, "Hemp", "UFCI2101.HMX")

model_options <- list(
  DSSAT_path = dssat_path,
  DSSAT_exe = "DSCSM048.EXE",
  project_file = project_file,
  suppress_output = TRUE
)

obs_list <- DSSAT_omni_read_obs(
  model_options = model_options,
  situation = c("UFCI2101_1", "UFCI2101_2", "UFCI2101_3"),
  read_end_season = TRUE
)

cat("Observed situations:", paste(names(obs_list), collapse = ", "), "\n")
for (nm in names(obs_list)) {
  obs <- obs_list[[nm]]
  cat("\n===", nm, "===\n")
  cat("Rows:", nrow(obs), "\n")
  cat("Columns:", paste(names(obs), collapse = ", "), "\n")
}

if (length(obs_list) != 3 || any(vapply(obs_list, nrow, integer(1)) == 0)) {
  stop("DSSAT_omni_read_obs did not return non-empty observations for the Hopf hemp cases.")
}

cat("\nObservation path regression test completed successfully.\n")
