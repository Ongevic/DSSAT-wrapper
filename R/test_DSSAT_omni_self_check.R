# Preflight checks for the registry-driven DSSAT omniwrapper.

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) == 0) {
  stop("Unable to determine script location from commandArgs().")
}
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
script_dir <- dirname(script_path)
source(file.path(script_dir, "DSSAT_omniwrapper.R"))

check <- DSSAT_omni_self_check(
  model_options = list(
    DSSAT_path = "C:/DSSAT48",
    DSSAT_exe = "DSCSM048.EXE",
    project_file = "C:/DSSAT48/Wheat/KSAS8101.WHX",
    suppress_output = TRUE
  ),
  situation = "KSAS8101_1",
  required_var = "GSTD"
)

print(check$checks)
cat("\nSelf-check overall status:", if (isTRUE(check$ok)) "OK" else "FAILED", "\n")

if (!isTRUE(check$ok)) {
  stop("DSSAT omniwrapper self-check failed.")
}
