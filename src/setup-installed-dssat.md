# Installed DSSAT Setup

Start here if DSSAT is already installed on your computer. This page gets your
environment ready and confirms the wrapper can see DSSAT. The actual run is on
the next page, *Your First Run*.

## 1. What you need

- A **local DSSAT installation** — on Windows this is a folder such as
  `C:\DSSAT48` containing `DSCSM048.EXE` and crop folders (`Wheat`, `Maize`, …).
- **R** (4.1 or newer).
- These R packages (install once):

  ```r
  install.packages(c("DSSAT", "dplyr", "tidyr", "lubridate", "DBI", "RSQLite"))
  ```

## 2. Load the wrapper

You source **one** file; it loads the rest:

```r
source("R/DSSAT_omniwrapper.R")
```

Use a full path if you are not in the repository folder, e.g.
`source("C:/Users/you/DSSAT-wrapper/R/DSSAT_omniwrapper.R")`.

## 3. Confirm DSSAT is found (before running anything)

Run the self-check. It does **not** simulate — it just verifies the executable,
genotype files, experiment, situation and variable all exist, so you catch setup
problems early:

```r
chk <- DSSAT_omni_self_check(
  model_options = list(
    DSSAT_path   = "C:/DSSAT48",
    DSSAT_exe    = "DSCSM048.EXE",
    project_file = "C:/DSSAT48/Wheat/KSAS8101.WHX"
  ),
  situation    = "KSAS8101_1",
  required_var = "GSTD"
)
chk$ok      # TRUE means you are ready
chk$checks  # a table showing each item and whether it was found
```

If `chk$ok` is `TRUE`, continue to **[Your First Run](first-run.md)**. If not,
the `chk$checks` table names exactly which file or setting is missing.
