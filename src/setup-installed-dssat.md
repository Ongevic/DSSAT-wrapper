# Installed DSSAT Setup

Use this path when DSSAT is already installed locally.

## Requirements

- a local DSSAT installation
- R
- required R packages:
  - `DSSAT`
  - `dplyr`
  - `tidyr`
  - `lubridate`

Install packages if needed:

```r
install.packages(c("DSSAT", "dplyr", "tidyr", "lubridate"))
```

## Source the wrapper

```r
source("R/DSSAT_omniwrapper.R")
```

## Minimal example

```r
result <- DSSAT_omniwrapper(
  model_options = list(
    DSSAT_path = "C:/path/to/DSSAT48",
    DSSAT_exe = "DSCSM048.EXE",
    project_file = "C:/path/to/DSSAT48/Wheat/KSAS8101.WHX",
    suppress_output = TRUE
  ),
  situation = "KSAS8101_1",
  var = "GSTD"
)
```
