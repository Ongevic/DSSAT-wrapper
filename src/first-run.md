# Your First Run

The fastest useful first test is a known shipped example.

```r
source("R/DSSAT_omniwrapper.R")

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

Check:

- did the run finish without error?
- did you get a non-empty time series?
- is the requested variable present?
