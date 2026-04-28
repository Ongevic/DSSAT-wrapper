# Using GitHub-Sourced Example Data

You can keep DSSAT installed locally while pointing `project_file` at example
files from a separate checkout such as `dssat-csm-data`.

```r
source("R/DSSAT_omniwrapper.R")

result <- DSSAT_omniwrapper(
  model_options = list(
    DSSAT_path = "C:/path/to/DSSAT48",
    DSSAT_exe = "DSCSM048.EXE",
    Crop = "Maize",
    project_file = "C:/path/to/dssat-csm-data/Maize/UFGA8201.MZX",
    module_code = "MZIXM048",
    suppress_output = TRUE
  ),
  situation = "UFGA8201_1",
  var = "CWAD"
)
```
