# Your First Run

This page assumes you have never used the wrapper before. Follow it top to bottom.

## 0. What you need first

1. **DSSAT installed** on your computer. On Windows this is usually a folder like
   `C:\DSSAT48` that contains `DSCSM048.EXE` and crop folders (`Wheat`, `Maize`, …).
2. **R** (4.1 or newer).
3. **The R packages** the wrapper uses. Install them once:

   ```r
   install.packages(c("DSSAT", "dplyr", "tidyr", "lubridate", "DBI", "RSQLite"))
   ```

## 1. Load the wrapper

You only ever source **one** file — it loads the rest for you:

```r
source("R/DSSAT_omniwrapper.R")
```

(Use the full path if you are not in the repository folder, e.g.
`source("C:/Users/you/DSSAT-wrapper/R/DSSAT_omniwrapper.R")`.)

## 2. Run a shipped example (copy–paste)

Every DSSAT install ships example experiments. Here we run the wheat one. You do
**not** need to know the model name — the wrapper figures it out from your DSSAT
install (this is "auto-detection").

```r
result <- DSSAT_omniwrapper(
  model_options = list(
    DSSAT_path   = "C:/DSSAT48",                       # <- your DSSAT folder
    DSSAT_exe    = "DSCSM048.EXE",                      # the DSSAT program
    project_file = "C:/DSSAT48/Wheat/KSAS8101.WHX",    # an experiment file (.??X)
    suppress_output = TRUE                             # hide DSSAT console spam
  ),
  situation = "KSAS8101_1",   # which treatment: <experiment name>_<number>
  var       = "GSTD"          # the variable you want back (here: growth stage)
)
```

## 3. Look at what you got back

`result` is a list with two parts:

```r
result$error                       # FALSE means the run succeeded
sim <- result$sim_list[["KSAS8101_1"]]   # the data for your situation
head(sim)                          # a data frame: Date + the variable you asked for
nrow(sim)                          # how many daily rows came back
```

A good first run means: `result$error` is `FALSE`, `nrow(sim)` is greater than 0,
and your variable (`GSTD` here) is one of the columns.

## 4. Run it for YOUR OWN data

Change three things in `model_options`:

- `DSSAT_path`   → the folder where DSSAT is installed on your machine.
- `project_file` → your experiment file (a `.??X` file, e.g. `.SBX` soybean,
  `.MZX` maize, `.WHX` wheat). It can be inside the DSSAT crop folder or your own.
- `situation`    → `"<experiment file name without extension>_<treatment number>"`,
  e.g. the first treatment of `MYEXP01.SBX` is `"MYEXP01_1"`.

If you want a different output variable, change `var` (e.g. `"CWAD"` for above-ground
biomass, `"HWAM"` for yield). You can also use a friendly name like `"biomass"` or
`"yield"` — the wrapper maps it to the right column for that crop.

## 5. If something goes wrong

Run the **self-check** first — it tells you exactly which file or setting is missing
before you blame the model. See *Self-Check and Validation*.
