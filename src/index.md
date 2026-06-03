# DSSAT-wrapper Guide

`DSSAT-wrapper` lets you run **any** DSSAT crop model from R and get tidy
observed-vs-simulated data back. You point it at your DSSAT installation and an
experiment file; it works out which model to use, runs DSSAT, and returns the
variable you asked for.

## Read the pages in this order

1. **[Installed DSSAT Setup](setup-installed-dssat.md)** — what you need and how
   to confirm the wrapper can see your DSSAT install. *Start here.*
2. **[Using GitHub-Sourced Example Data](setup-from-github.md)** — optional: run
   experiments from a separate data download instead of your DSSAT folder.
3. **[Your First Run](first-run.md)** — a full copy-paste example and how to read
   the result.
4. **[Self-Check and Validation](self-check-and-validation.md)** — how to test
   that your setup works, and what "testing DSSAT" actually means.
5. **[Supported Families](supported-families.md)** — every crop family the
   wrapper drives, with the example used to validate each.
6. **[Attribution and Disclaimer](attribution-and-license.md)**.

## If you only read one thing

You almost never need to tell the wrapper which model to use — it reads that from
your DSSAT install. A minimal run is:

```r
source("R/DSSAT_omniwrapper.R")
result <- DSSAT_omniwrapper(
  model_options = list(
    DSSAT_path   = "C:/DSSAT48",
    DSSAT_exe    = "DSCSM048.EXE",
    project_file = "C:/DSSAT48/Wheat/KSAS8101.WHX"
  ),
  situation = "KSAS8101_1",
  var       = "GSTD"
)
```

Developers who want to understand or extend the code should read
`ARCHITECTURE.md` in the repository root (the resolve → run → parse design).
