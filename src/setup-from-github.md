# Using GitHub-Sourced Example Data

**This page is optional.** A normal DSSAT install already ships example
experiments inside each crop folder (e.g. `C:/DSSAT48/Maize/UFGA8201.MZX`), so
for most users the *Installed DSSAT Setup* page is all you need.

Use this page only if you want to run experiments that are **not** in your DSSAT
folder — for example the public example datasets from the DSSAT source
repository.

## 1. Get the example data

Clone (or download) the DSSAT model repository, which contains example data:

```bash
git clone https://github.com/DSSAT/dssat-csm-os.git
```

The crop example files live under that checkout (e.g. a `Maize/UFGA8201.MZX`).

## 2. Point the wrapper at it

Keep `DSSAT_path` pointing at your **installed** DSSAT (it still provides the
executable and genotype files); set `project_file` to the experiment from your
downloaded data. Naming a different model is optional — here we ask for the
CERES-IXIM maize model with `module_code`:

```r
source("R/DSSAT_omniwrapper.R")

result <- DSSAT_omniwrapper(
  model_options = list(
    DSSAT_path   = "C:/DSSAT48",                               # installed DSSAT
    DSSAT_exe    = "DSCSM048.EXE",
    Crop         = "Maize",
    project_file = "C:/path/to/dssat-csm-os/Maize/UFGA8201.MZX",  # downloaded data
    module_code  = "MZIXM048"                                  # optional: force a model
  ),
  situation = "UFGA8201_1",
  var       = "CWAD"
)
```

## 3. Validation scripts and the data path

The family-validation script can read example experiments from such a checkout.
Tell it where they are with the `DSSAT_CSM_DATA` environment variable (see
*Self-Check and Validation*). If you do not set it, the scripts look for the data
next to the repository, and fall back to your DSSAT install — which, since DSSAT
ships its own examples, is usually enough on its own.
