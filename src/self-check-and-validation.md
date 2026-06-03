# Self-Check and Validation

"Testing DSSAT" here means two different things. Know which one you want.

| You want to check… | Use | Answers |
|--------------------|-----|---------|
| **Is my setup wired up?** (is DSSAT found, are files in place?) | the **self-check** | "are the executable, genotype files, experiment, situation and variable all present?" |
| **Does the wrapper actually drive DSSAT correctly across crops?** | the **validation scripts** | "do real experiments run and return data?" |

## A. Self-check — "is my setup correct?"

This runs **no** simulation. It just confirms every required file/setting exists,
so you can fix setup problems before running anything. From R:

```r
source("R/DSSAT_omniwrapper.R")
chk <- DSSAT_omni_self_check(
  model_options = list(
    DSSAT_path   = "C:/DSSAT48",
    DSSAT_exe    = "DSCSM048.EXE",
    project_file = "C:/DSSAT48/Wheat/KSAS8101.WHX"
  ),
  situation    = "KSAS8101_1",
  required_var = "GSTD"
)
chk$ok        # TRUE if everything needed is present
chk$checks    # a table: each item, the path checked, and exists = TRUE/FALSE
```

Or run the ready-made script from a terminal:

```bash
Rscript R/test_DSSAT_omni_self_check.R
```

You should see a table ending in `Self-check overall status: OK`. Any `exists =
FALSE` row on a `required = TRUE` item tells you exactly what to fix.

## B. Run-it-for-real tests

These actually launch DSSAT and check that data comes back.

```bash
# one experiment end-to-end
Rscript R/test_DSSAT_omniwrapper.R

# observed-data reading
Rscript R/test_DSSAT_omni_read_obs.R

# every model family (pineapple, cassava, sugarcane, forage, …)
Rscript R/validate_DSSAT_omniwrapper_families.R
```

`validate_DSSAT_omniwrapper_families.R` is the big one: it runs one example per
model family and stops with an error if any family fails, so a clean finish means
all validated families work on your machine.

## C. The two environment variables

The scripts need to know where DSSAT and its example data live. Set these once per
session (or let them default):

| Variable | What it points to | Default if unset |
|----------|-------------------|------------------|
| `DSSAT_PATH` | your DSSAT install folder | `C:/DSSAT48` |
| `DSSAT_CSM_DATA` | folder of example experiments (often the same DSSAT folder, since it ships examples) | the repo's `examples/` then `../dssat-csm-data` |

Set them in R:

```r
Sys.setenv(DSSAT_PATH = "C:/DSSAT48", DSSAT_CSM_DATA = "C:/DSSAT48")
```

…or in a terminal before calling `Rscript`:

```bash
# Windows PowerShell
$env:DSSAT_PATH = "C:/DSSAT48"; $env:DSSAT_CSM_DATA = "C:/DSSAT48"
Rscript R/validate_DSSAT_omniwrapper_families.R
```

A standard DSSAT install already contains the example experiments the validator
needs, so pointing both variables at `C:/DSSAT48` is usually enough.
