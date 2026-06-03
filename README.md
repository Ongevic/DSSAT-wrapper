# DSSAT-wrapper

`DSSAT-wrapper` is a public R wrapper project for running DSSAT experiments in a
more reproducible and more extensible way.

This fork preserves the original `DSSAT_wrapper()` associated with the AgMIP
Calibration Phase III workflow and adds a cleaner, registry-driven
`DSSAT_omniwrapper()` for multi-family DSSAT support.

It is best described as a practical implementation that builds on the original
AgMIP wrapper direction while extending it for broader DSSAT family coverage and
reproducible wrapper workflows.

## What this fork adds

- a public `DSSAT_omniwrapper()` entry point
- modular internals for registry discovery, run staging, and output parsing
- broader cross-family validation
- support for alternate engines such as `MZIXM048`, `WHAPS048`, `TFAPS048`,
  `SUOIL048`, `SCCSP048`, and `SCSAM048`
- observation reading from either installed DSSAT crop folders or external
  project directories
- a focused `mdBook` guide for setup, validation, and contribution

## Validated families

`DSSAT_omniwrapper()` has been validated across all the standard DSSAT 4.8 crop
families — CERES, CROPGRO, CROPSIM, FORAGE, SUGARCANE (CANEGRO/CASUPRO/SAMUCA),
RICE, SUBSTOR, ALOHA, AROIDS, CSYCA, NWHEAT, OILCROP and CERES-IXIM — by running
a shipped example for every crop in a local DSSAT install.

The one current exception is **`CSCAS`** (older cassava model): it launches but
DSSAT's own executable stops with a model/data error, so use `CSYCA` for cassava.

The full crop-by-crop table, with the example used and row counts, lives in the
guide so there is a single source of truth:
[Supported Families](src/supported-families.md).

## What this repository does not include

- a bundled DSSAT installation
- bundled DSSAT source code
- bundled `dssat-csm-data`
- bundled experiment outputs from one private machine

To use the wrapper, you should provide your own DSSAT installation and, when
needed, your own clone or download of public experiment data such as
`dssat-csm-data`.

## Quick start

Install the main R dependencies:

```r
install.packages(c("DSSAT", "dplyr", "tidyr", "lubridate"))
```

Source the omniwrapper from the repository root:

```r
source("R/DSSAT_omniwrapper.R")
```

Example with an installed DSSAT folder:

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

Example with external example data:

```r
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

## Validation scripts

These scripts are included so users can verify their own setup:

- `R/test_DSSAT_omniwrapper.R`
- `R/test_DSSAT_omni_read_obs.R`
- `R/validate_DSSAT_omniwrapper_families.R`

They use:

- `DSSAT_PATH` for the local DSSAT installation
- `DSSAT_CSM_DATA` for an optional clone of `dssat-csm-data`

If `DSSAT_PATH` is not set, the scripts try the common Windows default
`C:/DSSAT48`.

## Documentation

The repository includes a focused `mdBook` guide in `src/`, covering:

- setup from an installed DSSAT folder
- setup with GitHub-sourced example data
- your first run
- self-check and validation
- supported families
- attribution and disclaimer

For developers, `ARCHITECTURE.md` explains how the code is organised
(resolve → run → parse) and where to make common changes.

## Attribution

This fork builds on the original DSSAT wrapper work associated with the AgMIP
Calibration Phase III effort.

The header of the original wrapper credits:

- Jing Qi
- Amir Souissi
- Samuel Buis
- Vakhtang Shelia

This fork preserves that lineage while adding the omniwrapper architecture,
broader family validation, and public-facing documentation.

## License and disclaimer

At the time of writing, the checked upstream wrapper snapshot used for this fork
does not include an explicit `LICENSE` file. For that reason, this repository
keeps attribution clear and avoids claiming license terms that are not stated in
the upstream snapshot.

This work is shared in good faith for research and educational use, but it is
provided as-is without warranty, and users are responsible for checking whether
it fits their own workflows, data, and redistribution needs.
