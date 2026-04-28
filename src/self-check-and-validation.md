# Self-Check and Validation

The wrapper includes validation scripts so users can confirm their setup.

## Main checks

- `R/test_DSSAT_omni_self_check.R`
- `R/test_DSSAT_omniwrapper.R`
- `R/test_DSSAT_omni_read_obs.R`
- `R/validate_DSSAT_omniwrapper_families.R`

## Environment variables

- `DSSAT_PATH`
- `DSSAT_CSM_DATA`

If `DSSAT_PATH` is not set, the scripts try `C:/DSSAT48`.
