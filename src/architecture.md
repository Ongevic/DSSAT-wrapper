# Architecture (for developers)

*Author: Victor Nyabuti Ong'era*

This page explains how the code is organised, for anyone who wants to understand
or extend it. If you only want to *run* the wrapper, the earlier pages are enough.

## The 30-second mental model

```
            you call DSSAT_omniwrapper(model_options, situation, var)
                                   │
          ┌────────────────────────┼─────────────────────────┐
          ▼                        ▼                          ▼
   1. RESOLVE              2. RUN                      3. PARSE
   (registry.R)           (run.R)                     (outputs.R)
   "what model is         "stage files, run            "read PlantGro.OUT,
    this crop?"            DSCSM048.EXE"                pick the variable asked for"
          │                        │                          │
          └────────────────────────┴──────────────────────────┘
                                   ▼
                    list(sim_list = <data by situation>, error = FALSE)
```

## The four files (each does one job)

| File | Job | Key functions |
|------|-----|---------------|
| `DSSAT_omniwrapper.R` | **Entry point.** Sources the helpers, exposes the public function. | `DSSAT_omniwrapper`, `DSSAT_omni_read_obs` |
| `dssat_omni_registry.R` | **Resolve.** Work out the crop, the 5-char model code, and the output adapter. | `dssat_omni_family_map`, `dssat_infer_model_options` |
| `dssat_omni_run.R` | **Run.** Make a clean run directory, copy in the experiment + genotype + companion files, run DSSAT. | `dssat_omni_run` |
| `dssat_omni_outputs.R` | **Parse.** Read `PlantGro.OUT` and translate a friendly variable name (e.g. `biomass`) to the family's real column (e.g. `CWAD`). | `dssat_variable_alias_map`, `dssat_resolve_requested_vars` |

## Two concepts that explain everything

1. **The family map** (`dssat_omni_family_map`): a small table mapping a 5-char
   model code → an *adapter* name (e.g. `MZCER → CERES`, `CRGRO → CROPGRO`,
   `PRFRM → FORAGE`). The adapter decides which output columns/aliases to use.
   To support a new family you add one row here.

2. **DSSATPRO.V48 is the source of truth for "which model does this crop use".**
   The DSSAT install ships this file; the wrapper reads it instead of hard-coding
   crop→model rules. This is what makes auto-detection work (you usually do not
   pass `module_code`).

## A worked trace (soybean)

1. You call with `project_file = .../Soybean/GAGR0201.SBX`.
2. **Resolve:** crop code `SB` → DSSATPRO default module `CRGRO048` → model code
   `CRGRO` → family map adapter `CROPGRO`.
3. **Run:** copy `GAGR0201.SBX` + genotype files into a temp run dir, run
   `DSCSM048.EXE`, producing `PlantGro.OUT`.
4. **Parse:** read `PlantGro.OUT`, return the `CWAD` column for situation
   `GAGR0201_1` as a `Date`-indexed data frame.

## Where to make common changes

- **Add a crop/family:** one row in `dssat_omni_family_map()`.
- **A family needs an extra input file** (e.g. forage `.MOW`): the companion-file
  staging block in `dssat_omni_run.R`.
- **A variable maps to a different column for some family:** the
  `dssat_variable_alias_map()` switch in `dssat_omni_outputs.R`.
