# DSSAT-wrapper — Architecture (read this first)

*Author: Victor Nyabuti Ong'era*

This wrapper runs **any** DSSAT crop family from R and returns tidy
observed-vs-simulated data. The original `DSSAT_wrapper.R` handled one crop; the
"omni" version (`DSSAT_omniwrapper`) handles all of them. Multi-family support is
why there is more code — but the flow is simple and always the same.

## The 30-second mental model

```
            you call DSSAT_omniwrapper(model_options, situation, var)
                                   │
          ┌────────────────────────┼─────────────────────────┐
          ▼                        ▼                          ▼
   1. RESOLVE              2. RUN                      3. PARSE
   (registry.R)           (run.R)                     (outputs.R)
   "what model is         "stage files, run            "read PlantGro.OUT,
    this crop?"            DSCSM048.EXE"                pick the variable you asked for"
          │                        │                          │
          └────────────────────────┴──────────────────────────┘
                                   ▼
                    list(sim_list = <data by situation>, error = FALSE)
```

## The four files (each does ONE job)

| File | Job | Key functions |
|------|-----|---------------|
| `DSSAT_omniwrapper.R` | **Entry point.** Sources the helpers, exposes the public function. Tiny. | `DSSAT_omniwrapper`, `DSSAT_omni_read_obs` |
| `dssat_omni_registry.R` | **Resolve.** Work out the crop, the 5-char model code, and the output adapter. | `dssat_omni_family_map` (the lookup table), `dssat_infer_model_options` (the decision logic — see its header comment) |
| `dssat_omni_run.R` | **Run.** Make a clean run directory, copy in the experiment + genotype + companion files, run the DSSAT executable. | `dssat_omni_run` |
| `dssat_omni_outputs.R` | **Parse.** Read DSSAT's `PlantGro.OUT`, translate a friendly variable name (e.g. `biomass`) to the family's real column (e.g. `CWAD`). | `dssat_variable_alias_map`, `dssat_resolve_requested_vars` |

## Two concepts that explain everything

1. **The family map** (`dssat_omni_family_map`): a small table mapping a 5-char
   model code → an *adapter* name. Example: `MZCER → CERES`, `CRGRO → CROPGRO`,
   `PRFRM → FORAGE`. The adapter decides which output columns/aliases to use.
   To support a new family you add one row here.

2. **DSSATPRO.V48 is the source of truth for "which model does this crop use".**
   The DSSAT install ships this file; we read it so we don't have to hard-code
   crop→model rules. This is what makes auto-detection work (you usually don't
   pass `module_code`). See the resolution-order comment atop `dssat_infer_model_options`.

## A worked trace (soybean)

1. You call with `project_file = .../Soybean/GAGR0201.SBX`.
2. **Resolve:** crop code `SB` → DSSATPRO says default module `CRGRO048` →
   model code `CRGRO` → family map says adapter `CROPGRO`.
3. **Run:** copy `GAGR0201.SBX` + genotype files into a temp run dir, run
   `DSCSM048.EXE`, producing `PlantGro.OUT`.
4. **Parse:** read `PlantGro.OUT`, return the `CWAD` column for situation
   `GAGR0201_1` as a `Date`-indexed data frame.

## Where to make common changes

- **Add a crop/family:** one row in `dssat_omni_family_map()`.
- **A family needs an extra input file** (e.g. forage `.MOW`): the companion-file
  staging block in `dssat_omni_run.R`.
- **A variable name maps to a different column for some family:** the
  `dssat_variable_alias_map()` switch in `dssat_omni_outputs.R`.

## Status of families
See `src/supported-families.md` for the validated family/crop table (45 crops
tested against a local DSSAT 4.8 install; 43 run from their shipped example with
no `module_code`).
