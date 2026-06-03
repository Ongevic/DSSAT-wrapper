# Supported Families

Crop → model-family mapping is taken from the DSSAT install's `DSSATPRO.V48`
(the authoritative crop-to-module map) and confirmed by running an example
experiment shipped in the DSSAT crop folders through `DSSAT_omniwrapper()`.

## Validated families

| Family | DSSAT module | Crops covered | Confirmed example (rows) |
|--------|--------------|---------------|--------------------------|
| `CERES` | MZCER / SGCER / MLCER / SWCER / BSCER / CSCER | maize, sorghum, pearl millet, sweetcorn, sugarbeet, barley, wheat | Maize `UFGA8201.MZX` (129); Sorghum `CSKU8201.SGX` (122) |
| `CERES-IXIM` | MZIXM | maize (alternative) | `UFGA8201.MZX` (129) |
| `CROPGRO` | CRGRO | soybean, peanut, drybean, cowpea, chickpea, faba/green bean, canola, cotton, tomato, bell pepper, cabbage, sunflower, safflower, lentil, pigeonpea, velvetbean, bambara groundnut, amaranth, chia, quinoa, hemp, guar, strawberry, carinata | Soybean `GAGR0201.SBX` (45) |
| `CROPSIM` | CSCRP | barley, wheat (Cropsim-CERES) | Barley `IEBR8201.BAX` (181) |
| `NWHEAT` | WHAPS / TFAPS | wheat (APSIM-NWheat), teff | Teff `ETGA0201.TFX` (208); Wheat `KSAS8101.WHX` (251) |
| `OILCROP` | SUOIL | sunflower | `AUTA8101.SUX` (136) |
| `SUGARCANE` (CANEGRO / CASUPRO / SAMUCA) | SCCAN / SCCSP / SCSAM | sugarcane | `ESAL1401.SCX` (328); SAMUCA `SAPO8601.SCX` (335) |
| `ALOHA` | PIALO | pineapple | `UHKN8901.PIX` (679) |
| `AROIDS` | TRARO / TNARO | taro, tanier | Taro `IBHK8901.TRX` (404) |
| `CSYCA` | CSYCA | cassava | `CCPA7801.CSX` (363) |
| `SUBSTOR` | PTSUB | potato | `AUCB7001.PTX` (135) |
| `RICE` | RICER | rice | `DTSP8502.RIX` (93) |
| `FORAGE` | PRFRM | alfalfa, bahia grass, bermuda grass, brachiaria, guinea grass | Alfalfa `AGZG1219.ALX` (1483); Bahia `TXEL7901.BHX` (610) |

**FORAGE note:** perennial-forage (`PRFRM`) experiments require their mowing/harvest
companion file (`<EXPERIMENT>.MOW`) to be staged into the run directory alongside the
experiment. The wrapper now stages `.MOW`/`.HAR` companions automatically, so forage
crops run without manual setup.

## Known blocker

- `CSCAS` (older cassava model): launches but the DSSAT executable exits with status 99.
  This is a **DSSAT-side** issue, not a wrapper problem — the same experiment
  (`Cassava/CCPA7801.CSX`) runs cleanly under `CSYCA`, the cultivars exist in
  `CSCAS048.CUL`, and all genotype files are present. Use `CSYCA` for cassava.

## Auto-detection

You normally do **not** need to pass `module_code`. The wrapper resolves the model from
the crop's default module in `DSSATPRO.V48` (the authoritative DSSAT crop→module map).
A model code parsed from the experiment header is only used when it is itself a registered
family — otherwise a blank/garbled `SMODEL` field would override the correct default.
With this resolution, **43 of 45 crop folders run from their shipped example with no
`module_code` supplied**.

Two shipped examples currently fail on their own data (not a wrapper or family issue):

- `Amaranth` — example experiment aborts with DSSAT status 99.
- `BambaraGroundnut` — DSSAT reports a missing input file for the shipped example.

Both belong to the working `CROPGRO` family (20+ other CROPGRO crops run), so these are
example-dataset problems, not family-support gaps.

## Not crop families

The DSSAT folders `ClimateChange`, `Seasonal`, `Sequence`, `Spatial`, and
`YieldForecast` are simulation/analysis modes, not crop models, and are intentionally
outside the family map.
