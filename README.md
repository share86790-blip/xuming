# HCC Meta-analysis — Step 3C-1 M1 RCT-OS (Nature-R)

This repository is an R-only reproducibility package for the formally frozen M1 RCT-OS forest plot.

## Locked source
- Formal pool: R04 Gu 2020 + R18 Fan/TREAT 2024 only.
- Parent locked dataset SHA256: `65cd85b3a1cf15c9068ca5bd0cb3cb124d4617a1c1c8793137538fb5b9faecc8`
- M1 source CSV SHA256: `482cdaa0f1d21f20f59356d75f2b4a6d222d994d0cfaaa2db18ef9800a3bae80`

## Nature skill
Binder clones `Yuan1z0825/nature-skills` and pins commit:
`44defbcce0b8534f9a0a4734f56c40e4f703bbf4`

The selected `nature-figure` backend is **R**.

## Run in browser
Open:

`https://mybinder.org/v2/gh/share86790-blip/xuming/main?urlpath=rstudio`

Then in the RStudio console run:

```r
source("R/run_all.R")
```

The script performs Nature source preflight, renders the figure in R, and audits PDF text size.

## Expected outputs
- `outputs/M1_model_results.csv`
- `outputs/M1_study_results.csv`
- `outputs/Figure_M1_RCT_OS_Nature_R.svg`
- `outputs/Figure_M1_RCT_OS_Nature_R.pdf`
- `outputs/Figure_M1_RCT_OS_Nature_R.tiff`
- `outputs/Figure_M1_RCT_OS_Nature_R.png`

## Important
No Python plotting is allowed. Python is used only for the Nature skill's non-visual source/PDF QA utilities.
