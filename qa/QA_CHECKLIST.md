# QA checklist

Automated sequence:
1. Nature `validate_figure.py` on the R source.
2. Run the R script in Binder RStudio.
3. Nature `audit_pdf_text.py --min-pt 5` on the exported PDF.
4. Inspect SVG/PDF/TIFF/PNG at final physical size.

Human audit:
- Exactly R04 + R18 only.
- Study HR/CI exactly match the locked source CSV.
- No M2, NRSI, LAUNCH, E1, RFS, TFS or TTP records.
- HR=1 reference line is visible.
- REML-Wald and REML-HKSJ both displayed.
- HKSJ CI is not cropped by the x-axis.
- Study square areas reflect inverse-variance weights.
- `n` is total randomized sample size for each study.
- Typography remains >=5 pt in exported PDF.
- No label/CI collisions or clipped text.
- Output remains legible in grayscale.
- Do not approve solely from automated QA.
