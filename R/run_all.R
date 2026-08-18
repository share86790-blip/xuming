preflight <- system2(
  "python3",
  c(
    "vendor/nature-skills/skills/nature-figure/scripts/validate_figure.py",
    "R/step3c1_m1_rct_os_nature.R",
    "--backend", "r"
  )
)
if (!identical(preflight, 0L)) {
  stop("Nature source preflight did not pass. Review validator output before rendering.")
}

source("R/step3c1_m1_rct_os_nature.R")

pdf_audit <- system2(
  "python3",
  c(
    "vendor/nature-skills/skills/nature-figure/scripts/audit_pdf_text.py",
    "outputs/Figure_M1_RCT_OS_Nature_R.pdf",
    "--min-pt", "5"
  )
)
if (!identical(pdf_audit, 0L)) {
  stop("PDF text audit did not pass. Do not approve the figure.")
}

cat("\nNature QA automation completed. Final-size human visual audit is still required.\n")
