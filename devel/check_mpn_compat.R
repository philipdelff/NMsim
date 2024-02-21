install.packages("flextable",repos="https://cloud.r-project.org")
install.packages("purrr",repos="https://cloud.r-project.org")
library(remotes)
install_github("pharmaR/riskmetric")
install_github("metrumresearchgroup/mpn.scorecard")

library(mpn.scorecard)

results_dir <- score_pkg(
    ## pkg = "~/wdirs/NMsim_0.0.9.901.tar.gz",
    pkg = "~/wdirs/NMsim_0.0.10.tar.gz",
    ## pkg = "~/wdirs/NMdata_0.1.4.tar.gz",
    out_dir = file.path(tempdir(), "results"),
    overwrite=TRUE
)


results_dir

pdf_path <- render_scorecard(
  results_dir = results_dir,
  risk_breaks = c(0.3, 0.7),
  overwrite=T
)

browseURL(pdf_path)

