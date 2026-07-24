required_packages <- c("rmarkdown", "knitr", "DT", "plotly")
dir.create("r-library", showWarnings = FALSE, recursive = TRUE)
project_library <- normalizePath("r-library")
.libPaths(c(project_library, .libPaths()))
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org", lib = project_library)
}

cat("Required report packages are ready.\n")
