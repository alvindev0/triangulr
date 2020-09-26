suppressPackageStartupMessages(library(tidyverse))


# Parameters
min <- 0
max <- 1
mode <- 0.5
sizes <- 10^(2:7)
rds_path <- "rds/rbench.rds"


# Benchmark
cat("\nBenchmarking Random Variate Generator Functions...\n")

if (file.exists(rds_path)) {
  status <- file.remove(rds_path)
}

pb <- txtProgressBar(min = 0, max = length(sizes), initial = 0)

for (i in 1:length(sizes)) {
  size <- sizes[i]

  suppressWarnings({
    rbench <- bench::mark(
      EnvStats::rtri(n      = size, min   = min, max   = max, mode  = mode),
      extraDistr::rtriang(n = size, a     = min, b     = max, c     = mode),
      jmuOutlier::rtriang(n = size, min   = min, max   = max              ),
      mc2d::rtriang(n       = size, min   = min, max   = max, mode  = mode),
      metRology::rtri(n     = size, min   = min, max   = max, mode  = mode),
      OOmisc::rtriangular(n = size, a     = min,              b     = mode),
      propagate:::rtriang(n = size, a     = min, c     = max, b     = mode),
      Runuran::urtriang(n   = size, a     = min, b     = max, m     = mode),
      triangle::rtriangle(n = size, a     = min, b     = max, c     = mode),
      triangulr::rtri(n     = size, min   = min, max   = max, mode  = mode),
      VGAM::rtriangle(n     = size, lower = min, upper = max, theta = mode),
      check = FALSE
    ) %>%
      mutate(size)
  })

  if (file.exists(rds_path)) {
    read_rds(rds_path) %>%
      bind_rows(rbench) %>%
      write_rds(rds_path)
  } else {
    rbench %>%
      write_rds(rds_path)
  }

  setTxtProgressBar(pb, i)

  gc()
}
