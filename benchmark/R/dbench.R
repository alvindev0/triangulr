suppressPackageStartupMessages(library(tidyverse))


# Parameters
min <- 0
max <- 1
mode <- 0.5
sizes <- 10^(2:7)
rds_path <- "rds/dbench.rds"


# Benchmark
cat("\nBenchmarking Density Functions...\n")

pb <- txtProgressBar(min = 0, max = length(sizes), initial = 0)

if (file.exists(rds_path)) {
  status <- file.remove(rds_path)
}

for (i in 1:length(sizes)) {
  size <- sizes[i]

  x <- seq(1 / size, 1, 1 / size)

  suppressWarnings({
    dbench <- bench::mark(
      EnvStats::dtri(x      = x, min   = min, max   = max, mode  = mode),
      extraDistr::dtriang(x = x, a     = min, b     = max, c     = mode),
      fitODBOD::dTRI(p      = x,                           mode  = mode)$pdf,
      jmuOutlier::dtriang(x = x, min   = min, max   = max              ),
      mc2d::dtriang(x       = x, min   = min, max   = max, mode  = mode),
      metRology::dtri(x     = x, min   = min, max   = max, mode  = mode),
      propagate:::dtriang(x = x, a     = min, c     = max, b     = mode),
      triangle::dtriangle(x = x, a     = min, b     = max, c     = mode),
      triangulr::dtri(x     = x, min   = min, max   = max, mode  = mode),
      VaRES::dtriangular(x  = x, a     = min, b     = max, c     = mode),
      VGAM::dtriangle(x     = x, lower = min, upper = max, theta = mode),
      check = FALSE
    ) %>%
      mutate(size = size)
  })

  gc()

  if (file.exists(rds_path)) {
    read_rds(rds_path) %>%
      bind_rows(dbench) %>%
      write_rds(rds_path)
  } else {
    dbench %>%
      write_rds(rds_path)
  }

  setTxtProgressBar(pb, i)
}
