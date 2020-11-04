suppressPackageStartupMessages(library(tidyverse))


# Parameters
min <- 0
max <- 1
mode <- 0.5
sizes <- 10^(2:7)
rds_path <- "rds/pbench.rds"


# Benchmark
cat("\nBenchmarking Distribution Functions...\n")

pb <- txtProgressBar(min = 0, max = length(sizes), initial = 0)

if (file.exists(rds_path)) {
  status <- file.remove(rds_path)
}

for (i in 1:length(sizes)) {
  size <- sizes[i]

  x <- seq(1 / size, 1, 1 / size)

  suppressWarnings({
    pbench <- bench::mark(
      EnvStats::ptri(q      = x, min   = min, max   = max, mode  = mode),
      extraDistr::ptriang(q = x, a     = min, b     = max, c     = mode),
      fitODBOD::pTRI(p      = x,                           mode  = mode),
      jmuOutlier::ptriang(q = x, min   = min, max   = max              ),
      mc2d::ptriang(q       = x, min   = min, max   = max, mode  = mode),
      metRology::ptri(q     = x, min   = min, max   = max, mode  = mode),
      triangle::ptriangle(q = x, a     = min, b     = max, c     = mode),
      triangulr::ptri(q     = x, min   = min, max   = max, mode  = mode),
      VaRES::ptriangular(x  = x, a     = min, b     = max, c     = mode),
      VGAM::ptriangle(q     = x, lower = min, upper = max, theta = mode),
      check = FALSE
    ) %>%
      mutate(size)
  })

  gc()

  if (file.exists(rds_path)) {
    read_rds(rds_path) %>%
      bind_rows(pbench) %>%
      write_rds(rds_path)
  } else {
    pbench %>%
      write_rds(rds_path)
  }

  setTxtProgressBar(pb, i)
}
