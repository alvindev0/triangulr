suppressPackageStartupMessages(library(tidyverse))


# Parameters
min <- 0
max <- 1
mode <- 0.5
sizes <- 10^(2:7)
rds_path <- "rds/qbench.rds"


# Benchmark
cat("\nBenchmarking Quantile Functions...\n")

pb <- txtProgressBar(min = 0, max = length(sizes), initial = 0)

if (file.exists(rds_path)) {
  status <- file.remove(rds_path)
}

for (i in 1:length(sizes)) {
  size <- sizes[i]

  x <- seq(1 / size, 1, 1 / size)

  suppressWarnings({
    qbench <- bench::mark(
      EnvStats::qtri(p       = x, min   = min, max   = max,  mode = mode),
      extraDistr::qtriang(p  = x, a     = min, b     = max,     c = mode),
      jmuOutlier::qtriang(p  = x, min   = min, max   = max              ),
      mc2d::qtriang(p        = x, min   = min, max   = max,  mode = mode),
      metRology::qtri(p      = x, min   = min, max   = max,  mode = mode),
      triangle::qtriangle(p  = x, a     = min, b     = max,     c = mode),
      triangulr::qtri(p      = x, min   = min, max   = max,  mode = mode),
      VaRES::vartriangular(p = x, a     = min, b     = max,     c = mode),
      VGAM::qtriangle(p      = x, lower = min, upper = max, theta = mode),
      check = FALSE
    ) %>%
      mutate(size)
  })

  gc()

  if (file.exists(rds_path)) {
    read_rds(rds_path) %>%
      bind_rows(qbench) %>%
      write_rds(rds_path)
  } else {
    qbench %>%
      write_rds(rds_path)
  }

  setTxtProgressBar(pb, i)
}
