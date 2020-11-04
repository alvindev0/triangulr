suppressPackageStartupMessages(library(tidyverse))


# Parameters
min <- 0
max <- 1
mode <- 0.5
sizes <- 10^(2:5)
rds_path <- "rds/esbench.rds"


# Benchmark
cat("\nBenchmarking Expected Shortfall Functions...\n")

pb <- txtProgressBar(min = 0, max = length(sizes), initial = 0)

if (file.exists(rds_path)) {
  status <- file.remove(rds_path)
}

for (i in 1:length(sizes)) {
  size <- sizes[i]

  x <- seq(1 / size, 1, 1 / size)

  suppressWarnings({
    esbench <- bench::mark(
      triangulr::estri(p    = x, min = min, max = max, mode = mode),
      VaRES::estriangular(p = x, a   = min, b   = max, c    = mode),
      check = FALSE
    ) %>%
      mutate(size)
  })

  gc()

  if (file.exists(rds_path)) {
    read_rds(rds_path) %>%
      bind_rows(esbench) %>%
      write_rds(rds_path)
  } else {
    esbench %>%
      write_rds(rds_path)
  }

  setTxtProgressBar(pb, i)
}
