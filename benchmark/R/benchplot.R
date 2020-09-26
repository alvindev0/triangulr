suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(bench)
})

cat("\nPlotting Results...\n")

for (prefix in c("d", "p", "q", "r", "es")) {
  rds <- read_rds(glue("rds/{prefix}bench.rds"))

  for (len in unique(rds$size[rds$size >= 1e5])) {
    p <- rds %>%
      filter(size == len) %>%
      select(-size) %>%
      autoplot() +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        legend.position = "bottom"
      )

    ggsave(glue("plot/{prefix}bench_{log10(len)}.png"), p, width = 10, height = 5)
  }

  rds <- rds %>%
    filter(size >= 1e3) %>%
    rowwise() %>%
    mutate(expression = deparse(expression)[[1]]) %>%
    ungroup() %>%
    transmute(
      pkg = str_extract(expression, "[^:]*"),
      size, median, mem_alloc
    )

  p <- rds %>%
    mutate(pkg = fct_reorder2(pkg, size, median)) %>%
    ggplot(aes(x = size, y = median, color = pkg)) +
    geom_point() +
    geom_line() +
    scale_x_log10() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      legend.title = element_blank()
    )

  ggsave(glue("plot/{prefix}bench_time.png"), p, width = 10, height = 5)

  p <- rds %>%
    mutate(pkg = fct_reorder2(pkg, size, mem_alloc)) %>%
    ggplot(aes(x = size, y = mem_alloc, color = pkg)) +
    geom_point() +
    geom_line() +
    scale_x_log10() +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      legend.title = element_blank()
    )

  ggsave(glue("plot/{prefix}bench_mem.png"), p, width = 10, height = 5)
}
