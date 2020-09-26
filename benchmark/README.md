# Benchmarks for Functions for the Triangular Distribution in R

**NOTE**: Functions from [distr6](https://alan-turing-institute.github.io/distr6/) and [tfprobability](https://rstudio.github.io/tfprobability/) packages will not be included as they do not work without requiring a distribution object.


## Methodology

I used [bench](bench.r-lib.org) for speed and memory benchmarking. The bench package is preferred as it provides a number of advantages over other alternatives, such as tracking of memory allocation and number of garbage collections. More info: [bench.r-lib.org](http://bench.r-lib.org/)


## Environment

R 4.0.2

Component |	Value
--------- | -----
CPU Model | AMD Ryzen 5 3600 @ 3.6 Ghz
CPU Cores | 6
RAM Model | DIMM Synchronous 3200 MHz
RAM GB    | 16


## Density Functions

### 100K Quantiles
![](plot/dbench_5.png)

### 1M Quantiles
![](plot/dbench_6.png)

### 10M Quantiles
![](plot/dbench_7.png)

### Median Time
![](plot/dbench_time.png)

### Memory
![](plot/dbench_mem.png)


## Distribution Functions

### 100K Quantiles
![](plot/pbench_5.png)

### 1M Quantiles
![](plot/pbench_6.png)

### 10M Quantiles
![](plot/pbench_7.png)

### Median Time
![](plot/pbench_time.png)

### Memory Allocation
![](plot/pbench_mem.png)


## Quantile Functions

### 100K Probabilities
![](plot/qbench_5.png)

### 1M Probabilities
![](plot/qbench_6.png)

### 10M Probabilities
![](plot/qbench_7.png)

### Median Time
![](plot/qbench_time.png)

### Memory Allocation
![](plot/qbench_mem.png)


## Random Variate Generator Functions

### 100K Random Variates
![](plot/rbench_5.png)

### 1M Random Variates
![](plot/rbench_6.png)

### 10M Random Variates
![](plot/rbench_7.png)

### Median Time
![](plot/rbench_time.png)

### Memory Allocation
![](plot/rbench_mem.png)


## Expected Shortfall Functions

### 100K Probabilities
![](plot/esbench_5.png)

### Median Time
![](plot/esbench_time.png)

### Memory Allocation
![](plot/esbench_mem.png)
