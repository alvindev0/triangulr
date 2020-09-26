# Benchmarks for Functions for the Triangular Distribution in R

**NOTE**: Functions from [distr6](https://alan-turing-institute.github.io/distr6/) and [tfprobability](https://rstudio.github.io/tfprobability/) packages will not be included as they do not work without requiring a distribution object.


## Methodology

I used [bench](bench.r-lib.org) for speed and memory benchmarking. The bench package is preferred as it provides a number of advantages over other alternatives, such as tracking of memory allocation and number of garbage collections. More info: [bench.r-lib.org](http://bench.r-lib.org/)


## Environment

R 4.0.2

Package    | Version
---------- | -------
EnvStats   | 2.3.1
extraDistr | 1.9.1
fitODBOD   | 1.4.1-1
jmuOutlier | 2.2
mc2d       | 0.1-18
metRology  | 0.9-28-1
OOmisc     | 1.2
propagate  | 1.0-6
Runuran    | 0.30
triangle   | 0.12
triangulr  | 1.0.1
VaRES      | 1.0
VGAM       | 1.1-3

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
