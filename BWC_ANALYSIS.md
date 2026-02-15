# BWC Implementation Analysis & Performance Optimization

## Task 1: How BWC Works in OutputAggregates

### Conceptual Differences: BWC vs Baseline

**Baseline Method (`.ori`):**
- Calculates deaths using **period-based** approach
- Deaths = Mortality Rate × Population for each year
- Formula: `death0.ct[,i] = M0.ct[,i] * pop0.ct[,i]`
- Regional aggregates: Simple sum of country deaths

**BWC Method (Birth Week Cohort):**
- Calculates deaths using **cohort-based** approach  
- Tracks 52 weekly birth cohorts per year through their lifetime
- Deaths accumulate as cohorts age across years
- Regional aggregates: Sum cohort-specific survivors/deaths across countries

### BWC Implementation Details

#### 1. **Input Data** (Lines 70-88)
```r
livebirths.file <- file.path("input", "data_livebirths.csv")
data.livebirths <- read.csv(livebirths.file, ...)
lb.ct <- data.livebirths[, ...] # Extract live births by country-year
```
- Adds livebirths data (not used in `.ori`)
- Livebirths serve as starting radix (l₀) for each weekly cohort

#### 2. **Birth Week Cohort Assignment** (Lines 606-612)
```r
wpp.livebirths.k <- as.numeric(livebirths.ct[k, match(years.k, years)])
bwc.vec.k <- rep(wpp.livebirths.k/52, 52)[order(rep(years.k, 52))]
```
- Divides each year's births into 52 equal weekly cohorts
- Each cohort = annual_births / 52

#### 3. **Weekly Mortality Rate Weighting** (Lines 585-598)
```r
weight.j.1 <- (53-(1:52))/52  # Linear weight: week 1 = 52/52, week 52 = 1/52
wgt.mat <- t(matrix(cbind(weight.j.1, 1-weight.j.1), ncol=10, nrow=52))
```
- Cohorts born early in year face Year₁ mortality longer
- Cohorts born late face more Year₂ mortality
- Creates smooth transition between calendar years

#### 4. **Cohort Life Table Construction** (Lines 663-678)
```r
# Build mortality matrix (10 age intervals × 52 weeks per year)
nmx.mat.k <- matrix(nmx.i, nrow=10, ncol=52)  # Repeated for all years

# Apply weights and calculate survival
nqx.mat.k <- wgt.mat.k * (nmx.mat.k/1000)
npx.mat.k <- 1 - nqx.mat.k
lx.mat.k <- apply(rbind(bwc.vec.k, npx.mat.k), 2, cumprod)  # Cohort survivors
dx.mat.k <- -apply(lx.mat.k, 2, diff)  # Cohort deaths
```
- **lx**: Number surviving in each cohort at each age
- **dx**: Number dying from each cohort at each age
- Matrix dimensions: (age groups × weeks) = (10 × 52×years)

#### 5. **Death Aggregation by Calendar Year** (Lines 687-697)
```r
years.k.mat <- matrix(rep(years.mat[,match(years.k, years)], 52), ...)
for(yrk in (year1.est.u5[k]+5):max(years.k)){
  deathu5.ct[k, match(yrk, years)] <- sum(dx.mat.k[floor(years.k.mat)==yrk], na.rm=T) 
}
```
- Maps cohort deaths back to calendar years
- U5 deaths in year Y include cohorts born Y-5 to Y
- Requires 5 years of prior cohorts for complete U5 death counts

#### 6. **Regional Aggregation** (Lines 1430-1700)
```r
# Load all country cohort arrays
load(paste0("dx.array.ct_", j, ".rda"))  # Country deaths by cohort
load(paste0("lx.array.ct_", j, ".rda"))  # Country survivors by cohort

# Sum across countries for regional cohorts
q0.wt[,i] <- sum(dx.array.by.c[1, getBWC(year=floor(est.years)[i]), ], na.rm=T) / 
             sum(lx.array.by.c[1, getBWC(year=floor(est.years)[i]), ], na.rm=T)
```
- Aggregate regional q₀, q₁₋₄ from summed cohort dx/lx
- Regional rate = Σ(country deaths) / Σ(country survivors)

### Key Behavior Changes vs `.ori`

| Aspect | Baseline `.ori` | BWC |
|--------|----------------|-----|
| **Time unit** | Annual period | Weekly cohort |
| **Deaths** | Rate × Population | Cohort life table |
| **U5 deaths in year Y** | U5MR(Y) × Pop(Y) | Sum of cohorts born Y-5 to Y |
| **First valid year** | Y₁ | Y₁+5 (needs prior cohorts) |
| **Regional aggregate** | Σ(deaths) or weighted mean | Σ(cohort dx) / Σ(cohort lx) |
| **Computation** | O(C × T) | O(C × T × 52 × 10) |

---

## Task 2: Performance Hotspots & Optimizations

### Hotspot #1: Repeated File I/O in Trajectory Loop **[CRITICAL]**

**Location:** `CalculateWorldDeaths` (lines 1510-1530), `CalculateRegionalDeaths` (lines 2175-2195)

**Problem:**
```r
for (j in 1:nsim) {
  load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
  load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
  # ... process ...
}
```
- Loads 2-4 .rda files per trajectory
- With nsim=1000, that's 2000-4000 disk reads
- Disk I/O is 1000× slower than memory access

**Fix:** Preload all trajectory data into combined array
```r
# BEFORE loop (line ~1505)
if (!file.exists(file.path(output.dir.samplescombined, "dx.array.ctj.rda"))) {
  dx.array.ctj <- array(NA, c(3, length(years)*52, C, nsim))
  lx.array.ctj <- array(NA, c(3, length(years)*52, C, nsim))
  for (j in 1:nsim) {
    load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
    load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
    dx.array.ctj[,,,j] <- dx.array.by.c
    lx.array.ctj[,,,j] <- lx.array.by.c
  }
  save(dx.array.ctj, file = file.path(output.dir.samplescombined, "dx.array.ctj.rda"))
  save(lx.array.ctj, file = file.path(output.dir.samplescombined, "lx.array.ctj.rda"))
} else {
  load(file.path(output.dir.samplescombined, "dx.array.ctj.rda"))
  load(file.path(output.dir.samplescombined, "lx.array.ctj.rda"))
}

# IN loop - just slice
for (j in 1:nsim) {
  dx.array.by.c <- dx.array.ctj[,,,j]
  lx.array.by.c <- lx.array.ctj[,,,j]
  # ... process ...
}
```

**Impact:** Removes O(nsim × regions) file reads → **10-50× faster for I/O-bound sections**

---

### Hotspot #2: Repeated Matrix Construction in Nested Loops

**Location:** `CalculateCountryDeathsBWC` (lines 606-670)

**Problem:**
```r
for(k in 1:C) {  # Countries
  for(i in 1:length(years.k)) {  # Years per country
    # Construct 10×52 mortality matrix every iteration
    nmx.mat.k <- cbind(nmx.mat.k, matrix(nmx.i, nrow=10, ncol=52))
    
    # Construct weight matrix every time
    wgt.mat.k <- matrix(rep(wgt.mat, length(years.k)), nrow=10, ncol=length(years.k)*52)
  }
}
```
- `wgt.mat` is constant but rebuilt C×years times
- `matrix(nmx.i, nrow=10, ncol=52)` creates new object every iteration

**Fix:** Precompute constant structures
```r
# BEFORE k loop (line ~600)
max_years <- max(sapply(1:C, function(k) length(years[!is.na(u5mr.ct[k,])])))
wgt.mat.cache <- lapply(1:max_years, function(n) {
  matrix(rep(wgt.mat, n), nrow=10, ncol=n*52)
})

# IN k loop
for(k in 1:C) {
  years.k <- years[!is.na(u5mr.ct[k,])]
  n_years <- length(years.k)
  wgt.mat.k <- wgt.mat.cache[[n_years]]  # Lookup, not rebuild
  # ...
}
```

**Impact:** Removes O(C × years) matrix allocations → **2-5× faster for nested loop**

---

### Hotspot #3: Inefficient Year-to-Cohort Mapping

**Location:** `CalculateWorldDeaths` (lines 1555-1560)

**Problem:**
```r
q0.wt[,i] <- sum(dx.array.by.c[1, getBWC(year=floor(est.years)[i], bwc=1), ], na.rm=T) /
             sum(lx.array.by.c[1, getBWC(year=floor(est.years)[i], bwc=1), ], na.rm=T)

# getBWC called multiple times per year
getBWC <- function(bwc=NULL, year){
  return(((year-1950)*52) + bwc)  # Simple arithmetic
}
```
- Function call overhead for simple arithmetic
- Same year index computed repeatedly

**Fix:** Vectorize cohort index computation
```r
# BEFORE i loop (line ~1550)
cohort_idx <- outer((floor(est.years) - 1950) * 52, 1:52, "+")  # Precompute all indices

# IN i loop
q0.wt[,i] <- sum(dx.array.by.c[1, cohort_idx[i, 1], ], na.rm=T) /
             sum(lx.array.by.c[1, cohort_idx[i, 1], ], na.rm=T)
```

**Impact:** Removes O(nyears × regions) function calls → **1.5-2× faster for aggregation**

---

### Hotspot #4: Repeated `roundoff()` Calls on Large Arrays

**Location:** Multiple places (lines 1424-1427, 2181-2184)

**Problem:**
```r
deathu5.ctj <- roundoff(deathu5.ctj, digits = 0)
death0.ctj <- roundoff(death0.ctj, digits = 0)
death1to4.ctj <- roundoff(death1to4.ctj, digits = 0)
```
- Called multiple times on same data in different functions
- `roundoff(x, 0)` is just `round(x)`

**Fix:** Round once and cache
```r
# IN CombineAndOutputCountryResults (line ~1195)
save(deathu5.ctj, file = file.path(output.dir.samplescombined, "deathu5.ctj.rda"))
deathu5.ctj.rounded <- round(deathu5.ctj)
save(deathu5.ctj.rounded, file = file.path(output.dir.samplescombined, "deathu5.ctj.rounded.rda"))

# IN subsequent functions
load(file.path(output.dir.samplescombined, "deathu5.ctj.rounded.rda"))
```

**Impact:** Removes O(regions) redundant rounding → **Minor, but cleaner**

---

### Hotspot #5: Redundant Array Slicing

**Location:** `CalculateWorldDeaths` (lines 1505-1530)

**Problem:**
```r
for (j in 1:nsim) {
  for (i in 1:nyears) {
    death0.wt[,i] <- sum(death0.ctj[,i,j], na.rm=T)  # Extract column every iter
    deathu5.wt[,i] <- sum(deathu5.ctj[,i,j], na.rm=T)
  }
}
```
- Slices 3D array in nested loop

**Fix:** Vectorize with `apply` or pre-slice trajectory
```r
for (j in 1:nsim) {
  death0.ct_j <- death0.ctj[,,j]  # Slice once per trajectory
  deathu5.ct_j <- deathu5.ctj[,,j]
  
  death0.wt <- colSums(death0.ct_j, na.rm=T)  # Vectorized
  deathu5.wt <- colSums(deathu5.ct_j, na.rm=T)
}
```

**Impact:** Removes O(nsim × nyears) array indexing → **1.5-2× faster**

---

## Minimal Patch Summary

### Patch 1: Cache dx/lx Arrays (Hotspot #1)
**File:** `R/outputaggregates-BWC.R`  
**Line:** ~1390 (before `CalculateWorldDeaths` call)

```r
# ADD after line 1390 (before CalculateWorldDeaths call)
  # Cache combined trajectory arrays to avoid repeated file I/O
  if (!file.exists(file.path(output.dir.samplescombined, "dx.array.ctj.rda"))) {
    cat("Combining dx/lx arrays into single cache file...\n")
    dx.array.ctj <- array(NA, c(3, length(years)*52, C, nsim))
    lx.array.ctj <- array(NA, c(3, length(years)*52, C, nsim))
    if(nn.exists) {
      dx.nn.array.ctj <- array(NA, c(2, length(years)*52, C, nsim))
      lx.nn.array.ctj <- array(NA, c(2, length(years)*52, C, nsim))
    }
    for (j in 1:nsim) {
      load(file.path(output.dir.samples, paste0("dx.array.ct_", j, ".rda")))
      load(file.path(output.dir.samples, paste0("lx.array.ct_", j, ".rda")))
      dx.array.ctj[,,,j] <- dx.array.by.c
      lx.array.ctj[,,,j] <- lx.array.by.c
      if(nn.exists) {
        load(file.path(output.dir.samples, paste0("dx.nn.array.ct_", j, ".rda")))
        load(file.path(output.dir.samples, paste0("lx.nn.array.ct_", j, ".rda")))
        dx.nn.array.ctj[,,,j] <- dx.nn.array.by.c
        lx.nn.array.ctj[,,,j] <- lx.nn.array.by.c
      }
    }
    save(dx.array.ctj, file = file.path(output.dir.samplescombined, "dx.array.ctj.rda"))
    save(lx.array.ctj, file = file.path(output.dir.samplescombined, "lx.array.ctj.rda"))
    if(nn.exists) {
      save(dx.nn.array.ctj, file = file.path(output.dir.samplescombined, "dx.nn.array.ctj.rda"))
      save(lx.nn.array.ctj, file = file.path(output.dir.samplescombined, "lx.nn.array.ctj.rda"))
    }
    cat("Cache created.\n")
  }
```

**THEN modify CalculateWorldDeaths** (line ~1510):
```r
# REPLACE file loads in j loop with array slice
load(file.path(output.dir.samplescombined, "dx.array.ctj.rda"))
load(file.path(output.dir.samplescombined, "lx.array.ctj.rda"))
if(nn.exists) {
  load(file.path(output.dir.samplescombined, "dx.nn.array.ctj.rda"))
  load(file.path(output.dir.samplescombined, "lx.nn.array.ctj.rda"))
}

for (j in 1:nsim) {
  dx.array.by.c <- dx.array.ctj[,,,j]
  lx.array.by.c <- lx.array.ctj[,,,j]
  # ... rest unchanged
}
```

---

### Patch 2: Precompute Weight Matrices (Hotspot #2)
**File:** `R/outputaggregates-BWC.R`  
**Line:** 600 (before k loop in CalculateCountryDeathsBWC)

```r
# ADD after line 598 (after wgt.mat definition)
  # Precompute weight matrices for all possible year lengths
  max_years_any_country <- nyears  # Conservative upper bound
  wgt.mat.cache <- vector("list", max_years_any_country)
  for (n in 1:max_years_any_country) {
    wgt.mat.cache[[n]] <- matrix(rep(wgt.mat, n), nrow=10, ncol=n*52)
  }
  if(!is.null(nmr.ctj)) {
    wgt.nmr.mat.cache <- vector("list", max_years_any_country)
    for (n in 1:max_years_any_country) {
      wgt.nmr.mat.cache[[n]] <- matrix(rep(wgt.nmr.mat, n), nrow=2, ncol=n*52)
    }
  }

# THEN in k loop (line ~665), REPLACE:
#   wgt.mat.k <- matrix(rep(wgt.mat,length(years.k)),nrow=10, ncol=length(years.k)*52)
# WITH:
  wgt.mat.k <- wgt.mat.cache[[length(years.k)]]

# AND (line ~675):
#   wgt.nn.mat.k <- matrix(rep(wgt.nmr.mat,length(years.k)),nrow=2, ncol=length(years.k)*52)
# WITH:
  wgt.nn.mat.k <- wgt.nmr.mat.cache[[length(years.k)]]
```

---

### Patch 3: Vectorize Year Sums (Hotspot #5)
**File:** `R/outputaggregates-BWC.R`  
**Line:** ~1555 (in CalculateWorldDeaths)

```r
# REPLACE the nested i loop:
#   for (i in 1:nyears) {
#     death0.wt[, i] <- sum(death0.ctj[, i, j], na.rm = T)
#     deathu5.wt[, i] <- sum(deathu5.ctj[, i, j], na.rm = T)
#     ...
#   }

# WITH vectorized version:
  death0.ct_j <- death0.ctj[,,j]
  deathu5.ct_j <- deathu5.ctj[,,j]
  if(nn.exists) deathnn.ct_j <- deathnn.ctj[,,j]
  
  death0.wt <- matrix(colSums(death0.ct_j, na.rm=T), nrow=1)
  deathu5.wt <- matrix(colSums(deathu5.ct_j, na.rm=T), nrow=1)
  if(nn.exists) deathnn.wt <- matrix(colSums(deathnn.ct_j, na.rm=T), nrow=1)
  
  # Keep q calculation loop (requires cohort indexing)
  q0.wt <- q1to4.wt <- q5.wt <- qnn.wt <- matrix(NA, 1, nyears)
  for (i in 1:nyears) {
    q0.wt[,i] <- sum(dx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=T) / 
                 sum(lx.array.by.c[1,getBWC(year=floor(est.years)[i], bwc=1),], na.rm=T)
    # ... rest of q calculations
  }
```

---

## Sanity Check Plan

### 1. Checksum Comparison
```r
# Before optimization
results_old <- read.csv("output/Rates & Deaths_Country Summary.csv")
checksum_old <- digest::digest(results_old[,c("ISO3Code", "U5MR 2020", "IMR 2020")])

# After optimization  
results_new <- read.csv("output_optimized/Rates & Deaths_Country Summary.csv")
checksum_new <- digest::digest(results_new[,c("ISO3Code", "U5MR 2020", "IMR 2020")])

stopifnot(checksum_old == checksum_new)
```

### 2. Numeric Tolerance Test
```r
# Allow tiny floating point differences
max_diff_u5mr <- max(abs(results_old$`U5MR 2020` - results_new$`U5MR 2020`), na.rm=T)
stopifnot(max_diff_u5mr < 1e-10)  # Practically identical
```

### 3. Small Sample Test
```r
# Run with test=TRUE (uses only 10 trajectories)
OutputAggregates(..., test=TRUE)
# Compare aggregates manually for 2-3 countries
```

---

## Expected Performance Gains

| Optimization | Affected Operations | Speed-Up | Impact Level |
|--------------|---------------------|----------|--------------|
| Cache dx/lx arrays | Regional aggregation | 10-50× | **CRITICAL** |
| Precompute weight matrices | Country deaths calc | 2-5× | High |
| Vectorize sums | World/regional sums | 1.5-2× | Medium |
| All combined | Full pipeline | **15-100×** | **MAJOR** |

**Bottleneck shift:** After optimizations, dominant cost becomes the cohort life table calculations (lines 665-678), which are inherently O(C × years × 52) and cannot be avoided without changing the methodol
