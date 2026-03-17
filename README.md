# xtpretest

**Comprehensive Panel Data Pre-Testing Suite**

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/xtpretest)](https://CRAN.R-project.org/package=xtpretest)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## Overview

`xtpretest` provides a single function, `xtpretest()`, that runs a comprehensive
pre-testing suite for panel data models. It helps researchers choose the correct
estimator (Pooled OLS, FE, MG, CCEMG) before running their main regression.

### Tests Implemented

| Module        | Test                                        | Reference             |
|---------------|---------------------------------------------|-----------------------|
| `hsiao`       | F1 (overall), F2 (slopes), F3 (intercepts)  | Hsiao (2014)          |
| `robust`      | HC1 robust Wald variants of F2, F3          | —                     |
| `heterogeneity` | Swamy chi-squared                         | Swamy (1970)          |
| `csd`         | Pesaran CD test on FE residuals             | Pesaran (2004)        |
| `summary`     | Between/within variance decomposition       | —                     |

## Installation

```r
# From CRAN
install.packages("xtpretest")

# Development version
remotes::install_github("muhammedalkhalaf/xtpretest")
```

## Usage

```r
library(xtpretest)

set.seed(1)
df <- data.frame(
  id   = rep(1:5, each = 10),
  time = rep(1:10, times = 5),
  y    = rnorm(50),
  x1   = rnorm(50),
  x2   = rnorm(50)
)

# Run all pre-tests
res <- xtpretest(df, y ~ x1 + x2, index = c("id", "time"))

# Run specific modules
res <- xtpretest(df, y ~ x1, index = c("id", "time"),
                 tests = c("hsiao", "csd"))
```

## References

- Hsiao, C. (2014). *Analysis of Panel Data* (3rd ed.). Cambridge University
  Press. <https://doi.org/10.1017/CBO9781139839327>
- Swamy, P. A. V. B. (1970). Efficient inference in a random coefficient
  regression model. *Econometrica*, 38(2), 311–323.
- Pesaran, M. H. (2004). General diagnostic tests for cross section dependence
  in panels. Cambridge Working Paper No. 0435.

## Author

Muhammad Alkhalaf  
<muhammedalkhalaf@gmail.com>  
ORCID: 0009-0002-2677-9246
