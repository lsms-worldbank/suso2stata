
<!-- README.md is generated from README.Rmd. Please edit that file -->

# suso2stata

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of suso2stata is to translate enablement and validation
conditions from Survey Solutions’ dialect of C# into their nearest
equivalent in Stata through intelligent string interpolation.

## Installation

Since suso2stata is not yet available on CRAN, it can be installed from
GitHub as follows:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("lsms-worldbank/suso2stata")
```

## Usage

Translate one expression element at a time:

``` r
# Translate SuSo's `InList`, method that contains values inside `()`...
my_suso_expr <- "M4_Q01c_Amt != 0 && M4_Q01.InList(1,4,6,7,96)"

# ... to Stata's `inlist`, function whose parameters are a variable and values
my_stata_expr <- suso2stata::replace_InList(my_suso_expr)

# before
my_suso_expr
#> [1] "M4_Q01c_Amt != 0 && M4_Q01.InList(1,4,6,7,96)"
# after
my_stata_expr
#> [1] "M4_Q01c_Amt != 0 && inlist(M4_Q01, 1,4,6,7,96)"
```

Translate all expression elements in a pipeline:

``` r
# start with a complex SuSo expression that includes several
# several SuSo-specific syntactic elements
my_complex_suso_expr <- "IsAnswered(M22A_Q10_qty_A) && M22A_Q09_A.ContainsAny(1, 2, 4)"

my_complex_stata_expr <- my_complex_suso_expr |>
  # first, replace `IsAnswered` with `!mi()`
  suso2stata::replace_IsAnswered() |>
  # next, replace `&&` with `&`
  suso2stata::replace_and() |>
  # then, replace `ContainsAny` with its Stata equivalent
  suso2stata::replace_ContainsAny()

# before
my_complex_suso_expr
#> [1] "IsAnswered(M22A_Q10_qty_A) && M22A_Q09_A.ContainsAny(1, 2, 4)"
# after
my_complex_stata_expr
#> [1] "!mi(M22A_Q10_qty_A) & (M22A_Q09_A__1 == 1 | M22A_Q09_A__2 == 1 | M22A_Q09_A__4 == 1)"
```
