testthat::test_that("Replaces `.ContainsAny` with Stata equivalent", {
    testthat::expect_equal(
        replace_ContainsAny("M22A_Q10_qty_A >0 && M22A_Q09_A.ContainsAny(1, 2, 4)"),
        "M22A_Q10_qty_A >0 && (M22A_Q09_A__1 == 1 | M22A_Q09_A__2 == 1 | M22A_Q09_A__4 == 1)"
    )
})

testthat::test_that("Returns unmodified string if no ContainsAny found", {
    testthat::expect_equal(
        replace_ContainsAny("M13_Q02==1 && M13_Q04_curr == 3"),
        "M13_Q02==1 && M13_Q04_curr == 3"
    )
})

testthat::test_that("Replaces multiple `.ContainsAny` with Stata equivalent", {
    testthat::expect_equal(
        replace_ContainsAny("M13_Q04_curr.ContainsAny(1, 2) || M13_Q04_curr.ContainsAny(3, 5)"),
        "(M13_Q04_curr__1 == 1 | M13_Q04_curr__2 == 1) || (M13_Q04_curr__3 == 1 | M13_Q04_curr__5 == 1)"
    )
})

testthat::test_that("Handles a vector of length > 1", {
    testthat::expect_equal(
        replace_ContainsAny(c("var.ContainsAny(1, 2)", "a >= 1")),
        c("(var__1 == 1 | var__2 == 1)", "a >= 1")
    )
})
