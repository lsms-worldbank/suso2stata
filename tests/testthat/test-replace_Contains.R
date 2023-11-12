testthat::test_that("Replaces `.ContainsAll` with Stata equivalent", {
    testthat::expect_equal(
        replace_ContainsAll("M13_Q02==1 && M13_Q04_curr.ContainsAll(3, 4)"),
        "M13_Q02==1 && (M13_Q04_curr__3 == 1 & M13_Q04_curr__4 == 1)"
    )
})

testthat::test_that("Returns unmodified string if no Contains found", {
    testthat::expect_equal(
        replace_Contains("M13_Q02==1 && M13_Q04_curr == 3"),
        "M13_Q02==1 && M13_Q04_curr == 3"
    )
})

testthat::test_that("Replaces multiple `.ContainsAll` with Stata equivalent", {
    testthat::expect_equal(
        replace_Contains("M13_Q04_curr.Contains(1) || M13_Q04_curr.Contains(3)"),
        "M13_Q04_curr__1 == 1 || M13_Q04_curr__3 == 1"
    )
})

testthat::test_that("Handles a vector of length > 1", {
    testthat::expect_equal(
        replace_Contains(c("var.Contains(1)", "a >= 1")),
        c("var__1 == 1", "a >= 1")
    )
})

testthat::test_that("Handles NA inputs", {
    testthat::expect_equal(
        replace_Contains(c("var.Contains(1)", NA)),
        c("var__1 == 1", NA)
    )
})
