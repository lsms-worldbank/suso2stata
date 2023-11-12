testthat::test_that("Replaces `.ContainsAll` with Stata equivalent", {
    testthat::expect_equal(
        replace_ContainsAll("M22A_Q10_qty_A >0 && M22A_Q09_A.ContainsAll(1, 2, 4)"),
        "M22A_Q10_qty_A >0 && (M22A_Q09_A__1 == 1 & M22A_Q09_A__2 == 1 & M22A_Q09_A__4 == 1)"
    )
})

testthat::test_that("Returns unmodified string if no ContainsAll found", {
    testthat::expect_equal(
        replace_ContainsAll("M13_Q02==1 && M13_Q04_curr == 3"),
        "M13_Q02==1 && M13_Q04_curr == 3"
    )
})

testthat::test_that("Replaces multiple `.ContainsAll` with Stata equivalent", {
    testthat::expect_equal(
        replace_ContainsAll("M13_Q04_curr.ContainsAll(1, 2) || M13_Q04_curr.ContainsAll(3, 5)"),
        "(M13_Q04_curr__1 == 1 & M13_Q04_curr__2 == 1) || (M13_Q04_curr__3 == 1 & M13_Q04_curr__5 == 1)"
    )
})

testthat::test_that("Handles a vector of length > 1", {
    testthat::expect_equal(
        replace_ContainsAll(c("var.ContainsAll(1, 2)", "a >= 1")),
        c("(var__1 == 1 & var__2 == 1)", "a >= 1")
    )
})

testthat::test_that("Handles NA input", {
    testthat::expect_equal(
        replace_ContainsAll(c(NA, "var.ContainsAll(1, 2)", "a >= 1")),
        c(NA, "(var__1 == 1 & var__2 == 1)", "a >= 1")
    )
})
