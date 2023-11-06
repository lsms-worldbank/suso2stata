testthat::test_that("Replaces `Yes.Contains` with Stata equivalent", {
    testthat::expect_equal(
        replace_Contains_yn("M22A_Q04_A.Yes.Contains(1) && M22A_Q03_A==1"),
        "M22A_Q04_A__1 == 1 && M22A_Q03_A==1"
    )
})

testthat::test_that("Replaces `No.Contains` with Stata equivalent", {
    testthat::expect_equal(
        replace_Contains_yn("M22A_Q11_qty_A >0 && M22A_Q09_A.No.Contains(2)"),
        "M22A_Q11_qty_A >0 && M22A_Q09_A__2 == 0"
    )
})
