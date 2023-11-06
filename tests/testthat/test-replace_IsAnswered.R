testthat::test_that("Replaces !IsAnswered before IsAnswered", {
    testthat::expect_equal(
        replace_IsAnswered("!IsAnswered(var1) && IsAnswered(var2)"),
        "mi(var1) && !mi(var2)"
    )
})

testthat::test_that("Replaces more than 1 IsAnswered", {
    testthat::expect_equal(
        replace_IsAnswered("IsAnswered(var1) && IsAnswered(var2)"),
        "!mi(var1) && !mi(var2)"
    )
})

testthat::test_that("Returns unmodified string if no IsAnswered match found", {
  testthat::expect_equal(
    replace_IsAnswered("!M4_Q08.ContainsOnly(10)"),
    "!M4_Q08.ContainsOnly(10)"
  )
})
