testthat::test_that("Replaces `&&`` with `&`", {
    testthat::expect_equal(
        replace_and("M4_Q26==1 && (M4_Q16.InRange(3,96) || M4_Q19.InRange(3,96))"),
        "M4_Q26==1 & (M4_Q16.InRange(3,96) || M4_Q19.InRange(3,96))"
    )
})

testthat::test_that("Returns unmodified string if no `&&` match found", {
  testthat::expect_equal(
    replace_or("!M4_Q08.ContainsOnly(10)"),
    "!M4_Q08.ContainsOnly(10)"
  )
})

