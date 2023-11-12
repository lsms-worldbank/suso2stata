testthat::test_that("Replaces single InRange", {
  testthat::expect_equal(
    replace_InRange("M4_Q19.InRange(3,96)"),
    "inrange(M4_Q19, 3,96)"
  )
})

testthat::test_that("Replaces multiple inlist", {
  testthat::expect_equal(
    replace_InRange(
      "M4_Q26==1 && (M4_Q16.InRange(3,96) || M4_Q19.InRange(3,96))"
    ),
    "M4_Q26==1 && (inrange(M4_Q16, 3,96) || inrange(M4_Q19, 3,96))"
  )
})

testthat::test_that("Returns unmodified string if no match found", {
  testthat::expect_equal(
    replace_InRange("!M4_Q08.ContainsOnly(10)"),
    "!M4_Q08.ContainsOnly(10)"
  )
})

testthat::test_that("Handle vector of length > 1", {
  testthat::expect_equal(
      replace_InRange(
        c("M4_Q26==1", "M4_Q16.InRange(3,96)", "M4_Q19.InRange(3,96)")
      ),
      c("M4_Q26==1", "inrange(M4_Q16, 3,96)", "inrange(M4_Q19, 3,96)")
  )
})

testthat::test_that("Handle NA input", {
  testthat::expect_equal(
      replace_InRange(
        c(NA, "M4_Q26==1", "M4_Q16.InRange(3,96)", "M4_Q19.InRange(3,96)")
      ),
      c(NA, "M4_Q26==1", "inrange(M4_Q16, 3,96)", "inrange(M4_Q19, 3,96)")
  )
})
