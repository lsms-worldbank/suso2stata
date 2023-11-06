testthat::test_that("Replaces single InList", {
  testthat::expect_equal(
    replace_InList("M4_Q01c_Amt != 0 && M4_Q01.InList(1,4,6,7,96)"),
    "M4_Q01c_Amt != 0 && inlist(M4_Q01, 1,4,6,7,96)"
  )
})


testthat::test_that("Replaces multiple inlist", {
  testthat::expect_equal(
    replace_InList(
      "M10_Q01 !=1 && !M10_Q06.InList(1,2) && M10_Q08 !=1 && M10_Q10.InList(1, 2) && M10_Q11 !=1"
    ),
    "M10_Q01 !=1 && !inlist(M10_Q06, 1,2) && M10_Q08 !=1 && inlist(M10_Q10, 1, 2) && M10_Q11 !=1"
  )
})

testthat::test_that("Returns unmodified string if no InList match found", {
  testthat::expect_equal(
    replace_InList("!M4_Q08.ContainsOnly(10)"),
    "!M4_Q08.ContainsOnly(10)"
  )
})
