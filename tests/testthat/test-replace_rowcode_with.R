testthat::test_that("Errors if rowcode doesn't include `@`", {
    testthat::expect_error(
        replace_rowcode_with(
            suso_expr = "@rowcode.InList(101,102)",
            rowcode = "rowcode",
            replacement = "food__id"
        )
    )
})

testthat::test_that("Replaces @rowcode, implicitly specified", {
    testthat::expect_equal(
        replace_rowcode_with(
            suso_expr = "@rowcode.InList(101,102)",
            replacement = "food__id"
        ),
        "food__id.InList(101,102)"
    )
})

testthat::test_that("Replaces properly in pipeline", {
    testthat::expect_equal(
        "@rowcode.InList(101,102)" |>
            replace_rowcode_with(replacement = "food__id") |>
            replace_InList()
        ,
        "inlist(food__id, 101,102)"
    )
})
