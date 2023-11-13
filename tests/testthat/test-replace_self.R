testthat::test_that("Replaces `self`", {
    testthat::expect_equal(
        replace_self(
            suso_expr = "self.Contains(1)",
            varname = "var1"
        ),
        "var1.Contains(1)"
    )
})

testthat::test_that("Replaces `self` in df", {
    # create text fixtures
    df_in <- data.frame(
        varname = c("b", "c"),
        condition_expression = c("a > 1", "self == 2")
    )
    df_out <- data.frame(
        varname = c("b", "c"),
        condition_expression = c("a > 1", "c == 2")
    )
    # perform test
    testthat::expect_equal(
        df_rev <- dplyr::mutate(
            .data = df_in,
            condition_expression = replace_self(
                suso_expr = condition_expression,
                varname = varname
            )
        ),
        df_out    
    )
})
