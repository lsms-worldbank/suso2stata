testthat::test_that("Replaces `null` with `.`", {
    testthat::expect_equal(
        replace_null("auto_totqty_0==false || auto_totqty_0==null"),
        "auto_totqty_0==false || auto_totqty_0==."
    )
})
