testthat::test_that("Replaces Boolean values", {
    testthat::expect_equal(
        replace_bool(
            c(
                "v300e==1 && v340a==true", 
                "v300e==1 && v340a==false", 
                "v300e==false && v340a==true"
            )
        ),
        c(
                "v300e==1 && v340a==1", 
                "v300e==1 && v340a==0", 
                "v300e==0 && v340a==1"
        )
    )
})
