#' Replace SuSo IsAnswered with Stata mi
#' 
#' @details First, replace all negated IsAnswered calls 
#' (i.e., `!IsAnswered()`). Then, replace all non-negated calls 
#' (i.e., `IsAnswered()`). In that way, negated calls will be handled properly.
#' 
#' @param suso_expr Character. SuSo enablement or validation condition.
#' 
#' @importFrom stringr str_replace_all
#' 
#' @export 
replace_IsAnswered <- function(
    suso_expr
) {

    # first, replace all NOT IsAnswered, if any
    # that way, properly handle negated before non-negated calls
    suso_expr_rev <- stringr::str_replace_all(
        string = suso_expr,
        pattern = "!IsAnswered",
        replacement = "mi"
    )

    # then, replace all IsAnswered calls, which are free of negation
    suso_expr_rev <- stringr::str_replace_all(
        string = suso_expr_rev,
        pattern = "IsAnswered",
        replacement = "!mi"
    )

    return(suso_expr_rev)

}

#' Replace SuSo `||` with Stata `|`
#' 
#' @param suso_expr Character. SuSo enablement or validation condition.
#' 
#' @importFrom stringr str_replace_all
#' 
#' @export 
replace_or <- function(
    suso_expr
) {

    suso_expr_rev <- stringr::str_replace_all(
        string = suso_expr,
        pattern = "\\|\\|",
        replacement = "|"
    )

    return(suso_expr_rev)

}

#' Replace SuSo `&&` with Stata `&`
#' 
#' @param suso_expr Character. SuSo enablement or validation condition.
#' 
#' @importFrom stringr str_replace_all
#' 
#' @export 
replace_and <- function(
    suso_expr
) {

    suso_expr_rev = stringr::str_replace_all(
        string = suso_expr,
        pattern = "&&",
        replacement = "&"
    )

}

#' Replace SuSo `null` with Stata `.`
#' 
#' @param suso_expr Character. SuSo enablement or validation condition.
#' 
#' @importFrom stringr str_replace_all
#' 
#' @export 
replace_null <- function(
    suso_expr
) {

    suso_expr_rev <- stringr::str_replace_all(
        string = suso_expr,
        pattern = "null",
        replacement = "."
    )

    return(suso_expr_rev)

}
