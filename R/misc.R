#' Replace SuSo IsAnswered with Stata mi
#' 
#' @details First, replace all negated IsAnswered calls 
#' (i.e., `!IsAnswered()`). Then, replace all non-negated calls 
#' (i.e., `IsAnswered()`). In that way, negated calls will be handled properly.
#' 
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#' 
#' @return Character vector. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
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
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#' 
#' @return Character vector. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
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
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#' 
#' @return Character vector. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
#' 
#' @importFrom stringr str_replace_all
#' 
#' @export 
replace_and <- function(
    suso_expr
) {

    suso_expr_rev <- stringr::str_replace_all(
        string = suso_expr,
        pattern = "&&",
        replacement = "&"
    )

    return(suso_expr_rev)

}

#' Replace SuSo `null` with Stata `.`
#' 
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#' 
#' @return Character vector. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
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

#' Replace `true` and `false` values with `1` and `0`, respectively
#' 
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#' 
#' @return Character vector. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
#' 
#' @importFrom stringr str_replace_all
#' 
#' @export 
replace_bool <- function(
    suso_expr
) {

    # replace `true` with `1`
    suso_expr_rev <- stringr::str_replace_all(
        string = suso_expr,
        pattern = "true",
        replacement = "1"
    )


    # replace `false` with `0`
    suso_expr_rev <- stringr::str_replace_all(
        string = suso_expr_rev,
        pattern = "false",
        replacement = "0"
    )

    return(suso_expr_rev)

}

#' Replace rowcode or roster variable references with row ID variable
#' 
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#' @param rowcode Character. `rowcode` mention like `@rowcode` or '
#' `@roster_var_name`
#' @param replacement Character. Row ID variable replacement
#'  (e.g., `members__id`).
#' 
#' @return Character vector. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
#' 
#' @importFrom stringr str_replace_all fixed
#' 
#' @export 
replace_rowcode_with <- function(
    suso_expr,
    rowcode = "@rowcode",
    replacement
) {

    # check that rowcode contains `@`
    contains_at <- grepl(x = rowcode, pattern = "@", fixed = TRUE)
    if (contains_at == FALSE) {
        stop("`rowcode` should contain a character string with `@`, like `@rowcode`")
    }

    suso_expr_rev <- stringr::str_replace_all(
        string = suso_expr,
        pattern = stringr::fixed(rowcode),
        replacement = replacement
    )

    return(suso_expr_rev)

}

#' Replace `self` keyword with variable name
#' 
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#' @param varname Character vector. Variable name to replace `self`
#' 
#' @return Character vector. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
#' 
#' @importFrom stringr str_replace fixed
#' 
#' @export 
replace_self <- function(
    suso_expr,
    varname
) {

    suso_expr_rev <- stringr::str_replace_all(
        string = suso_expr,
        pattern = stringr::fixed("self"),
        replacement = varname
    )

    return(suso_expr_rev)

}
