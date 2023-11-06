# ==============================================================================
# InList
# ==============================================================================

#' Replace SuSo's InList with equivalent Stata inlist
#' 
#' @param suso_expr Character. SuSo enablement or validation condition.
#' 
#' @return Character. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
#' 
#' @importFrom stringr str_extract str_replace
#' @importFrom glue glue
replace_one_InList <- function(
    suso_expr
) {

    # --------------------------------------------------------------------------
    # Extract InList expression
    # --------------------------------------------------------------------------

    # extract first InList from larger expression
    inlist_expr <- stringr::str_extract(
        string = suso_expr,
        # expression with these components
        # - variable characters
        # - method dot
        # - InList() method
        # - parameters inside method
        pattern = "[A-Za-z0-9_]+?\\.InList\\(.+?\\)"
    )

    # if pattern found, return modified character string
    if (!is.na(inlist_expr)) {

        # ----------------------------------------------------------------------
        # Extract components of InList call
        # ----------------------------------------------------------------------

        # extract variable from call 
        inlist_var <- stringr::str_extract(
            string = inlist_expr,
            pattern = "[A-Za-z0-9_]+?(?=\\.)"
        )

        # extract parameters from InList() method
        inlist_params <- stringr::str_extract(
            string = inlist_expr,
            # everything inside the parentheses, non-greedily
            pattern = "(?<=\\().+?(?=\\))"
        )

        # ----------------------------------------------------------------------
        # Construct equivalent Stata call
        # ----------------------------------------------------------------------

        inlist_stata <- glue::glue("inlist({inlist_var}, {inlist_params})")

        # ----------------------------------------------------------------------
        # Replace SuSo InList call with Stata inlist
        # ----------------------------------------------------------------------

        suso_expr_rev <- stringr::str_replace(
            string = suso_expr,
            # IMPORTANT: find/replace without regex
            pattern = stringr::fixed(inlist_expr),
            replacement = inlist_stata
        )

        return(suso_expr_rev)

    # otherwise, return unmodified character string
    } else if (is.na(inlist_expr)) {

        return(suso_expr)

    }

}

#' Replace all of SuSo's InList with equivalent Stata inlist
#' 
#' @param suso_expr Character. SuSo enablement or validation condition.
#' 
#' @return Character. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
#' 
#' @importFrom stringr str_count
#' @importFrom purrr reduce
#' 
#' @export 
replace_InList <- function(
    suso_expr
) {

    # count how many InList are in the expression
    n_matches <- stringr::str_count(
        string = suso_expr, 
        pattern = "[A-Za-z0-9_]+?\\.InList\\(.+?\\)"
    )

    if (n_matches == 0) {

        return(suso_expr)

    } else if (n_matches >= 1) {

        # iteratively replace each InList in the expression
        suso_expr_rev <- purrr::reduce(
            .x = seq(1, n_matches),
            .f = ~ replace_one_InList(suso_expr = ..1),
            .init = suso_expr
        )

        return(suso_expr_rev)

    }


}

# ==============================================================================
# InRange
# ==============================================================================

#' Replace SuSo's InRange with equivalent Stata inrange
#' 
#' @param suso_expr Character. SuSo enablement or validation condition.
#' 
#' @return Character. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
#' 
#' @importFrom stringr str_extract str_replace
#' @importFrom glue glue
replace_one_InRange <- function(
    suso_expr
) {

    # --------------------------------------------------------------------------
    # Extract InRange expression
    # --------------------------------------------------------------------------

    # extract first InRange from larger expression
    inrange_expr <- stringr::str_extract(
        string = suso_expr,
        # expression with these components
        # - variable characters
        # - method dot
        # - InRange() method
        # - parameters inside method
        pattern = "[A-Za-z0-9_]+?\\.InRange\\(.+?\\)"
    )

    # if pattern found, return modified character string
    if (!is.na(inrange_expr)) {

        # ----------------------------------------------------------------------
        # Extract components of InRange call
        # ----------------------------------------------------------------------

        # extract variable from call 
        inrange_var <- stringr::str_extract(
            string = inrange_expr,
            pattern = "[A-Za-z0-9_]+?(?=\\.)"
        )

        # extract parameters from InRange() method
        inrange_params <- stringr::str_extract(
            string = inrange_expr,
            # everything inside the parentheses, non-greedily
            pattern = "(?<=\\().+?(?=\\))"
        )

        # ----------------------------------------------------------------------
        # Construct equivalent Stata call
        # ----------------------------------------------------------------------

        inrange_stata <- glue::glue("inrange({inrange_var}, {inrange_params})")

        # ----------------------------------------------------------------------
        # Replace SuSo InRange call with Stata inrange
        # ----------------------------------------------------------------------

        suso_expr_rev <- stringr::str_replace(
            string = suso_expr,
            # IMPORTANT: find/replace without regex
            pattern = stringr::fixed(inrange_expr),
            replacement = inrange_stata
        )

        return(suso_expr_rev)

    # otherwise, return unmodified character string
    } else if (is.na(inrange_expr)) {

        return(suso_expr)

    }

}

#' Replace all of SuSo's InRange with equivalent Stata inrange
#' 
#' @param suso_expr Character. SuSo enablement or validation condition.
#' 
#' @return Character. If pattern found, modified expression. 
#' Otherwise, unmodified expression.
#' 
#' @importFrom stringr str_count
#' @importFrom purrr reduce
#' 
#' @export 
replace_InRange <- function(
    suso_expr
) {

    # count how many InRange are in the expression
    n_matches <- stringr::str_count(
        string = suso_expr, 
        pattern = "[A-Za-z0-9_]+?\\.InRange\\(.+?\\)"
    )

    if (n_matches == 0) {

        return(suso_expr)

    } else if (n_matches >= 1) {

        # iteratively replace each InRange in the expression
        suso_expr_rev <- purrr::reduce(
            .x = seq(1, n_matches),
            .f = ~ replace_one_InRange(suso_expr = ..1),
            .init = suso_expr
        )

        return(suso_expr_rev)

    }


}

# Consider making function above slighty more abstract
# choose whether look for InList or InRange, and replace with inlist or inrange, 
# respectively
