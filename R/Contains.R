# ==============================================================================
# Yes.Contains / No.Contains
# ==============================================================================

#' Replace SuSo's Yes.Contains and No.Contains
#'
#' @param suso_expr Character. SuSo enablement or validation condition.
#'
#' @return Character. If pattern found, modified expression.
#' Otherwise, unmodified expression.
#'
#' @importFrom stringr str_extract str_replace
#' @importFrom glue glue
#' @importFrom dplyr case_when
#' 
#' @noRd 
replace_one_Contains_yn <- function(
    suso_expr
) {

    contains_expr <- stringr::str_extract(
        string = suso_expr,
        pattern = "[A-Za-z0-9_]+?\\.(Yes|No)\\.Contains\\(.+?\\)"
    )

    # if pattern found, return modified SuSo expression
    if (!is.na(contains_expr)) {

        contains_var <- stringr::str_extract(
            string = contains_expr,
            # variable characters before method's ddot
            pattern = "[A-Za-z0-9_]+?(?=\\.(Yes|No))"
        )

        contains_val <- stringr::str_extract(
            string = contains_expr,
            # everything inside the parentheses, non-greedily
            pattern = "(?<=\\().+?(?=\\))"
        )

        contains_yes_or_no <- stringr::str_extract(
                string = contains_expr,
                pattern = "(Yes|No)(?=\\.Contains)"
            )

        contains_yn_val <- dplyr::case_when(
            contains_yes_or_no == "Yes" ~ 1,
            contains_yes_or_no == "No" ~ 0,
            TRUE ~ NA
            )

        contains_stata <- glue::glue(
            "{contains_var}__{contains_val} == {contains_yn_val}"
        )

        suso_expr_rev <- stringr::str_replace(
            string = suso_expr,
            # IMPORTANT: find/replace without regex
            pattern = stringr::fixed(contains_expr),
            replacement = contains_stata
        )

        return(suso_expr_rev)

    # otherwise, return unmodified SuSo expression
    } else if (is.na(contains_expr)) {

        return(suso_expr)

    }

}

#' Replace all Yes.Contains or No.Contains found in string
#' 
#' @importFrom stringr str_count
#' @importFrom purrr reduce
#' 
#' @noRd 
replace_all_Contains_yn <- function(
    suso_expr
) {

    # count how many InList are in the expression
    n_matches <- stringr::str_count(
        string = suso_expr,
        pattern = "[A-Za-z0-9_]+?\\.(Yes|No)\\.Contains\\(.+?\\)"
    )

    if (is.na(n_matches) | n_matches == 0) {

        return(suso_expr)

    } else if (n_matches >=1) {

        # iteratively replace each InList in the expression
        suso_expr_rev <- purrr::reduce(
            .x = seq(1, n_matches),
            .f = ~ replace_one_Contains_yn(suso_expr = ..1),
            .init = suso_expr
        )

        return(suso_expr_rev)

    }

}

#' Replace all SuSo's Yes.Contains and No.Contains calls with Stata equivalent
#'
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#'
#' @return Character vector. Expression where SuSo content has been replaced with
#' Stata content.
#'
#' @importFrom purrr map_chr
#'
#' @export
replace_Contains_yn <- function(
    suso_expr
) {

    purrr::map_chr(
        .x = suso_expr,
        .f = ~ replace_all_Contains_yn(.x)
    )

}

# ==============================================================================
# Contains
# ==============================================================================

#' Replace SuSo's Contains with Stata equivalent
#'
#' @param suso_expr Character. SuSo enablement or validation condition.
#'
#' @return Character. If pattern found, modified expression.
#' Otherwise, unmodified expression.
#'
#' @importFrom stringr str_extract str_replace fixed
#' @importFrom glue glue
#' 
#' @noRd 
replace_one_Contains <- function(
    suso_expr
) {

    contains_expr <- stringr::str_extract(
        string = suso_expr,
        pattern = "[A-Za-z0-9_]+?\\.Contains\\(.+?\\)"
    )

    # if pattern found, return modified character string
    if (!is.na(contains_expr)) {

        contains_var <- stringr::str_extract(
            string = contains_expr,
            # variable characters before method's ddot
            pattern = "[A-Za-z0-9_]+?(?=\\.)"
        )

        contains_val <- stringr::str_extract(
            string = contains_expr,
            # everything inside the parentheses, non-greedily
            pattern = "(?<=\\().+?(?=\\))"
        )

        contains_stata <- glue::glue("{contains_var}__{contains_val} == 1")

        suso_expr_rev <- stringr::str_replace(
            string = suso_expr,
            # important: find/replace without regex
            pattern = stringr::fixed(contains_expr),
            replacement = contains_stata
        )

        return(suso_expr_rev)

    # otherwise, return unmodified character string
    } else if (is.na(contains_expr)) {

        return(suso_expr)

    }

}

#' Replace all occurrences of Contains in string
#' 
#' @importFrom stringr str_count
#' @importFrom purrr reduce
#' 
#' @noRd 
replace_all_Contains <- function(
    suso_expr
) {

    # count how many InList are in the expression
    n_matches <- stringr::str_count(
        string = suso_expr,
        pattern = "[A-Za-z0-9_]+?\\.Contains\\(.+?\\)"
    )

    if (is.na(n_matches) | n_matches == 0) {

        return(suso_expr)

    } else if (n_matches >=1) {

        # iteratively replace each InList in the expression
        suso_expr_rev <- purrr::reduce(
            .x = seq(1, n_matches),
            .f = ~ replace_one_Contains(suso_expr = ..1),
            .init = suso_expr
        )

        return(suso_expr_rev)

    }

}


#' Replace all of SuSo's Contains calls with Stata equivalent
#'
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#'
#' @return Character vector. Expression where SuSo content has been replaced 
#' with Stata content.
#'
#' @importFrom purrr map_chr
#'
#' @export
replace_Contains <- function(
    suso_expr
) {

    purrr::map_chr(
        .x = suso_expr,
        .f = ~ replace_all_Contains(.x)
    )

}

# ==============================================================================
# ContainsAll
# ==============================================================================

#' Replace SuSo's ContainsAll with Stata equivalent
#'
#' @param suso_expr Character. SuSo enablement or validation condition.
#'
#' @return Character. If pattern found, modified expression.
#' Otherwise, unmodified expression.
#'
#' @importFrom stringr str_extract str_split str_replace fixed
#' @importFrom glue glue
#' @importFrom dplyr `%>%`
#' 
#' @noRd 
replace_one_ContainsAll <- function(
    suso_expr
) {

    containsAll_expr <- stringr::str_extract(
        string = suso_expr,
        pattern = "[A-Za-z0-9_]+?.ContainsAll\\(.+?\\)"
    )

    # if pattern found, return modified SuSo expression
    if (!is.na(containsAll_expr)) {

        containsAll_var <- stringr::str_extract(
            string = containsAll_expr,
            # variable characters before method's ddot
            pattern = "[A-Za-z0-9_]+?(?=\\.)"
        )

        containsAll_vals_txt <- stringr::str_extract(
            string = containsAll_expr,
            # everything inside the parentheses, non-greedily
            pattern = "(?<=\\().+?(?=\\))"
        )

        containsAll_vals <- stringr::str_split(
            string = containsAll_vals_txt,
            pattern = ",[ ]*"
        )

        inputs <- tibble::tibble(
            containsAll_var = containsAll_var,
            containsAll_val = unlist(containsAll_vals)
        )

        containsAll_stata <- inputs %>%
            glue::glue_data("{containsAll_var}__{containsAll_val} == 1") %>%
            glue::glue_collapse(sep = " & ")

        containsAll_stata <- paste0("(", containsAll_stata, ")")

        suso_expr_rev <- stringr::str_replace(
            string = suso_expr,
            # important: find/replace without regex
            pattern = stringr::fixed(containsAll_expr),
            replacement = containsAll_stata
        )

        return(suso_expr_rev)

    } else if (is.na(containsAll_expr)) {

        return(suso_expr)

    }

}

#' Replace all occurences of ContainsAll in string
#' 
#' @importFrom stringr str_count
#' @importFrom purrr reduce
#' 
#' @noRd 
replace_all_ContainsAll <- function(
    suso_expr
) {

    # count how many InList are in the expression
    n_matches <- stringr::str_count(
        string = suso_expr,
        pattern = "[A-Za-z0-9_]+?.ContainsAll\\(.+?\\)"
    )

    if (is.na(n_matches) | n_matches == 0) {

        return(suso_expr)

    } else if (n_matches >=1) {

        # iteratively replace each InList in the expression
        suso_expr_rev <- purrr::reduce(
            .x = seq(1, n_matches),
            .f = ~ replace_one_ContainsAll(suso_expr = ..1),
            .init = suso_expr
        )

        return(suso_expr_rev)

    }

    
}

#' Replace all of SuSo's ContainsAll call with Stata equivalent
#'
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#'
#' @return Character vector. If pattern found, modified expression.
#' Otherwise, unmodified expression.
#'
#' @importFrom purrr map_chr
#'
#' @export
replace_ContainsAll <- function(
    suso_expr
) {

    purrr::map_chr(
        .x = suso_expr,
        .f = ~ replace_all_ContainsAll(.x)
    )

}

# ==============================================================================
# ContainsAny
# ==============================================================================

#' Replace SuSo's Contains with Stata equivalent
#'
#' @param suso_expr Character. SuSo enablement or validation condition.
#'
#' @return Character. If pattern found, modified expression.
#' Otherwise, unmodified expression.
#'
#' @importFrom stringr str_extract str_replace str_split fixed
#' @importFrom tibble tibble
#' @importFrom glue glue_data glue_collapse
#' @importFrom dplyr `%>%`
#' 
#' @noRd 
replace_one_ContainsAny <- function(
    suso_expr
) {

    containsAny_expr <- stringr::str_extract(
        string = suso_expr,
        pattern = "[A-Za-z0-9_]+?.ContainsAny\\(.+?\\)"
    )

    # if pattern found, return modified character string
    if (!is.na(containsAny_expr)) {

        containsAny_var <- stringr::str_extract(
            string = containsAny_expr,
            # variable characters before method's ddot
            pattern = "[A-Za-z0-9_]+?(?=\\.)"
        )

        containsAny_vals_txt <- stringr::str_extract(
            string = containsAny_expr,
            # everything inside the parentheses, non-greedily
            pattern = "(?<=\\().+?(?=\\))"
        )

        containsAny_vals <- stringr::str_split(
            string = containsAny_vals_txt,
            pattern = ",[ ]*"
        )

        inputs <- tibble::tibble(
            containsAny_var = containsAny_var,
            containsAny_val = unlist(containsAny_vals)
        )

        containsAny_stata <- inputs %>%
            glue::glue_data("{containsAny_var}__{containsAny_val} == 1") %>%
            glue::glue_collapse(sep = " | ")

        containsAny_stata <- paste0("(", containsAny_stata, ")")

        suso_expr_rev <- stringr::str_replace(
            string = suso_expr,
            # important: find/replace without regex
            pattern = stringr::fixed(containsAny_expr),
            replacement = containsAny_stata
        )

        return(suso_expr_rev)

    # otherwise, return unmodified character string
    } else if (is.na(containsAny_expr)) {

        return(suso_expr)

    }

}

#' Replace all ContainAny occurrences in single string
#' 
#' Iterate over all matches in string
#' 
#' @param suso_expr Character. SuSo enablement or validation condition.
#'
#' @return Character. If pattern found, modified expression.
#' Otherwise, unmodified expression.
#' 
#' @importFrom stringr str_count
#' @importFrom purrr reduce
#' 
#' @noRd 
replace_all_ContainsAny <- function(
    suso_expr
) {

    # count how many InList are in the expression
    n_matches <- stringr::str_count(
        string = suso_expr,
        pattern = "[A-Za-z0-9_]+?.ContainsAny\\(.+?\\)"
    )

    if (is.na(n_matches) | n_matches == 0) {

        return(suso_expr)

    } else if (n_matches >=1) {

        # iteratively replace each InList in the expression
        suso_expr_rev <- purrr::reduce(
            .x = seq(1, n_matches),
            .f = ~ replace_one_ContainsAny(suso_expr = ..1),
            .init = suso_expr
        )

        return(suso_expr_rev)

    }

}

#' Replace all of SuSo's ContainsAny calls with the Stata equivalent(s)
#'
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#'
#' @return Character vector. Expression where SuSo content has been replaced 
#' with Stata content.
#'
#' @importFrom purrr map_chr
#'
#' @export
replace_ContainsAny <- function(
    suso_expr
) {

    purrr::map_chr(
        .x = suso_expr,
        .f = ~ replace_all_ContainsAny(.x)
    )

}
