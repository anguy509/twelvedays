#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
pluralize_gift <- function(gifts){


  gifts <- gifts %>%
    str_replace("y$", "ies") %>%
    str_replace("[a-z&&[^s]]$", paste(str_sub(gifts, nchar(gifts)), "s", sep="")) %>%
    str_replace("gooses", "geese")


  return(gifts)

}
