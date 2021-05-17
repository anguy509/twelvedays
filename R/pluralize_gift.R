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
#' @import english
#'
#' @export
pluralize_gift <- function(gift){
  if(str_detect(gift, "oo"))
  {
    gift <- str_replace(gift, "oo", "ee")
    return(gift)
  }else{
    gift <- gift %>%
      str_replace("y$", "ies") %>%
      str_replace("[a-z&&[^s]]$", paste(str_sub(gift, nchar(gift)), "s", sep=""))
    return(gift)
  }
}

#xmas <- xmas %>% mutate(Full.Phrase = pmap(xmas, ~make_phrase(..1, ..2, ..3, ..4, ..5, ..6)))
