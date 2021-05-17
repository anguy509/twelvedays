#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import english
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export



make_phrase <- function(num, num_word, item, verb, adjective, location){
  an <- F
  if(str_detect(item, "^[aeiou]"))
  {
    an <- T
  }
  phrase <- glue(as.character(english(num)), " {adjective} {item} {verb} {location}", .na = "")
  if(an)
  {
    phrase <- phrase %>%
      str_replace("one", "an")
  }else{
    phrase <- phrase %>%
      str_replace("one", "a")
  }

  phrase <- phrase %>%
    str_squish()
  return(phrase)
}

#xmas <- xmas %>% mutate(Full.Phrase = pmap(xmas, ~make_phrase(..1, ..2, ..3, ..4, ..5, ..6)))
