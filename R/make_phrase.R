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
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#' @import english
#'
#' @export



make_phrase <- function(num, num_word, item, verb, adjective, location){
  phrase <- glue(as.character(english(num)), " {adjective} {item} {verb} {location}", .na = "")
  phrase <- phrase %>%
    str_replace("one", "a") %>%
    str_squish()
  return(phrase)
}

#xmas <- xmas %>% mutate(Full.Phrase = pmap(xmas, ~make_phrase(..1, ..2, ..3, ..4, ..5, ..6)))
