#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#' @import english
#
#'
#' @export
sing_day <- function(dataset, line, phrase_col){
  phrases <- dataset %>% pull({{phrase_col}})
  phrase <- glue("On the ", {dataset$Day.in.Words[line]}, " day of Christmas, my true love sent to me,", "\n")
  result <- ""

  if(line >= 2)
  {
    result <- map_chr(phrases[line:2], ~paste(result, .x, sep = "", collapse = "\n"))
    result <- paste(result, collapse = '\n', sep = "")
    result <- paste(phrase, '\n', result, ' \nand ', phrases[1], '.', sep = "")
  }
  else
  {
    result<- paste(phrase, '\n', phrases[line], '.', sep = "")
  }

  return(result)
}
