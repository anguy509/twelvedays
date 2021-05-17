context("sing_day")

test_that("sing_day works", {
  # set up for dataframe to use to test
  library(english)
  xmas <- read.csv("https://www.dropbox.com/s/e584pryn8evm1gz/xmas.csv?dl=1")
  xmas$Gift.Item[2:length(xmas$Gift.Item)] <- xmas$Gift.Item[2:length(xmas$Gift.Item)] %>%
    map_chr(pluralize_gift)
  xmas <- xmas %>% mutate(
    Full.Phrase = pmap(xmas, ~make_phrase(..1, ..2, ..3, ..4, ..5, ..6))
  )
  output <- "On the first day of Christmas, my true love sent to me,\na partridge in a pear tree."
  expect_equal(sing_day(xmas, 1, Full.Phrase), output)

  output <- "On the second day of Christmas, my true love sent to me,\ntwo turtle doves \nand a partridge in a pear tree."
  expect_equal(sing_day(xmas, 2, Full.Phrase), output)

  output <- "On the third day of Christmas, my true love sent to me,\nthree french hens\ntwo turtle doves \nand a partridge in a pear tree."
  expect_equal(sing_day(xmas, 3, Full.Phrase), output)

  output <- "On the twelfth day of Christmas, my true love sent to me,\ntwelve drummers drumming\neleven pipers piping\nten lords a-leaping\nnine ladies dancing\neight maids a-milking\nseven swans a-swimming\nsix geese a-laying\nfive golden rings\nfour calling birds\nthree french hens\ntwo turtle doves \nand a partridge in a pear tree."
  expect_equal(sing_day(xmas, 12, Full.Phrase), output)



})
