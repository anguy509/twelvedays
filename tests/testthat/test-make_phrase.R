context("make_phrase")

test_that("make_phrase works", {
  output <- "ten lords a-leaping"
  expect_equal(make_phrase(num = 10,
                           num_word = "ten",
                           item = "lords",
                           verb = "a-leaping",
                           adjective = "",
                           location = ""), output)

  output <- "an elephant a-eating peanuts"
  expect_equal(make_phrase(num = 1,
                           num_word = "one",
                           item = "elephant",
                           verb = "a-eating peanuts",
                           adjective = "",
                           location = ""), output)

  output <- "a cute panda climbing bamboo in Japan"
  expect_equal(make_phrase(num = 1,
                           num_word = "one",
                           item = "panda",
                           verb = "climbing bamboo",
                           adjective = "cute",
                           location = "in Japan"), output)

  output <- "thirteen sleepy koalas napping in Australia"
  expect_equal(make_phrase(num = 13,
                           num_word = "thirteen",
                           item = "koalas",
                           verb = "napping",
                           adjective = "sleepy",
                           location = "in Australia"), output)

})

