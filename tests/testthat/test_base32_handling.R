library(lordecks)

test_that("encode to base 32", {
  ints <- c("17", "6", "1", "1", "5", "29", "1", "2", "4", "8", "1", "4",
            "4", "5", "2", "1", "4", "51", "52", "2", "3", "5", "4", "6", "3", "3", "4", "5",
            "13", "18", "2", "1", "1", "4", "54", "2", "1", "5", "19", "47", "2", "1", "3",
            "5", "12", "3", "1", "5", "1", "25", "33")
  code <- "CEDACAIFDUAQEBAIAECAIBICAECDGNACAMCQIBQDAMCAKDISAIAQCBBWAIAQKEZPAIAQGBIMAMAQKAIZEE"
  expect_equal(lordecks:::base32_encode(ints), code)
  expect_equal(lordecks:::base32_encode(as.numeric(ints)), code)

})


test_that("decode from base 32", {
  #note: first entry ("17") encodes format and is not returned, output only contains deck
  ints <- c(#17,
    6, 1, 1, 5, 29, 1, 2, 4, 8, 1, 4,
    4, 5, 2, 1, 4, 51, 52, 2, 3, 5, 4, 6, 3, 3, 4, 5,
    13, 18, 2, 1, 1, 4, 54, 2, 1, 5, 19, 47, 2, 1, 3,
    5, 12, 3, 1, 5, 1, 25, 33)
  code <- "CEDACAIFDUAQEBAIAECAIBICAECDGNACAMCQIBQDAMCAKDISAIAQCBBWAIAQKEZPAIAQGBIMAMAQKAIZEE"
  expect_equal(lordecks:::base32_decode(code), ints)

})
