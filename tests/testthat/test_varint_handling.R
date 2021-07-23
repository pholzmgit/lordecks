library(lordecks)

test_that("varints to binary", {
  expect_equal(lordecks:::varint_bytes_to_binary(c("01111111", "10000000", "00000001", "11011001", "00000001")),
               c("1111111", "00000010000000", "00000011011001"))

})

test_that("int to varints", {
  expect_equal(lordecks:::int_to_varint(c(3, 128, 12, 217)),
               c("00000011", "10000000", "00000001", "00001100", "11011001", "00000001"))

})
