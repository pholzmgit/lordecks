library(lordecks)

test_that("varints to binary", {
  expect_equal(lordecks:::varint_bytes_to_binary(c("01111111", "10000001", "00000000")),
               c("1111111", "00000010000000"))

})

test_that("int to varints", {
  expect_equal(lordecks:::int_to_varint(c(3, 128, 12)),
               c("00000011", "10000001", "00000000", "00001100"))

})
