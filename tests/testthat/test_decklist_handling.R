library(lordecks)

test_that("conversion between deck formats",{
  df_deck <- data.frame(
    "count" = c(3, 3),
     "cardcode" = c("01PZ008", "01PZ040")
  )
  simple_deck <- c("3:01PZ008", "3:01PZ040")
  expect_equal(lordecks:::df_to_simple_decklist(df_deck), simple_deck)
  expect_equal(lordecks:::simple_decklist_to_df(simple_deck), df_deck)

})


test_that("decklist parsing", {

  int_list <- c(1, 2, 1, 4, 8, 40, 0, 0)

  parsed_df <- data.frame(
    "cardcode" = c("01PZ008", "01PZ040"),
    "count" = c(3, 3),
    "faction" = c("PZ", "PZ"),
    "set" = c(1, 1),
    "card_number" = c("008", "040")
  )
  #includes version info
  int_list_out <- c(17, 1, 2, 1, 4, 8, 40, 0, 0)

  expect_equal(lordecks:::parse_decklist(int_list), parsed_df)
  expect_equal(lordecks:::gen_int_decklist(parsed_df), int_list_out)

})
