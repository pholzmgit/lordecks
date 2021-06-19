library(lordecks)
library(readr)
library(stringr)

#get hardcoded examples taken from https://github.com/RiotGames/LoRDeckCodes/tree/master/LoRDeckCodes_Tests

tst <- readr::read_lines("./DeckCodesTestData.txt",skip_empty_rows = TRUE)
code_positions <- which(stringr::str_length(tst) > 9)

examples <- list()

for(i in 1:length(code_positions)) {
  examples[[i]] <- list("code" = tst[code_positions[i]],
                        "simple_format" = tst[(code_positions[i] + 1):
                                                ifelse(i == length(code_positions),
                                                      length(tst),
                                                      code_positions[i+1] - 1)]
                        )
}

test_that("correct parsing of example deck codes", {
  # this produces warnings due to the version info in the testdata
  # but all decks ARE actually correctly parsed.
  for(i in 1:length(examples)) {
    expect_equal(sort(get_decklist_from_code(examples[[i]]$code, format = "simple")),
                 sort(examples[[i]]$simple_format))
  }
})


test_that("encoding and decoding decklists", {
  # check that the decklists remain unchanged after
  # encoding and then decoding again
  for(i in 1:length(examples)) {
    expect_equal(sort(get_decklist_from_code(
                        get_code_from_decklist(examples[[i]]$simple_format),
                        format = "simple")),
                 sort(examples[[i]]$simple_format))
  }
})

test_that("extreme deck lists", {
  small_deck <- c("1:01DE002")
  large_deck <- c("3:01DE002", "3:01DE003", "3:01DE004", "3:01DE005",
                  "3:01DE006", "3:01DE007", "3:01DE008", "3:01DE009",
                  "3:01DE010", "3:01DE011", "3:01DE012", "3:01DE013",
                  "3:01DE014", "3:01DE015", "3:01DE016", "3:01DE017",
                  "3:01DE018", "3:01DE019", "3:01DE020", "3:01DE021")
  highcount_small <- c("4:01DE002")
  highcount_large <- c("3:01DE002", "3:01DE003", "3:01DE004", "3:01DE005",
                        "4:01DE006", "5:01DE007", "6:01DE008", "7:01DE009",
                        "8:01DE010", "9:01DE011", "3:01DE012", "3:01DE013",
                        "3:01DE014", "3:01DE015", "3:01DE016", "3:01DE017",
                        "3:01DE018", "3:01DE019", "3:01DE020", "3:01DE021")
  single40 <- c("40:01DE002")

  worstcase_length <- c("4:01DE002", "4:01DE003", "4:01DE004", "4:01DE005",
                        "4:01DE006", "5:01DE007", "6:01DE008", "7:01DE009",
                        "8:01DE010", "9:01DE011", "4:01DE012", "4:01DE013",
                        "4:01DE014", "4:01DE015", "4:01DE016", "4:01DE017",
                        "4:01DE018", "4:01DE019", "4:01DE020", "4:01DE021")

  expect_equal(sort(get_decklist_from_code(
                      get_code_from_decklist(small_deck),
                      format = "simple")),
               sort(small_deck))
  expect_equal(sort(get_decklist_from_code(
                      get_code_from_decklist(large_deck),
                      format = "simple")),
               sort(large_deck))

  expect_equal(sort(get_decklist_from_code(
                      get_code_from_decklist(highcount_large),
                      format = "simple")),
               sort(highcount_large))

  expect_equal(sort(get_decklist_from_code(
                      get_code_from_decklist(highcount_small),
                    format = "simple")),
               sort(highcount_small))

  expect_equal(sort(get_decklist_from_code(
                      get_code_from_decklist(single40),
                      format = "simple")),
                sort(single40))

  expect_equal(sort(get_decklist_from_code(
                      get_code_from_decklist(worstcase_length),
                      format = "simple")),
              sort(worstcase_length))
})


test_that("order should not matter", {
 deck1 <-  c("1:01DE002", "2:01DE003", "3:02DE003")
 deck2 <-  c("2:01DE003", "3:02DE003", "1:01DE002")
 # count > 4
 deck3 <-  c("4:01DE002", "2:01DE003", "3:02DE003")
 deck4 <-  c("2:01DE003", "3:02DE003", "4:01DE002")
 # multiple count > 4
 deck5 <-  c("4:01DE002", "2:01DE003", "3:02DE003", "5:01DE004")
 deck6 <-  c("5:01DE004", "2:01DE003", "3:02DE003", "4:01DE002")

 expect_equal(get_code_from_decklist(deck1), get_code_from_decklist(deck2))
 expect_equal(get_code_from_decklist(deck3), get_code_from_decklist(deck4))
 expect_equal(get_code_from_decklist(deck5), get_code_from_decklist(deck6))

})


