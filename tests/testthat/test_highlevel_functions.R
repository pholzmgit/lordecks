library(lordecks)

code <- "CEDACAIFDUAQEBAIAECAIBICAECDGNACAMCQIBQDAMCAKDISAIAQCBBWAIAQKEZPAIAQGBIMAMAQKAIZEE"
simple_format <- c("3:01SI029", "3:02PZ008", "3:04PZ005", "3:01PZ051", "3:01PZ052",
                   "3:03SI004", "3:03SI006", "3:03PZ005", "3:03PZ013", "3:03PZ018",
                   "2:01PZ054", "2:01SI019", "2:01SI047", "1:03SI012", "1:01SI001",
                   "1:01SI025", "1:01SI033")
df_format <- data.frame(
  "cardcode" = c('01SI029', '02PZ008', '04PZ005', '01PZ051', '01PZ052', '03SI004',
                 '03SI006', '03PZ005', '03PZ013', '03PZ018', '01PZ054', '01SI019',
                 '01SI047', '03SI012', '01SI001', '01SI025', '01SI033'),
  "count" = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1),
  "faction" = c('SI', 'PZ', 'PZ', 'PZ', 'PZ', 'SI', 'SI', 'PZ', 'PZ', 'PZ', 'PZ', 'SI', 'SI', 'SI', 'SI', 'SI', 'SI'),
  "set" = c(1, 2, 4, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 3, 1, 1, 1),
  "card_number" = c('029', '008', '005', '051', '052', '004', '006', '005', '013', '018', '054', '019', '047', '012', '001', '025', '033')
)

test_that("codes to decklist", {

  expect_equal(get_decklist_from_code(code, format = "simple"), simple_format)
  expect_equal(get_decklist_from_code(code, format = "df"), df_format)

})

test_that("decklist to codes", {

  expect_equal(get_code_from_decklist(simple_format), code)
  expect_equal(get_code_from_decklist_df(df_format), code)

})
