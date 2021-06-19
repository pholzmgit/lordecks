#' Generate deck list from shareable deck code
#'
#' Deck lists are stored and shared in an \emph{RFC 4648} base-32 code. This top-level
#' function takes such an code and extracts the cards and their count according to the rules
#' (\href{https://github.com/RiotGames/LoRDeckCodes}{laid out here})
#'
#' @param code character deck code in base 32 (RFC 4648)
#' @param format  output format of deck list, can be either "df" or "simple"
#' @returns dataframe with cardcode, count, faction, set and card number (for format "df")
#' or a character vector with format '\{count\}:\{cardcode\}' (for format "simple")
#' @export
#' @examples
#' get_decklist_from_code(
#' "CEDACAIFDUAQEBAIAECAIBICAECDGNACAMCQIBQDAMCAKDISAIAQCBBWAIAQKEZPAIAQGBIMAMAQKAIZEE"
#' )
#'
#' # returns (format: "df")
#' #    cardcode count faction set card_number
#' # 1   01SI029     3      SI   1         029
#' # 2   02PZ008     3      PZ   2         008
#' # 3   04PZ005     3      PZ   4         005
#' # 4   01PZ051     3      PZ   1         051
#' # 5   01PZ052     3      PZ   1         052
#' # 6   03SI004     3      SI   3         004
#' # 7   03SI006     3      SI   3         006
#' # 8   03PZ005     3      PZ   3         005
#' # 9   03PZ013     3      PZ   3         013
#' # 10  03PZ018     3      PZ   3         018
#' # 11  01PZ054     2      PZ   1         054
#' # 12  01SI019     2      SI   1         019
#' # 13  01SI047     2      SI   1         047
#' # 14  03SI012     1      SI   3         012
#' # 15  01SI001     1      SI   1         001
#' # 16  01SI025     1      SI   1         025
#' # 17  01SI033     1      SI   1         033
#' #
#' # returns(format: simple)
#' # c("3:01SI029", "3:02PZ008", "3:04PZ005", "3:01PZ051", "3:01PZ052",
#' #   "3:03SI004", "3:03SI006", "3:03PZ005", "3:03PZ013", "3:03PZ018",
#' #   "2:01PZ054", "2:01SI019", "2:01SI047", "1:03SI012", "1:01SI001",
#' #   "1:01SI025", "1:01SI033")
#'
#'
get_decklist_from_code <- function(code, format = "df") {
  if(!(format %in% c("df", "simple"))) {
    stop("output format is not available")
  }
  decoded <- base32_decode(code)
  out <- parse_decklist(decoded)

  if(format == "simple") {
    out <- df_to_simple_decklist(out)
  }
  out
}



#' Generate a shareable deck code from a decklist
#'
#' Deck lists are stored and shared in an \emph{RFC 4648} base-32 code. This top-level
#' functions take the cards and their count , and encode them according to the rules
#' (\href{https://github.com/RiotGames/LoRDeckCodes}{laid out here})
#'
#' @param decklist character vector of decklist with format '\{count\}:\{cardcode\}'
#' @returns character deck code in base 32 (RFC 4648)
#' @export
#' @examples
#' #minimalist example (obviously incomplete deck)
#' min_deck <- data.frame(
#'   "cardcode" = c("01PZ008", "01PZ040"),
#'   "count" = c(3, 3)
#' )
#'
#' get_code_from_decklist(c("3:01PZ008", "3:01PZ040"))
#' # returns "CEAQEAIEBAUAAAA"#'
get_code_from_decklist <- function(decklist) {
  decklist_df <- simple_decklist_to_df(decklist)
  get_code_from_decklist_df(decklist_df)
}


#' @rdname get_code_from_decklist
#' @param decklist_df dataframe with character column \emph{cardcode} and numeric column \emph{count}
#' @export
#' @examples
#'
#' get_code_from_decklist_df(min_deck)
#' # returns "CEAQEAIEBAUAAAA"
get_code_from_decklist_df <- function(decklist_df) {
  int_list <- gen_int_decklist(decklist_df)
  base32_encode(int_list)
}


