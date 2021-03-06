#' Encode integer vector into base-32deck code
#'
#' Deck lists are stored and shared in an \emph{RFC 4648} base-32 code. This function
#' takes an integer representation of the deck list (according to \href{https://github.com/RiotGames/LoRDeckCodes}{these rules})
#' and encodes them via varint bytes into base-32
#'
#' @param int_decklist integer vector describing the deck list
#' @return character deck code in base 32 (RFC 4648)
#' @examples
#' lordecks:::base32_encode(c("17", "6", "1", "1", "5", "29", "1", "2", "4", "8", "1", "4",
#'  "4", "5", "2", "1", "4", "51", "52", "2", "3", "5", "4", "6", "3", "3", "4", "5",
#'  "13", "18", "2", "1", "1", "4", "54", "2", "1", "5", "19", "47", "2", "1", "3",
#'  "5", "12", "3", "1", "5", "1", "25", "33"))
#' #"CEDACAIFDUAQEBAIAECAIBICAECDGNACAMCQIBQDAMCAKDISAIAQCBBWAIAQKEZPAIAQGBIMAMAQKAIZEE"
base32_encode <- function(int_decklist) {

  #example in: "17" "6" "1" "1" "5" "29" "1" "2" "4" "8" "1" "4" "4" "5" "2" "1" "4" "51" "52" "2" "3" "5" "4" "6" "3" "3" "4" "5" "13" "18" "2" "1" "1" "4" "54" "2" "1" "5" "19" "47" "2" "1" "3" "5" "12" "3" "1" "5" "1" "25" "33"

  varint_bytes <- int_to_varint(int_decklist)
  varint_bin <- glue::glue_collapse(varint_bytes, sep = "")

  #ensure multiples of 5 bits by padding !trailing! zeros
  n_bits <- stringr::str_length(varint_bin)
  n_quintets <- ceiling(n_bits/5)
  pad_trail <- n_quintets * 5 - n_bits
  padding <- ifelse(pad_trail > 0, glue::glue_collapse(rep(0, pad_trail)), "")
  bin_varint_padded <- glue::glue("{varint_bin}{padding}")

  #
  b32_binary <- stringr::str_match_all(bin_varint_padded, "[01]{5}")[[1]][, 1]

  b32_nums <- strtoi(b32_binary, base = 2)

  # translate 0:31 to RFC 4648
  #RFC 4648
  symbol_vec <- c(LETTERS, "2", "3", "4", "5", "6", "7")
  value_vec <- 0:31

  encoded_single <- purrr::map_chr(b32_nums, ~symbol_vec[value_vec == .x])

  as.character(glue::glue_collapse(encoded_single, sep = ""))

  #example out: "CEDACAIFDUAQEBAIAECAIBICAECDGNACAMCQIBQDAMCAKDISAIAQCBBWAIAQKEZPAIAQGBIMAMAQKAIZEE"

}

#' Decode base-32 representation of deck code
#'
#' Deck lists are stored and shared in an \emph{RFC 4648} base-32 code. This function
#' decodes this first into varint bytes to get to the integer representation of the deck list.
#' The output can be further parsed according to \href{https://github.com/RiotGames/LoRDeckCodes}{these rules}.
#'
#' @param code character deck code in base 32 (RFC 4648)
#' @param max_known_version integer of version of encoding. Only effect is which regions are available due to
#' expansions.
#' @return integer vector describing the deck list (without the version info in the first byte, which is only
#' used for a validity check)
#' @examples
#' lordecks:::base32_decode(
#'  "CEDACAIFDUAQEBAIAECAIBICAECDGNACAMCQIBQDAMCAKDISAIAQCBBWAIAQKEZPAIAQGBIMAMAQKAIZEE"
#'  )
#' # returns
#' # c( 6, 1, 1, 5, 29, 1, 2, 4, 8, 1, 4,
#' #    4, 5, 2, 1, 4, 51, 52, 2, 3, 5, 4, 6, 3, 3, 4, 5,
#' #    13, 18, 2, 1, 1, 4, 54, 2, 1, 5, 19, 47, 2, 1, 3,
#' #    5, 12, 3, 1, 5, 1, 25, 33)
base32_decode <- function(code, max_known_version = 5) {

  #example in: "CEDACAIFDUAQEBAIAECAIBICAECDGNACAMCQIBQDAMCAKDISAIAQCBBWAIAQKEZPAIAQGBIMAMAQKAIZEE"

  # remove trailing whitespaces and padding
  code <- stringr::str_remove_all(code, "[ =]*$")

  if(code == "" | is.null(code) | is.na(code)) {
    stop("missing or empty code")
  }


  # translate RFC 4648 to 0:31

  #RFC 4648
  symbol_vec <- c(LETTERS, "2", "3", "4", "5", "6", "7")
  value_vec <- 0:31

  single_characters <- strsplit(code, NULL)[[1]]
  translated_code <- purrr::map_chr(single_characters, ~value_vec[symbol_vec == .x])

  #convert b32 to binary, repack into bytes
  code_binary <- sprintf("%05d", as.integer(R.utils::intToBin(translated_code))) #ensure output width is 5 bit

  varint_bytes <- (glue::glue_collapse(code_binary, sep = "") %>%
                     stringr::str_match_all("[01]{8}"))[[1]][, 1]

  #check version
  version_byte <- varint_bytes[1]
  # version/format 17 ("00010001"), last 4 bits are version (1)
  version <- strtoi(stringr::str_extract(version_byte, "[01]{4}$"), base = 2)
  if(version > max_known_version) {
    warning(glue::glue("The provided code requires a higher version ({version}) of this library (some regions may be missing); please update."))
  }


  varint_bytes <- varint_bytes[-1]
  # varint bytes to simple binary
  bin_values <- varint_bytes_to_binary(varint_bytes)

  # convert bytes back to integers

  strtoi(bin_values, base = 2)

  #example out: "6" "1" "1" "5" "29" "1" "2" "4" "8" "1" "4" "4" "5" "2" "1" "4" "51" "52" "2" "3" "5" "4" "6" "3" "3" "4" "5" "13" "18" "2" "1" "1" "4" "54" "2" "1" "5" "19" "47" "2" "1" "3" "5" "12" "3" "1" "5" "1" "25" "33"
}
