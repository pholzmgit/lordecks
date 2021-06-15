get_decklist_from_code <- function(code) {
  decoded <- base32_decode(code)
  parse_decklist(decoded)
}

get_code_from_decklist <- function(decklist) {
  int_list <- gen_int_decklist(decklist)
  base32_encode(int_list)
}
