#' Translate varint bytes into binary integer
#'
#' The function takes a vector of varint bytes and parses them into an array of
#' integers (in binary format). For varints, the most significant bit indicates
#' whether the next byte belongs to the same integer (1) or if it is the last byte (0).
#'
#' @param bytearray character vector with each entry representing one byte ("01110011")
#' @return If the input is a valid array of varint bytes,
#' the function returns the corresponding integers in binary
#' @examples
#' lordecks:::varint_bytes_to_binary("01111111")
#' # returns "1111111"                        # = 127
#'
#' lordecks:::varint_bytes_to_binary(c("01111111", "10000001", "00000000"))
#' # returns c(1111111, 00000010000000)       # = c(127, 128)
varint_bytes_to_binary <- function(bytearray) {
  # varint:
  # MSB:
  #     1: next byte part of same integer
  #     0: last (or only) byte of current integer
  #    this only has impact if there are > 127 cards per region & set

  #check all bytes are correct length
  if(sum(stringr::str_length(bytearray) == 8) != length(bytearray)) {
    stop("not all elements are bytes of length 8")
  }

  binary_out <- c()
  leading <- ""

  while(length(bytearray) > 0) {
    b <- bytearray[1]
    bytearray <- bytearray[-1]

    MSB <- stringr::str_extract(b, "^[01]{1}")
    bin_number <- stringr::str_extract(b, "[01]{7}$")

    current_bin <- paste0(leading, bin_number)

    if(MSB == "0") {
      binary_out <- c(binary_out, current_bin)
      leading <- ""
    } else {
      leading <- bin_number
    }

  }

  if(leading != "") {
    warning("Probably invalid varints (MSB of last byte not 0).")
  }

  binary_out
}





#' Translate integer vector into varint bytes
#'
#' The function takes a vector of integers and translates them into a varint byte vector.
#' For varints, the most significant bit indicates whether the next byte belongs to the
#' same integer (1) or if it is the last byte (0), so the lenght of the input and output
#' vectors do not match as soon as one of the numbers i larger than 127.
#'
#' @param int_array vector of positive integers
#' @return If all inputs are smaller than 2e9, a character vector containing varint bytes
#' @examples
#' lordecks:::int_to_varint(c(3, 128, 12))
#' # returns c("00000011", "10000001", "00000000", "00001100")
int_to_varint <- function(int_array) {

  if(any(as.numeric(int_array) < 0)) {

    stop("at least one of the integers is negative.")
  }

  if(any(as.numeric(int_array) > 2e9)) {

    stop("at least one of the integers is > 2e9,\n which causes issues with the int to binary conversion.  check ?integer")
  }

  varint_out <- c()

  for(i in int_array) {

    bin_num <- R.utils::intToBin(i)
    n_bits <- stringr::str_length(bin_num)


    #ensure multiples of 7 bits (and add relevant MSB later)
    n_bytes <- ceiling(n_bits / 7)
    pad_lead <- n_bytes * 7 - n_bits
    padding <- ifelse(pad_lead > 0, glue::glue_collapse(rep(0, pad_lead)), "")
    bin_num_padded <- glue::glue("{padding}{bin_num}")

    if(n_bits > 7) {

      non_MSB <- stringr::str_match_all(bin_num_padded, "[01]{7}")[[1]][, 1]
      #add leading 1 to all
      with_MSB <- as.character(glue::glue("1{non_MSB}"))
      #set leading 0 for last element in varint
      with_MSB[length(with_MSB)] <- stringr::str_replace(with_MSB[length(with_MSB)], "^1", "0")

      varint_out <- c(varint_out, with_MSB)

    } else {
      new_byte <- as.character(glue::glue("0{bin_num_padded}"))
      varint_out <- c(varint_out, new_byte)
    }
  }

  varint_out

}


