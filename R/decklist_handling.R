#' parse simple decklist format into dataframe
#'
#' Intended for compatibility of the original decklist format and the more R native use
#' of dataframes
#'
#' @param decklist character vector of decklist with format '\{count\}:\{cardcode\}'
#' @returns dataframe with character column \emph{cardcode} and numeric column \emph{count}
#' @examples
#' lordecks:::simple_decklist_to_df(c("3:01PZ008", "3:01PZ040"))
#' # returns  :
#' #   count cardcode
#' # 1     3  01PZ008
#' # 2     3  01PZ040
simple_decklist_to_df <- function(decklist) {
  has_colon <- stringr::str_detect(decklist, ":")
  if(sum(has_colon) < length(decklist)) {
    stop(glue::glue("Ill-formated entries: {glue::glue_collapse(decklist[!has_colon], sep = ', ')}"))
  }
  purrr::map_dfr(decklist, function(x) {
    count_card <- stringr::str_split(x, ":")[[1]]
    data.frame("count" = as.numeric(count_card[1]),
               "cardcode" = as.character(count_card[2]))
  })
}

#' parse dataframe decklist into simple format
#'
#' Intended for compatibility of the original decklist format and the more R native use
#' of dataframes
#'
#' @param decklist_df dataframe with character column \emph{cardcode} and numeric column \emph{count}
#' @returns character vector of decklist with format '\{count\}:\{cardcode\}'
df_to_simple_decklist <- function(decklist_df) {
  as.character(glue::glue("{decklist_df$count}:{decklist_df$cardcode}"))
}




#' translate decklist of card ID and count into LoR custom encoding
#'
#' LoR uses a custom encoding scheme (\href{https://github.com/RiotGames/LoRDeckCodes}{laid out here})
#' to store information about the cards (and their count) in any given deck as an
#' integer vector. This function takes care of this.
#'
#' @param decklist dataframe with character column \emph{cardcode} adn numeric column \emph{count}
#' @returns integer representation of deck code
#' @importFrom rlang .data
#' @examples
#' #minimalist example
#' min_deck <- data.frame(
#'   "cardcode" = c("01PZ008", "01PZ040"),
#'   "count" = c(3, 3)
#' )
#'
#' lordecks:::gen_int_decklist(min_deck)
#'
#' # returns  : "17" "1" "2" "1" "4" "8" "40" "0" "0"
#' # version/format 17 ("00010001"), last 4 bits are version (=1)
#' # 1 group of 3 copies
#' #    2 cards for combination of
#' #  1 set 1   and
#' #  4 faction 4 (PZ)
#' #      8   card code 01PZ008
#' #     40   card code 01PZ040
#' # 0 group of 2 copies
#' # 0 group if 1 copy
gen_int_decklist <- function(decklist) {

  faction_lut <- get_faction_lut()

  if(!is.data.frame(decklist) |
     !("cardcode" %in% colnames(decklist)) |
     !("count" %in% colnames(decklist))) {
    stop("decklist needs be dataframe containing columns `cardcode`and `count`")
  }

  if(sum(stringr::str_length(decklist$cardcode) == 7) != length(decklist$cardcode)) {
    stop("invalid cardcode(s): string length")
  }

  if(any(decklist$count < 1)) {
    stop("invalid card count: no listed cards may occur less than once")
  }

  #ensure all count levels are present
  if(sum(decklist$count == 1) == 0) {
    decklist <- decklist %>% dplyr::bind_rows(
      data.frame(count = 1, cardcode = "")
    )
  }
  if(sum(decklist$count == 2) == 0) {
    decklist <- decklist %>% dplyr::bind_rows(
      data.frame(count = 2, cardcode = "")
    )
  }
  if(sum(decklist$count == 3) == 0) {
    decklist <- decklist %>% dplyr::bind_rows(
      data.frame(count = 3, cardcode = "")
    )
  }


  decklist <- decklist %>%
    dplyr::mutate(
      faction = stringr::str_match(.data$cardcode, "[A-Z]{2}")[, 1],
      set = as.integer(stringr::str_match(.data$cardcode, "^[0-9]{2}")),
      card_number = as.integer(stringr::str_match(.data$cardcode, "[0-9]{3}$"))
    )

  decklist <- decklist %>%
    dplyr::left_join(faction_lut %>%
                       dplyr::select(.data$Integer_Identifier, .data$Faction_Identifier, .data$Version),
              by = c("faction" = "Faction_Identifier"))

  max_version <- max(decklist$Version, na.rm = TRUE)

  #first byte
  # the original encoder fixes this at 19, but the actual
  # game exports only based on the factions - do so as well
  version_format <- dplyr::case_when(
    max_version == 1 ~ 17, #i.e. 00010001 for version 1
    max_version == 2 ~ 18, #i.e. 00010010 for version 2
    max_version == 3 ~ 19, #i.e. 00010011 for version 3
    TRUE ~ 19 #fallback
  )

  int_out <- c(version_format)

  # minimalist example : "17" "1" "2" "1" "4" "8" "40" "0" "0"
  # version/format 17 ("00010001"), last 4 bits are version (1)
  # 1 group of 3 copies
  #   2 cards for combination of
  #   1 set 1   and
  #   4 faction 4 (PZ)
  #      8   card code 01PZ008
  #     40   card code 01PZ040
  # 0 group of 2 copies
  # 0 group if 1 copy

  # for 1-3 copies
  #-----------
  grouped_sorted_deck <- decklist %>%
    dplyr::filter(.data$count < 4) %>%
    dplyr::group_by(.data$count, .data$set, .data$faction) %>%
    dplyr::mutate(group_n = dplyr::n()) %>%
    # The sorting convention of this encoding scheme is
    # First by the number of set/faction combinations in each top-level list
    # Second by the alphanumeric order of the card codes within those lists.
    dplyr::arrange(dplyr::desc(.data$count), .data$group_n, .data$cardcode)

  output_by_count <- grouped_sorted_deck %>%
    dplyr::transmute(
      group_n = .data$group_n,
      Integer_Identifier = .data$Integer_Identifier,
      set = .data$set,
      cardid_ints = glue::glue_collapse(.data$card_number, sep = ", ")
    ) %>%
    unique() %>%
    dplyr::mutate(group_ints = glue::glue("{group_n}, {set}, {Integer_Identifier}, {cardid_ints}")) %>%
    dplyr::group_by(.data$count) %>%
    dplyr::summarize(
      toplevel_n = dplyr::n(),
      toplevel_ints = glue::glue_collapse(.data$group_ints, sep = ", "),
      .groups = "keep"
    ) %>%
    dplyr::transmute(top_ints = dplyr::case_when(stringr::str_detect(.data$toplevel_ints, "NA") ~ "0",
                                   # NA's should should only ever show up here
                                   # when we added invalid cardcodes for missing count levels
                                   TRUE ~ as.character(glue::glue("{toplevel_n}, {toplevel_ints}"))))

  int_out_3 <- (output_by_count %>% dplyr::filter(.data$count == 3) %>% dplyr::pull(.data$top_ints) %>%
                  stringr::str_split(pattern = ", "))[[1]]
  int_out_2 <- (output_by_count %>% dplyr::filter(.data$count == 2) %>% dplyr::pull(.data$top_ints) %>%
                  stringr::str_split(pattern = ", "))[[1]]
  int_out_1 <- (output_by_count %>% dplyr::filter(.data$count == 1) %>% dplyr::pull(.data$top_ints) %>%
                  stringr::str_split(pattern = ", "))[[1]]

  int_out <- c(int_out, int_out_3, int_out_2, int_out_1)

  # append for N > 3 cases
  # this will only happen in Limited and special game modes.
  # the encoding is simply [count] [cardcode]
  # simply sorted by card code
  if(max(decklist$count, na.rm = TRUE) > 3) {
    special_cases <- decklist %>%
      dplyr::filter(.data$count > 3) %>%
      dplyr::arrange(.data$cardcode) %>%
      dplyr::mutate(card_ints = glue::glue("{count}, {set}, {Integer_Identifier}, {card_number}"))

    for(c in special_cases$card_ints) {
      int_out <- c(int_out, stringr::str_split(c, pattern = ", ")[[1]])
    }

  }

  strtoi(int_out)

}


#' Parse integer representation of decklist
#' returns
#'
#' LoR uses a custom encoding scheme (\href{https://github.com/RiotGames/LoRDeckCodes}{laid out here})
#' to store information about the cards (and their count) in any given deck as an
#' integer vector. This function takes the integer vector and translates it
#' into a readable deck list with actual card codes and factions.
#'
#' @param cardinfo integer vector representing the deck list (without version info)
#' @returns dataframe with cardcode, count, faction, set and card number
#' @importFrom rlang .data
#' @examples
#' #minimalist example
#'
#' lordecks:::parse_decklist(c(1, 2, 1, 4, 8, 40, 0, 0))
#'
#' # returns  :
#' #   cardcode count faction set card_number
#' # 1  01PZ008     3      PZ   1         008
#' # 2  01PZ040     3      PZ   1         040
#' #
#' #
#' # 1 group of 3 copies
#' #    2 cards for combination of
#' #  1 set 1   and
#' #  4 faction 4 (PZ)
#' #      8   card code 01PZ008
#' #     40   card code 01PZ040
#' # 0 group of 2 copies
#' # 0 group if 1 copy
parse_decklist <- function(cardinfo) {

  faction_lut <- get_faction_lut()

  cardinfo <- as.integer(cardinfo)

  # minimalist example : "1" "2" "1" "4" "8" "40" "0" "0"
  # 1 group of 3 copies
  #   2 cards for combination of
  #   1 set 1   and
  #   4 faction 4 (PZ)
  #      8   card code 01PZ008
  #     40   card code 01PZ040
  # 0 group of 2 copies
  # 0 group if 1 copy

  if(any(is.na(cardinfo)|is.null(cardinfo))) {
    stop("at least one missing value in decoded decklist")
  }

  decklist <- data.frame("cardcode" = NULL,
                         "count" = NULL,
                         "faction" = NULL,
                         "set" = NULL,
                         "card_number" = NULL)

  for(i in 3:1) {

    n_of_groups <- cardinfo[1]
    cardinfo <- cardinfo[-1]

    if(n_of_groups > 0) {

      for(j in 1:n_of_groups) {

        n_in_group <- cardinfo[1]
        cur_set <- cardinfo[2]
        cur_faction <- faction_lut[faction_lut$Integer_Identifier==cardinfo[3], "Faction_Identifier"]

        for(k in 4:(n_in_group + 3)) {
          card_id <- sprintf('%03d', cardinfo[k])
          cardcode <- glue::glue("{sprintf('%02d', cur_set)}{cur_faction}{card_id}")

          decklist <- decklist %>%
            dplyr::bind_rows(data.frame("cardcode" = cardcode,
                                        "count" = i,
                                        "faction" = cur_faction,
                                        "set" = cur_set,
                                        "card_number" = card_id))
        }

        cardinfo <- cardinfo[-1:(-(n_in_group + 3))]
      }

    }

  }



  while(length(cardinfo) > 0) {
    #the remainder of the deck code is comprised of entries for cards with counts >= 4
    #this will only happen in Limited and special game modes.
    #the encoding is simply [count] [cardcode]

    count4plus <- cardinfo[1]
    cur_set <- cardinfo[2]
    cur_faction <- faction_lut[faction_lut$Integer_Identifier==cardinfo[3], "Faction_Identifier"]
    card_id <- sprintf('%03d', cardinfo[4])
    cardcode <- glue::glue("{sprintf('%02d', cur_set)}{cur_faction}{card_id}")

    decklist <- decklist %>%
      dplyr::bind_rows(data.frame("cardcode" = cardcode,
                                  "count" = count4plus,
                                  "faction" = cur_faction,
                                  "set" = cur_set,
                                  "card_number" = card_id))

    cardinfo <- cardinfo[-1:-4]
  }

  decklist  <- decklist %>% dplyr::mutate(cardcode = as.character(cardcode))

  decklist

}

