#' translate decklist with cards and their count
#' into an integer representation following the rules
#' laid out in https://github.com/RiotGames/LoRDeckCodes/blob/master/README.md
gen_int_decklist <- function(decklist, faction_lut = get_faction_lut()) {

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
    decklist <- decklist %>% bind_rows(
      data.frame(count = 1, cardcode = "")
    )
  }
  if(sum(decklist$count == 2) == 0) {
    decklist <- decklist %>% bind_rows(
      data.frame(count = 2, cardcode = "")
    )
  }
  if(sum(decklist$count == 3) == 0) {
    decklist <- decklist %>% bind_rows(
      data.frame(count = 3, cardcode = "")
    )
  }


  decklist <- decklist %>%
    mutate(
      faction = str_match(cardcode, "[A-Z]{2}")[, 1],
      set = as.integer(str_match(cardcode, "^[0-9]{2}")),
      card_number = as.integer(str_match(cardcode, "[0-9]{3}$"))
    )

  decklist <- decklist %>%
    left_join(faction_lut %>% dplyr::select("Integer_Identifier", "Faction_Identifier", "Version"),
              by = c("faction" = "Faction_Identifier"))

  max_version <- max(decklist$Version, na.rm = TRUE)

  #first byte
  # the original encoder fixes this at 19, but the actual
  # game exports only based on the factions - do so as well
  version_format <- case_when(
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
    dplyr::filter(count < 4) %>%
    group_by(count, set, faction) %>%
    mutate(group_n = n()) %>%
    # The sorting convention of this encoding scheme is
    # First by the number of set/faction combinations in each top-level list
    # Second by the alphanumeric order of the card codes within those lists.
    arrange(desc(count), group_n, cardcode)

  output_by_count <- grouped_sorted_deck %>%
    dplyr::transmute(
      group_n = group_n,
      Integer_Identifier = Integer_Identifier,
      set = set,
      cardid_ints = glue::glue_collapse(card_number, sep = ", ")
    ) %>%
    unique() %>%
    mutate(group_ints = glue::glue("{group_n}, {set}, {Integer_Identifier}, {cardid_ints}")) %>%
    group_by(count) %>%
    dplyr::summarize(
      toplevel_n = n(),
      toplevel_ints = glue::glue_collapse(group_ints, sep = ", "),
      .groups = "keep"
    ) %>%
    transmute(top_ints = case_when(stringr::str_detect(toplevel_ints, "NA") ~ "0",
                                   # NA's should should only ever show up here
                                   # when we added invalid cardcodes for missing count levels
                                   TRUE ~ as.character(glue::glue("{toplevel_n}, {toplevel_ints}"))))

  int_out_3 <- (output_by_count %>% dplyr::filter(count == 3) %>% pull(top_ints) %>%
                  stringr::str_split(pattern = ", "))[[1]]
  int_out_2 <- (output_by_count %>% dplyr::filter(count == 2) %>% pull(top_ints) %>%
                  stringr::str_split(pattern = ", "))[[1]]
  int_out_1 <- (output_by_count %>% dplyr::filter(count == 1) %>% pull(top_ints) %>%
                  stringr::str_split(pattern = ", "))[[1]]

  int_out <- c(int_out, int_out_3, int_out_2, int_out_1)

  # append for N > 3 cases
  # this will only happen in Limited and special game modes.
  # the encoding is simply [count] [cardcode]
  # simply sorted by card code
  if(max(decklist$count, na.rm = TRUE) > 3) {
    special_cases <- decklist %>%
      dplyr::filter(count > 3) %>%
      arrange(cardcode) %>%
      mutate(card_ints = glue::glue("{count}, {set}, {Integer_Identifier}, {card_number}"))

    for(c in special_cases$card_ints) {
      int_out <- c(int_out, stringr::str_split(c, pattern = ", ")[[1]])
    }

  }

  int_out

}


#' parse integer representation of decklist
#' returns dataframe with cardcode, count, faction, set and card number
parse_decklist <- function(cardinfo, faction_lut = get_faction_lut()) {

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

  decklist

}

