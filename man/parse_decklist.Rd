% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decklist_handling.R
\name{parse_decklist}
\alias{parse_decklist}
\title{Parse integer representation of decklist
returns}
\usage{
parse_decklist(cardinfo)
}
\arguments{
\item{cardinfo}{integer vector representing the deck list (without version info)}
}
\value{
dataframe with cardcode, count, faction, set and card number
}
\description{
LoR uses a custom encoding scheme (\href{https://github.com/RiotGames/LoRDeckCodes}{laid out here})
to store information about the cards (and their count) in any given deck as an
integer vector. This function takes the integer vector and translates it
into a readable deck list with actual card codes and factions.
}
\examples{
#minimalist example

lordecks:::parse_decklist(c(1, 2, 1, 4, 8, 40, 0, 0))

# returns  :
#   cardcode count faction set card_number
# 1  01PZ008     3      PZ   1         008
# 2  01PZ040     3      PZ   1         040
#
#
# 1 group of 3 copies
#    2 cards for combination of
#  1 set 1   and
#  4 faction 4 (PZ)
#      8   card code 01PZ008
#     40   card code 01PZ040
# 0 group of 2 copies
# 0 group if 1 copy
}
