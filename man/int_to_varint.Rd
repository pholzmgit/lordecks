% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/varint_handling.R
\name{int_to_varint}
\alias{int_to_varint}
\title{Translate integer vector into varint bytes}
\usage{
int_to_varint(int_array)
}
\arguments{
\item{int_array}{vector of positive integers}
}
\value{
If all inputs are smaller than 2e9, a character vector containing varint bytes
}
\description{
The function takes a vector of integers and translates them into a varint byte vector.
For varints, the most significant bit indicates whether the next byte belongs to the
same integer (1) or if it is the last byte (0), so the lenght of the input and output
vectors do not match as soon as one of the numbers is larger than 127.
}
\examples{
lordecks:::int_to_varint(c(3, 128, 12, 217))
# returns c("00000011", "10000000", "00000001", "00001100", "11011001", "00000001")
}
