\name{strip}
\alias{strip}
\title{Strip strings from white space}
\description{Strip a string from white space from left, right or both}
\usage{
  strip(string, left=NA)
}
\arguments{
  \item{string}{A character vector of strings to be stripped -- or a list of them.}
  \item{left}{Whether to strip strings from right, left or both right and left.
    TRUE strips from left only, FALSE strips from right only and NA strips both.}
}
\value{A character vector or list of character vectors organized the same way
    as functions input, but each string stripped from right, left or both.
}
\details{
Tab or spaces are stripped from left and / or right. Line ends are always stripped
from right. This concerns lines ending in carriage return, lines ending in newline
or lines ending in carriage return and newline. Multiple newlines are not stripped.
}
\examples{
  \dontrun{basetools::strip(" abc ")}
}
