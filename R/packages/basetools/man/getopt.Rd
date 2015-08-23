\name{getopt}
\alias{getopt}
\title{Parse command line options}
\description{Load an "enhanced" Windows ini file, supporting
  multiple sections with same name and multiple values with
  same name.
}
\usage{
  getopt(options, arguments)
}

\arguments{
  \item{options}{A list where names are allowed option characters and values
    are TRUE or FALSE, whether the option requires an argument or not.}
  \item{arguments}{An argument vector as received by \code{commandArgs()}.
    Arguments must follow the option character like in \code{-fFilename}.
    Options are recognized by a leading \code{-} sign. An argument
    \code{--} will stop option parsing. All following arguments are
    not treated as options.}
}
\value{
  The function returns NULL on error and otherwise a named list with
  the options parsed in character vector. If options are given twice,
  the character vector will have more than one entry. Options that
  do not require an argument will receive an empty string for each
  time the option is given.
  The non-options parsed are returned in position 1 of the result list.
  This character vector will be empty if no non-option arguments are
  given.
}


\examples{
  \dontrun{basetools::getopt(list("a"=FALSE,"b"=TRUE,c("-a","-b25","foo","bla")))}
}
