\name{readConfigFile}
\alias{readConfigFile}
\title{Load Configuration File}
\description{Load an "enhanced" Windows ini file, supporting
  multiple sections with same name and multiple values with
  same name.
}
\usage{
  readConfigFile(fileNameOrList, fromMemory=FALSE)
}
\arguments{
  \item{fileNameOrList}{Either a character vector with one filename or
      the contents of a file to parse.}
  \item{fromMemory}{If set to TRUE (or NA) the first argument is
      treated as character vector containing the contents of a
      configuration file.}
}
\value{
  The function returns NULL on error and otherwise a named list with
  the configuration file read in.
}

\details{
\subsection{Simple Configuration}
The following configuration file uses an integer to code the TCP/IP
port, a string to configure the address and a boolean switch whether
to generate verbose output.

\preformatted{port=6789
address="127.0.0.1"
verbose=
}

When read in, this will give the following result:
\preformatted{list(port=6789, address="127.0.0.1",verbose=TRUE)}

This easily can be used for configuration. All output values within
the lists are vectors, which R hides on first sight from you. You can
easily access the values e.g. in the following way:
\preformatted{conf <- readConfigFile("server.conf")
message("Start server using port:",conf[["port"]])
}

If in our example the server binds to more than one address, the
sample configuration file may look like this:
\preformatted{port=6789
address="127.0.0.1"
address="1.2.3.4"
}

This will return the following result:
\preformatted{list(port=6789,address=c("127.0.0.1","1.2.3.4"))}

\subsection{Sections in Configuration Files}
Let's have a look at the following configuration file:
\preformatted{[address]
ip="127.0.0.1"
port=6789

[logging]
verbose=
}

This configuration file uses sections. Every section will be a new
list in the result, so you get back:
\preformatted{list(
  address=c(list(ip="127.0.0.1",port=6789)),
  logging=c(list(verbose=TRUE))
)
}

As you may imagine, the same section may be there more than once, so a
section is always returned as list. A new section starts with every
next section within the config file and ends with either the next
section within the configuration file or with the end of the file.

\subsection{Mixing Sections with Configuration Variables}
It is possible to mix sections with simple configuration
variables. You just have to make sure not to use a top level
configuration variable with the same name as a section.

\subsection{Data Types}
The following data types are supported:
\itemize{
\item Integer
\item Double
\item Character
\item Boolean (Neither FALSE not NA are supported)
}

The content is always converted into the most common data type. So
using both integer and character for one configuration variable will
convert the integers into characters.
}

\examples{
  \dontrun{basetools::readConfigFile("my-configuration.conf")}
  \dontrun{basetools::readConfigFile(c("a=1","b=2"), fromMemory=TRUE)}
}
