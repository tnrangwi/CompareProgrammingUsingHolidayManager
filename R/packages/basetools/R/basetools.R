#Very simple getopt implementation. For demonstration purposes only.
#TODO: Documentation: Receives a named list <opt> specifying the expected option
#      characters with TRUE or FALSE depending on required arguments or not.
#      Second <argv> is a character vector as received by e.g. commandArgs()
#      Returns a named list with arguments in position 1 and rest named from
#      position 0. Options both with and without arguments are not supported.
getopt <- function(opt, argv) {

  typecheck <- function(v) typeof(v) == "locical" && length(v) == 1
  
  if (typeof(opt) == "list" && all(sapply(opt, typecheck)) && typeof(argv) == "character") {
    warning("Invalid data type for getopt")
    return(NULL)
  }
  result <- list(character(0))
  if (length(argv) == 0) return(result)

  for (c in 1:length(argv)) {
    v <- argv[c]
    if (v == "--") {
      result[[1]] <- c(result[[1]],argv[-1:-c])
      break
    } else if (nchar(v) >= 2 && substr(v, 1, 1) == "-") {
      o = substr(v, 2, 2)
      if (is.null(opt[[o]])) {
        warning(paste("Unsupported option in getopt:",v))
        return(NULL)
      }
      vlen <- nchar(v)
      if(opt[[o]] == TRUE) {
        if(vlen >= 3) {
          result[[o]] <- c(result[[o]],substring(v,3))
        } else {
          warning(paste("Option requires an argument:",v))
          return(NULL)
        }
      } else {
        if(vlen == 2) {
          result[[o]] <- c(result[[o]],"")
        } else {
          warning(paste("Option does not support an argument:",v))
          return(NULL)
        }
      }
    } else {
      result[[1]] <- c(result[[1]], v)
    }
  }
  return(result)
}
