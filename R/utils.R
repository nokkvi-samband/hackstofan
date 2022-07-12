#' Get px from hagstofan or other px sources
#'
#' @param url the url to a json px source
#' @param queryList a list of data from px source
#'
#' @return something neat
#'
get_px <- function(url, queryList){
  # Extract data
  pxweb::pxweb_get(url = url, verbose = FALSE, query = queryList)
}


#' Create query for queryList
#'
#' @param vars variables
#' @param vals values
#'
#' @return the "queryList" query
create_query <- function(vars, vals){
  # Length of variables needs to be same as length og values
  if(length(vars) != length(vals)) {
    warning('Variables are not of the same length as values')
  } else {
    # Initialize list
    query <- list()
    for (i in 1:length(vars)) {
      # Create query list
      query[[vars[i]]] <- vals[i]
    }
    return(query)
  }
}


#' Clean the json from Hagstofan and extract either example values or example variables
#'
#' @param url the url to a json px source
#' @param identity identity of data needed to extract ('value' or 'code')
#'
#' @return example data
get_var_data <- function(url, identity) {
  # ReadLines
  rl <- suppressWarnings(readLines(url))
  # Remove all special characters
  rl <- gsub('[][!#$%()*.:;<=>@^_`|~.{}"]', "", rl)
  # Split by identity
  rl <- unlist(strsplit(rl, identity))
  # Delete everything after ","
  rl <- sub("\\,.*", "", rl)
  # Only keep 2nd value to longer values
  rl <- rl[2:length(rl)]
  # Return
  return(rl)
}


#' Get factor values from variable
#'
#' @param url the url to a json px source
#' @param variable the name of a variable i.e. a column variable
#'
#' @return example data
get_val_data <- function(url, variable) {
  # ReadLines
  rl <- suppressWarnings(readLines(url))
  # Remove all special characters
  rl <- gsub('[][!#$%()*:.;<=>@^_`|~.{}"]', "", rl)
  # Split by "code"
  rl <- trimws(unlist(strsplit(rl, 'code')))
  # Remove empty values - Safety
  rl <- rl[!rl == '']
  # Extract everything after valueTexts in variable cell
  rl <- gsub(".*valueTexts", "", rl[utils::tail(grep(variable, rl), 1)])

  ####
  # Anomalies in Hagstofan px json - this will be updated as often as possible
  rl <- gsub(', anna', '- anna', rl)
  rl <- gsub(',timetrue', '', rl)
  ####

  # Split by ","
  rl <- trimws(unlist(strsplit(rl, ',')))
  # Remove empty values - Safety
  rl <- rl[!rl == '']
  return(rl)
}


#' Cleaning subcategorical values
#'
#' @param values values from get_val_data function
#'
#' @return something cool
clean_subcategories <- function(values) {
  # If statement for values that are actually subcategories
  no_str <- length(values)
  # Find which numbers are numeric
  number <- suppressWarnings(!is.na(as.numeric(values)))
  # Find how many are numeric
  no_non_num <- sum(!number)
  # If names is split with some values numeric and some of char
  if (no_non_num != no_str | 0 != no_str){
    # Add NA to back of values, used for the for-loop
    rl <- c(values, NA)
    number[length(rl)] <- FALSE
    # Initialize vector
    newrl <- rep(NA, length(rl))
    for (i in 2:length(rl)) {
      if ((number[i-1] == FALSE) & (number[i] == TRUE)) {
        k <- i
        while (number[k] == TRUE){
          k <- k + 1
        }
        newrl[i-1] <- paste(rl[i:k-1], collapse = ' ')
      } else if ((number[i-1] == FALSE) & (number[i] == FALSE)) {
        newrl[i-1] <- rl[i-1]
      }
      else {
        newrl[i-1] <- NA
      }
    }
    rl <- newrl[!is.na(newrl)]
  }
  return(rl)
}








