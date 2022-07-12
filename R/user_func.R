
#' Get data from Hagstofan json px based on one known variable
#'
#' @param url the url to a json px source
#' @param variable known variable from the data
#'
#' @return something cool
#' @export
#'
#' @examples
#' \dontrun{
#' # url <- some http url from px.hagstofa.is or other px json website
#' # variable <- name of one variable in the px json
#' data <- hackstofan(url, variable)
#' }
hackstofan <- function(url, variable) {
  vals <- get_var_data(url, 'values')
  vars <- get_var_data(url, 'code')

  # Find which variable is the main variable
  index_splitter <- which(vars == variable)
  vals[index_splitter] <- c('*')
  # Create Query
  query_1 <- create_query(vars, vals)
  # Get data
  data_1 <- get_px(url, query_1)
  # Find length of unique values of main variable
  n <- length(data_1$data)
  # Find where the main variable is located
  main <- numeric(n)
  for(i in 1:n) {
    main[i] <- data_1$data[[i]]$key[[index_splitter]]
  }
  # Create query
  query_2 <- create_query(vars, rep(c('*'), length(vars)))
  # Initilize list
  data <- list()
  # Extract data based on variable
  for(i in 1:n) {
    # Query_2 updates with for loop based on variable
    query_2[[index_splitter]] <- main[i]
    # Get data
    tmp_px <- get_px(url, query_2)
    # How many data points
    N <- lengths(tmp_px)[3]
    # Initilize empty matrix
    matrix_i <- matrix(NA, N, length(vars) + 1)
    # Set colnames
    colnames(matrix_i) <- c(vars, 'Values')
    # Update new matrix
    for (j in 1:N) {
      matrix_i[j,] <- unlist(c(tmp_px$data[[j]][[1]], tmp_px$data[[j]][[2]]))
    }
    # Set as the data list
    data[[i]] <- matrix_i
    # Update human
    cat('Finished working on', i, 'out of', n, '\n')
  }
  # Get the variable ID's
  ID <- get_val_data(url, variable)

  if (length(ID) != length(data)) {
    ID <- clean_subcategories(ID)
  }

  # Add vector of variable ID's
  data <- mapply(cbind, data, ID=ID, SIMPLIFY=F)
  # Collapse list into a data.frame
  df <- data.frame(do.call("rbind", data))
  # Set value vector as ID with the variable ID's
  df[, variable] <- df$ID
  # Remove redundant vector
  df$ID <- NULL

  M <- ncol(df)
  bf <- df
  for (i in 1:(M-1)) {
    bf[, i] <- as.factor(bf[, i])
    levs <- get_val_data(url, vars[i])
    if (length(levs) != length(levels(bf[, i]))) {
      levs <- clean_subcategories(levs)
    }
    levs <- sort(levs)
    levels(bf[, i]) <- levs
  }

  # Return data.frame
  return(bf)
}
