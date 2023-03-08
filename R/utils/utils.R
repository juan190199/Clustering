#'Prepares data for clustering.
#'
#'`read_input` can read in data from a .csv or .json file or directly from a data.frame
#' Then the data is transformed into a data.frame with the data points on the columns.
#'
#' @section Note:
#' When using a json file one needs to ensure that the data can be directly transferred into
#' a table, e.g. column names as keys and entries of the same length
#'
#'@param filename character, contains the path to the .json or .csv file.
#'@param frame data.frame, that contains the data.
#'@param cols atomic character vector, the columns that should be used for clustering.
#'
#'@return data.frame, with the preprocessed data that can be clustered.
read_input <- function(filename, frame, cols){
  stopifnot("Either filename or frame should be given"=
              xor(missing(filename), missing(frame)))

  if(!missing(filename)){
    stopifnot("Filename has to be a character"=
                is.character(filename))
    if(endsWith(filename, ".json")){
      data <- rjson::fromJSON(file = filename)
      data <- as.data.frame(data)
    }
    else if(endsWith(filename, ".csv")){
      data <- readr::read_csv(filename)
    }
  }
  else{
    stopifnot("Frame has to be a dataframe"=
                is.data.frame(frame))
    data <- frame
  }

  if(!missing(cols)){
    stopifnot("cols has to be an atomic character vector"=
                is.atomic(cols) && is.character(cols))
    stopifnot("cols can't be empty"=
                length(cols) > 0 )
    data %>% dplyr::select(all_of(cols)) -> data
  }

  stopifnot("Cluster Frame can only contain numeric data, provide cols that select only
            numeric columns"=
              all(sapply(data, is.numeric)))

  return(data)
}

dist_func <- function(p, q, type = "euclidean") {
  # Check input data
  stopifnot(is.numeric(p) && is.vector(p) &&
              is.numeric(q) && is.vector(q))
  stopifnot(length(p) == length(q))

  # Calculate distance based on type
  if (type == "euclidean") {
    distance <- sqrt(sum((p - q)^2))
  } else if (type == "manhattan") {
    distance <- sum(abs(p - q))
  } else if (type == "minkowski") {
    p_val <- 3 # Change to desired p-value
    distance <- (sum(abs(p - q)^p_val))^(1/p_val)
  } else {
    stop("Invalid distance type. Choose 'euclidean', 'manhattan', or 'minkowski'.")
  }

  return(distance)
}

norm_vec <- function(x, type = "euclidean") {
  dist_func(x, rep(0, length(x)), type=type)
}
