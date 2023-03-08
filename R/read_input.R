#'Prepares data for clustering.
#'
#'`read_input` can read in data from a .csv or .json file or directly from a data.frame
#' Then the data is transformed into a data.frame with the data points on the columns.
#' TODO Note about the format of the json file
#'
#'@param filename character, contains the path to the .json or .csv file.
#'@param frame data.frame, that contains the data.
#'@param cols atomic character vector, the columns that should be used for clustering.
#'
#'@return A data.frame with the data that can be clustered.
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
