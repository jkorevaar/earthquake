# Author - Jan Korevaar
# Course - Mastering Software Development in R Capstone
# Last Edited 20/10/2020


#' eq_clean_data
#'
#' loads earthquake data.
#'
#' @param filename Name of the file containing earthquake data
#' @return Returns earthquake data with the following cleaning -a- added 01 for
#' empty months and days -b- removed dates in bc -c- created a DATE column with
#' date format -d- turned long and lat to numerics
#' @examples
#' eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv")


eq_clean_data <- function(filename){
  #check if file name exists
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")

  #read the data
  data = read.table(file = filename, sep = '\t', header = TRUE)

  #delete first row - empty result - and first column - empty col
  data = data[-1, -1]

  #fill empty data data
  data$Mo[is.na(data$Mo)] = 01
  data$Dy[is.na(data$Dy)] = 01

  #create date column
  data$DATE = as.POSIXct(strptime(paste(data$Year,
                                   data$Mo,
                                   data$Dy,
                                   sep="-"), "%Y-%m-%d"))

  # filter out BC
  data = data[!is.na(data$DATE), ]

  # turn LAT and LONG into numeric
  data$Longitude = as.numeric(data$Longitude)
  data$Latitude = as.numeric(data$Latitude)

  return(data)
}


