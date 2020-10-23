# Author - Jan Korevaar
# Course - Mastering Software Development in R Capstone
# Last Edited 20/10/2020

xxx <- eq_location_clean(eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv"))

### eq_clean_data --------------------------------------------------------------

#' eq_clean_data
#' loads earthquake data.
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

### eq_location_clean ----------------------------------------------------------

#' eq_location_clean
#' cleans location column
#' @param data the data that is returned from eq_clean_data
#' @return returns a data file where location is -a- trimmed of ws -b- converted
#' to title case -c- name of country removed
#' @importFrom stringr str_to_title
#' @examples
#' eq_location_clean(eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv"))

eq_location_clean <- function(data){
  # remove everything before colon
  data$Location.Name <- gsub(".*:","",data$Location.Name)
  #convert to title case & trim white space
  data$Location.Name <- stringr::str_to_title(trimws(data$Location.Name))

  return(data)
}

### geom_timeline --------------------------------------------------------------


geom_timeline<- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(y = 0, colour = "grey", size = 1,
                                                            alpha = 0.5, shape = 20, fill = NA, stroke = 1),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_scales, coord) {

                                   coords <- coord$transform(data, panel_scales)
                                   grid::gList(
                                     grid::pointsGrob(x = coords$x, y= coords$y,
                                                      pch = coords$shape,
                                                      gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
                                                                      fill = alpha(coords$fill, coords$alpha),
                                                                      fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                                      lwd = coords$stroke * .stroke / 2)
                                     ),
                                     grid::segmentsGrob(x0=min(coords$x), y0= coords$y, x1 = max(coords$x), y1 = coords$y,
                                                        gp = grid::gpar(col = "blue", lwd = 1))
                                   )
                                 }
)

library(ggplot2)
library(dplyr)
xxx <- eq_location_clean(eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv"))

xxx %>%
dplyr::filter(lubridate::year(DATE) >= 2019 & !is.na(Total.Deaths)) %>%
ggplot(aes(x=DATE,color=Total.Deaths, size = Total.Deaths)) +
geom_timeline(alpha=.5)



