# Author - Jan Korevaar
# Course - Mastering Software Development in R Capstone
# Last Edited 20/10/2020

### testing
#earthquakes <- readr::read_delim(filename, delim = "\t")
#eq_clean_data(earthquakes)


### eq_clean_data --------------------------------------------------------------

#' eq_clean_data
#' loads earthquake data.
#' @param earthquakes the file that contains the earthquake data
#' @return Returns earthquake data with the following cleaning -a- added 01 for
#' empty months and days and years -b- removed dates in bc -c- created a DATE
#' column with date format -d- turned long and lat to numeric -d- use functions
#' to clean Location Names and Country Names
#' @importFrom readr read_delim
#' @examples
#' \dontrun{
#' earthquakes <- readr::read_delim("earthquakes-2020-10-19_15-21-05_+0300.tsv",
#' delim = "\t")
#' eq_clean_data(earthquakes)
#' }
#' @export

eq_clean_data <- function(earthquakes) {
  result <- data.frame(earthquakes)
  #fill empty data data
  result$Mo[is.na(result$Mo)] = 01
  result$Dy[is.na(result$Dy)] = 01
  result$Year[is.na(result$Year)] = 01
  #create date column
  result$DATE = as.POSIXct(strptime(paste(result$Year,
                                          result$Mo,
                                          result$Dy,
                                        sep="-"), "%Y-%m-%d"))
  # filter out BC
  result = result[!is.na(result$DATE), ]
  LATITUDE <- as.numeric(result$Latitude)
  result$LONGITUDE <- as.numeric(result$Longitude)
  result$TOTAL_DEATHS <- as.numeric(result$Total.Deaths)
  result$LOCATION_NAME <- eq_location_clean(result$Location.Name)
  result$COUNTRY <- eq_country_clean(result$Location.Name)
  result
}

### eq_location_clean ----------------------------------------------------------

#' eq_location_clean
#' cleans location column and removes country data
#' @param loc_col the column that holds the location data
#' @return returns a data file where location is -a- trimmed of ws -b- converted
#' to title case -c- name of country removed
#' @importFrom stringr str_to_title
#' @examples
#' \dontrun{
#' eq_location_clean("Canada:Toronto")
#' }
#' @export

eq_location_clean <- function(loc_col) {
  loc_col = gsub(".*:","",loc_col)
  loc_col = stringr::str_to_title(trimws(loc_col))
  return(loc_col)
}

### eq_country_clean ----------------------------------------------------------

#' eq_country_clean
#' cleans location column only leaving the country
#' @param loc_col the column that holds the location data
#' @return returns a data file where location is -a- trimmed of ws -b- converted
#' to title case -c- name of country removed
#' @importFrom stringr str_to_title
#' @examples
#' \dontrun{
#' eq_country_clean("Canada:Toronto")
#' }
#' @export

eq_country_clean <- function(loc_col) {
  loc_col = gsub(":.*","",loc_col)
  loc_col = stringr::str_to_title(trimws(loc_col))
  return(loc_col)
}

### geom_timeline --------------------------------------------------------------

#' geom_timeline
#' plots a time line of earthquakes with a point for each earthwquake. x axis is
#' date and y axis is a factor
#' @param data The earthquake data to be plotted. See the example for details.
#' @param mapping mapping
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.as
#' @param ... ...
#' @return ggplot2 graphical object of earthquakes
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @examples
#' \dontrun{
#' library(ggplot2)
#' eq = eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv")
#' eq = dplyr::filter(eq, COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000)
#' ggplot2::ggplot(eq,aes(x=DATE,y = COUNTRY, color = TOTAL_DEATHS, size = TOTAL_DEATHS)) +
#'   geom_timeline(alpha=.5)
#' }
#' @export

geom_timeline <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimeline,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",
  ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "color"),
  default_aes = ggplot2::aes(
    shape = 19,
    color = "black",
    size = 1.5,
    alpha = NA,
    fill = NA,
    stroke = 0.5
  ),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord) {
    coords <- coord$transform(data, panel_scales)
    grid::grobTree(
      grid::segmentsGrob(
        x0 = 0.0,
        y0 = coords$y,
        x1 = 1.0,
        y1 = coords$y,
        gp = grid::gpar(col = "grey")
      ),
      grid::pointsGrob(
        x = coords$x,
        y = coords$y,
        pch = coords$shape,
        gp = grid::gpar(
          col = alpha(coords$colour, coords$alpha),
          fill = alpha(coords$fill, coords$alpha),
          fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
          lwd = coords$stroke * .stroke / 2
        )
      )
    )
  }
)

### geom_timeline_label --------------------------------------------------------

#' geom_timeline
#' This geom is intended to be used in conjunction with the geom_timeline geom
#' to add a vertical line with a text annotation
#' @param data The earthquake data to be plotted. See the example for details.
#' @param mapping mapping
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#' @return ggplot2 graphical object of earthquakes
#' @import ggplot2
#' @import dplyr
#' @examples
#' \dontrun{
#' library(ggplot2)
#' eq = eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv")
#' eq = dplyr::filter(eq, COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000)
#' ggplot2::ggplot(eq,aes(x=DATE,y = COUNTRY, color = TOTAL_DEATHS, size = TOTAL_DEATHS)) +
#'   geom_timeline(alpha=.5) +
#'   geom_timeline_label(aes(label = LOCATION_NAME, n_max = 5))
#' }
#' @export

geom_timeline_label <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    ggplot2::layer(
      geom = GeomTimelineLabel,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }

GeomTimelineLabel <-
  ggplot2::ggproto(
    "GeomTimelineLabel",
    ggplot2::Geom,
    required_aes = c("x", "label"),
    default_aes = ggplot2::aes(n_max = NA),
    setup_data = function(data, params) {
      n <- data$n_max[1]
      if (is.numeric(n)) {
        dplyr::top_n(dplyr::group_by_(data, "group"), n, size)
      } else {
        data
      }
    },
    draw_panel = function(data, panel_scales, coord) {
      coords <- coord$transform(data, panel_scales)
      grid::grobTree(
        grid::segmentsGrob(
          x0 = coords$x,
          y0 = coords$y,
          x1 = coords$x,
          y1 = coords$y + 0.1,
          gp = grid::gpar()
        ),
        grid::textGrob(
          x = coords$x,
          y = coords$y + 0.1,
          label = coords$label,
          rot = 45,
          hjust = -0.1,
          vjust = -0.1,
          gp = grid::gpar()
        )
      )
    }
  )


### eq_map  ------------------------------------------------------------------

#' eq_map
#' Produces a map of earthquake epicenters and annotates
#' each point with a popup window
#' @param data The earthquake data to be plotted. See the example for details.
#' @param annot_col the column that contains the annotations
#' @return leaflet object with earthquake data
#' @import leaflet
#' @examples
#' \dontrun{
#' library(dplyr)
#' eq = eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv")
#' eq = dplyr::filter(eq, COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000)
#' eq_map(eq, annot_col = "COUNTRY")
#' }
#' @export

eq_map = function(data, annot_col){
  map = addTiles(leaflet())
  map = addCircleMarkers(
                     map
                     ,data = data
                     ,lng = ~Longitude
                     ,lat = ~Latitude
                     ,radius = ~Mag*2
                     ,weight = 1
                     ,popup = data[, annot_col])
  return(map)
}

### eq_create_label ------------------------------------------------------------

#' eq_create_label
#' produces a column in the earthquake data that contains the information
#' necessary for a popu-up - location, magnitude, total deaths
#' @param df The earthquake dataframe
#' @return string with labels for the popup
#' @examples
#' \dontrun{
#' library(dplyr)
#' eq = eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv")
#' eq = dplyr::mutate(eq, popup_text = eq_create_label(eq))
#' eq_map(eq, annot_col = "popup_text")
#' }
#' @export

eq_create_label <- function(df) {
  paste(sep = "<br/>"
        ,paste("<b>Location:</b>", df$LOCATION_NAME)
        ,paste("<b>Magnitude:</b>", df$Mag)
        ,paste("<b>Total deaths:</b>", df$Total.Deaths)
  )
}


