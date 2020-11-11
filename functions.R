# Author - Jan Korevaar
# Course - Mastering Software Development in R Capstone
# Last Edited 20/10/2020

### eq_clean_data --------------------------------------------------------------

#' eq_clean_data
#' loads earthquake data.
#' @param filename Name of the file containing earthquake data
#' @return Returns earthquake data with the following cleaning -a- added 01 for
#' empty months and days -b- removed dates in bc -c- created a DATE column with
#' date format -d- turned long and lat to numerics
#' @examples
#' eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv")

eq_clean_data <- function(earthquakes) {

  #delete first row - empty result - and first column - empty col
  earthquakes = earthquakes[-1, -1]

  result <- data.frame(earthquakes)

  #fill empty data data
  result$Mo[is.na(data$Mo)] = 01
  result$Dy[is.na(data$Dy)] = 01
  result$Year[is.na(data$Year)] = 01
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
#' cleans location column
#' @param data the data that is returned from eq_clean_data
#' @return returns a data file where location is -a- trimmed of ws -b- converted
#' to title case -c- name of country removed
#' @importFrom stringr str_to_title
#' @examples
#' eq_location_clean(eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv"))

eq_location_clean <- function(location) {
  location = gsub(".*:","",location)
  location = stringr::str_to_title(trimws(location))
  return(location)
}

### eq_country_clean ----------------------------------------------------------

#' eq_location_clean
#' cleans location column
#' @param data the data that is returned from eq_clean_data
#' @return returns a data file where location is -a- trimmed of ws -b- converted
#' to title case -c- name of country removed
#' @importFrom stringr str_to_title
#' @examples
#' eq_location_clean(eq_clean_data("earthquakes-2020-10-19_15-21-05_+0300.tsv"))

eq_country_clean <- function(location) {
  location = gsub(":.*","",location)
  location = stringr::str_to_title(trimws(location))
  return(location)
}

### geom_timeline --------------------------------------------------------------

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

readr::read_delim("earthquakes-2020-10-19_15-21-05_+0300.tsv", delim = "\t") %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = "DATE")

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

readr::read_delim("earthquakes-2020-10-19_15-21-05_+0300.tsv", delim = "\t") %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "Mexico" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")

eq_create_label <- function(df) {
  paste(sep = "<br/>"
        ,paste("<b>Location:</b>", df$LOCATION_NAME)
        ,paste("<b>Magnitude:</b>", df$Mag)
        ,paste("<b>Total deaths:</b>", df$Total.Deaths)
  )
}








xxx = read.table(file = "earthquakes-2020-10-19_15-21-05_+0300.tsv", sep = '\t', header = TRUE)
xxx <- eq_clean_data(xxx) %>%
  dplyr::filter(lubridate::year(DATE) >= 2020 & !is.na(Total.Deaths))


xxx %>%
ggplot(aes(x=DATE,y = COUNTRY, color = TOTAL_DEATHS, size = TOTAL_DEATHS)) +
geom_timeline(alpha=.5) +
geom_timeline_label(aes(label = LOCATION_NAME, n_max = 5))




