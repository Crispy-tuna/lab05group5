#' Stamen map API
#'
#' @param name city name
#' @param zoom_l zoom level
#' @param map_type map type
#'
#' @return a ggmap plot
#' @export
#' @import ggmap
#' @importFrom ggmap get_stamenmap
get_infor <- function(name, zoom_l, map_type) {
  map_types = c("terrain",
                "toner",
                "watercolor")
  if (zoom_l > 12 | zoom_l < 9) {
    cat("Wrong input! ")
    cat("Map zoom level(9 to 12):")
  }
  if (map_type %in% map_types == FALSE) {
    stop()
  }
  infor = list("name" = name,
               "zoom_l" =  zoom_l,
               "map_type" =  map_type)
  return(infor)
}


#' display a map
#'
#' @param name city name
#' @param zoom_l zoom level
#' @param map_type map type
#'
#' @return a ggmap plot
#' @export
#' @import ggmap
#' @importFrom ggmap get_stamenmap
openmap_by_name <-
  function(name, zoom_l, map_type) {
    if (zoom_l > 12 | zoom_l < 9) {
      stop()
    }
    n_url = paste0("https://nominatim.openstreetmap.org/search.php?q=",
                   name,
                   "&format=jsonv2")
    url1 = httr::GET(url = n_url)
    url_text =  httr::content(url1, "text")
    json = rjson::fromJSON(url_text)
    location_data =  data.frame(
      name = json[[1]]$display_name,
      bbox = json[[1]]$boundingbox,
      lat = json[[1]]$lat,
      lon = json[[1]]$lon
    )
    bbox0 = as.numeric(
      c(
        left = location_data$bbox[3],
        bottom = location_data$bbox[1],
        right = location_data$bbox[4],
        top = location_data$bbox[2]
      )
    )
    map = ggmap(get_stamenmap(
      bbox = bbox0,
      zoom = zoom_l,
      maptype = map_type
    ))

    return(map)
  }
