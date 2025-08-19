#' Identify closest weather station to a specified point (default = five closest stations).
#' 
#' This function makes queries using WBAN numbers and ICAO identifiers.
#' 
#' Prior to filtering, stations are removed which do not have a valid WBAN or ICAO (might be buoys, etc.).
#' 
#' @param data Data frame. Must contain latitude, longitude, date, day label, and subject identifier.
#' @param stations Data frame. Must contain latitude, longitude, WBAN, and ICAO.
#' @param day_label_value Numeric, character, or factor. The value of the day to filter the GPS data for when identifying top locations.
#' @param n_stn Numeric. The number of weather stations to return. Allows returning multiple closest weather stations if more than one is desired.
#' @param data_lat_col Character. Name of the column in `data` that contains latitude values.
#' @param data_lon_col Character. Name of the column in `data` that contains longitude values.
#' @param stn_lat_col Character. Name of the column in `stations` that contains latitude values.
#' @param stn_lon_col Character. Name of the column in `stations` that contains longitude values.
#' @param stn_wban_col Character. Name of the column in `stations` that contains WBAN values.
#' @param stn_icao_col Character. Name of the column in `stations` that contains ICAO values.
#' 
#' @return A data frame containing `station_index`, `subid`, `date`, `wban`, `icao`, day label, and coordinates of the closest station(s).
#' 
#' @examples
#' identify_stations(data = gps_data, stations = station_data,
#'                     day_label_value = 1, n_stn = 5,
#'                     data_lat_col = "lat", data_lon_col = "lon",
#'                     stn_lat_col = "lat", stn_lon_col = "lon",
#'                     stn_wban_col = "wban", stn_icao_col = "icao")

identify_stations <- function(data, stations, day_label_value, n_stn = 5,
                              data_lat_col = "lat", data_lon_col = "lon",
                              stn_lat_col = "lat", stn_lon_col = "lon",
                              stn_wban_col = "wban", stn_icao_col = "icao") {
  
  # filter by day
  tmp <- data |> filter(.data[["day_label"]] == day_label_value)
  
  # filter out stations which do not have either a valid WBAN or ICAO
  valid_stn <- stations[!(stations[[stn_wban_col]] == 99999 & is.na(stations[[stn_icao_col]])), ]
  
  # create a distance matrix between GPS points and valid stations
  dist_matrix <- matrix(nrow = nrow(tmp), ncol = nrow(valid_stn))
  for (i in 1:nrow(tmp)) {
    for (j in 1:nrow(valid_stn)) {
      dist_matrix[i, j] <- geosphere::distGeo(
        c(tmp[[data_lon_col]][i], tmp[[data_lat_col]][i]),
        c(valid_stn[[stn_lon_col]][j], valid_stn[[stn_lat_col]][j])
      )
    }
  }
  
  # convert distance matrix to long format and select n_stn closest stations
  results <- dist_matrix |> 
    as_tibble() |>
    pivot_longer(cols = everything(), names_to = "station_index", values_to = "distance") |>
    mutate(
      station_index = as.integer(gsub("V", "", station_index)),
      subid = tmp$subid,
      day_label = day_label_value,
      date = tmp$date,
      wban = valid_stn[[stn_wban_col]][station_index],
      icao = valid_stn[[stn_icao_col]][station_index]
    ) |>
    slice_min(order_by = distance, n = n_stn)
  
  return(results)
}