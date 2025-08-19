#' Get the longest-duration location for a given day
#' 
#' If multiple maximum duration points, pull first from list by default
#' 
#' @param data Data frame. Must contain latitude, longitude, day labels, and duration information.
#' @param day_label_value Numeric, character, or factor. The value of the day to filter the GPS data for when identifying top locations.
#' @param day_col Character. Name of the column in `data` that contains labeled days.
#' @param lat_col Character. Name of the column in `data` that contains latitude values.
#' @param lon_col Character. Name of the column in `data` that contains longitude values.
#' @param duration_col Character. Name of the column in `data` that contains duration values.
#' @param top_n Numeric. The number of long-duration locations to return. Allows returning multiple top locations if more than one is desired.
#' 
#' @return A data frame containing `subid`, `date`, day label, and coordinates of the top location(s) by duration.
#' 
#' @examples
#' get_top_locations(data = gps_data, day_label_value = 1,
#'              day_col = "day_label", lat_col = "lat",
#'              lon_col = "lon", duration_col = "duration", top_n = 1)

get_top_locations <- function(data, day_label_value, day_col,
                         lat_col, lon_col,
                         duration_col,
                         top_n = 1) {
  
  # filter by day
  tmp <- data |> filter(.data[[day_col]] == day_label_value)
  
  # summarize duration by location
  tmp_longest <- tmp |> 
    group_by(.data[[lat_col]], .data[[lon_col]]) |> 
    summarize(duration = sum(.data[[duration_col]]), .groups = "drop") |>
    slice_max(order_by = duration, n = top_n)
  
  # returns rows for matching top locations
  result <- tmp |> 
    filter((.data[[lat_col]] %in% tmp_longest[[lat_col]]) &
             (.data[[lon_col]] %in% tmp_longest[[lon_col]])) |> 
    select(any_of(c("subid", "date", day_col, lat_col, lon_col)))
  
  return(result)
}