#' Pull relevant weather data for a given subject on a given day.
#' 
#' Tries WBAN first, then ICAO if WBAN fails.
#' 
#' Returns a data frame with weather data for a single GPS observation.
#' 
#' @param obs_label_value Numeric, character, or factor. Identifier for the GPS observation to fetch weather for.
#' @param data Data frame. Contains station matches for the observation.
#' @param obs_label_col Name of the column in `data` containing observation labels.
#' @param wban_col Name of the column in `data` containing WBAN identifiers.
#' @param icao_col Name of the column in `data` containing ICAO identifiers.
#' @param date_col Name of the column in `data` containing observation date.
#' @param subid_col Name of the column in `data` containing subject ID.
#' @param elements Character vector of weather elements to fetch. Defaults to include average temperature (`avgt`),
#' minimum temperature (`mint`), maximum temperature (`maxt`), snow (`snow`), snow depth (`snwd`), & precipitation (`pcpn`)
#' @param tidyup Boolean. If set to TRUE (default), missing values labeled as 'M' will be converted to NA and trace amounts of snow
#' labeled as 'T' will be set to 0.001. It is recommended to set this to FALSE if you would like to handle these artifacts separately.
#' @param verbose Boolean. Flag for printing output of which observations fail. Default is FALSE.
#'
#' @return Data frame with weather elements for the given observation.
#' 
#' @examples
#' pull_weather(obs_label_value = 1, data = observation_data, wban_col = "wban",
#' icao_col = "icao", date_col = "date", subid_col = "subid", elements = "avgt")

pull_weather <- function(obs_label_value, data,
                         obs_label_col = "obs_label", wban_col = "wban",
                         icao_col = "icao", date_col = "date", subid_col = "subid",
                         elements = c("avgt", "mint", "maxt",
                                      "snow", "snwd", "pcpn"),
                         tidyup = TRUE, verbose = FALSE) {
  
  # filter down to relevant row
  tmp <- data |> filter(.data[[obs_label_col]] == obs_label_value)
  
  subid <- tmp[[subid_col]]
  wban <- tmp[[wban_col]]
  icao <- tmp[[icao_col]]
  query_date <- tmp[[date_col]]
  
  # helper function for interfacing with ACIS API
  fetch_acis <- function(station_id) {
    api_url <- paste0(
      "https://data.rcc-acis.org/StnData",
      "?sid=", station_id,
      "&date=", query_date,
      "&elems=", paste(elements, collapse = ","),
      "&output=csv"
    )
    
    df <- tryCatch({
      read_csv(url(api_url), col_names = FALSE, skip = 1,
               col_types = cols(.default = "c"),
               show_col_types = FALSE)
    }, error = function(e) NULL)
    
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    colnames(df) <- c("date", elements)
    df |> mutate(subid = subid, wban = wban, icao = icao)
  }
  
  # Initialize failure flags
  wban_failed <- FALSE
  icao_failed <- FALSE
  
  # Try WBAN first
  weather <- fetch_acis(wban)
  
  if (is.null(weather)) {
    wban_failed <- TRUE
    if (verbose) message("WBAN failed! Trying ICAO for obs_label = ", obs_label_value)
    
    # Try ICAO
    weather <- fetch_acis(icao)
    
    if (is.null(weather)) {
      icao_failed <- TRUE
      if (verbose) message("Both WBAN and ICAO failed for obs_label = ", obs_label_value)
      
      # Create an NA row with correct columns
      weather <- data.frame(
        date = query_date,
        matrix(NA, nrow = 1, ncol = length(elements)),
        subid = subid,
        wban = wban,
        icao = icao
      )
      colnames(weather)[2:(length(elements)+1)] <- elements
    }
  }
  
  if (tidyup == TRUE) {
    weather <- weather |>
      mutate(across(where(is.character), ~na_if(., "M")),
             snow = as.numeric(if_else(snow == "T", ".001", snow)),
             snwd = as.numeric(if_else(snwd == "T", ".001", snwd)),
             pcpn = as.numeric(if_else(pcpn == "T", ".001", pcpn)))
  }
  
  weather <- weather |> 
    mutate(
      date = as.Date(date),
      subid = as.character(subid),
      wban = as.character(wban),
      icao = as.character(icao),
      avgt = as.numeric(avgt),
      mint = as.numeric(mint),
      maxt = as.numeric(maxt)
    )
  
  return(weather)
}