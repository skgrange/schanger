#' Function to detect intercept changes in a time series. 
#' 
#' @param df Input data frame containing a time series. 
#' 
#' @param n_pieces Number of different pieces in the time series to detect. 
#' 
#' @param chains Number of chains to run. 
#' 
#' @param n_cores Number of cores to be used for model calculation. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return \code{mcpfit} model object with an extra \code{"mcmc_tidy"} element. 
#' 
#' @seealso \code{\link{predict_intercept_changes}}
#' 
#' @export
find_intercept_changes <- function(df, n_pieces = 2, chains = 3, 
                                   n_cores = 1, verbose = FALSE) {
  
  # Message to user
  if (verbose) {
    
    # Get site if exists
    if("site" %in% names(df)) {
      message <- stringr::str_c("`", df$site[1], "`...")
    } else {
      message <- "Modelling..."
    }
    
    # Message
    message(threadr::date_message(), message)
    
  }
  
  # Check dates
  if (!"date" %in% names(df)) {
    stop("`date` must be present in data frame.", call. = FALSE)
  }
  
  # Check date resolution
  if (threadr::detect_date_interval(df$date) != 86400) {
    stop("Time series is not at daily resolution.", call. = FALSE)
  } 
  
  # Check value
  stopifnot("value" %in% names(df) && is.numeric(df$value))
  
  # Check n
  stopifnot(n_pieces >= 2L)
  
  # Build formula
  if (n_pieces == 2) {
    formula_model <- list(value ~ 1, ~ 1)
  } else if (n_pieces == 3) {
    formula_model <- list(value ~ 1, ~ 1, ~ 1)
  } else if (n_pieces == 4) {
    formula_model <- list(value ~ 1, ~ 1, ~ 1, ~ 1)
  } else if (n_pieces == 5) {
    formula_model <- list(value ~ 1, ~ 1, ~ 1, ~ 1, ~ 1)
  } else if (n_pieces == 6) {
    formula_model <- list(value ~ 1, ~ 1, ~ 1, ~ 1, ~ 1, ~ 1)
  } else {
    stop("`n_pieces` is not supported.", call. = FALSE)
  }
  
  # Check input for missing-ness and interpolate if needed
  if (anyNA(df$value)) {
    df <- mutate(df, value = threadr::na_interpolate(value))
    values_interpolated <- TRUE
    # warning("Missing values have been interpolated...", call. = FALSE)
  } else {
    values_interpolated <- FALSE
  }
  
  # Model
  list_model <- tryCatch({
    suppressMessages(
      df %>% 
        mutate(date = as.numeric(date)) %>% 
        mcp::mcp(
          formula_model,
          data = ., 
          par_x = "date", 
          chains = chains,
          cores = n_cores,
          sample = "post",
          iter = 3000,
          adapt = 1500
        )
    )
  }, error = function(e) {
    warning("Modeling failed...", call. = FALSE, immediate. = TRUE)
    return(NULL)
  })
  
  # Return here if modelling fails
  if (is.null(list_model)) return(list())
  
  # Extract data for easy use
  df_mc <- list_model$mcmc_post %>% 
    purrr::map_dfr(as_tibble, .id = "chain") %>% 
    mutate(chain = as.integer(chain)) %>% 
    group_by(chain) %>% 
    mutate(id = 1:n()) %>% 
    ungroup() %>% 
    mutate(values_interpolated = !!values_interpolated,
           across(dplyr::starts_with("cp_"), threadr::parse_unix_time)) %>% 
    relocate(id)

  # Append data to list
  list_model <- c(list_model, mcmc_tidy = list(df_mc))
  
  # Force list class for generic functions
  class(list_model) <- "mcpfit"
  
  return(list_model)
  
}


#' Function predict intercept changes from a model created by 
#' \code{\link{find_intercept_changes}}
#' 
#' @param model \code{mcpfit} model object from 
#' \code{\link{find_intercept_changes}}.
#' 
#' @param n Number of predictions to make. 
#' 
#' @param summarise Should the samples be summarised (the median)? 
#' 
#' @param tz Time zone to parse dates to.  
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
predict_intercept_changes <- function(model, n = 20, summarise = FALSE, 
                                      tz = "UTC") {
  
  # Get max and min from data, in Unix time
  date_start <- min(model$data$date)
  date_end <- max(model$data$date)
  
  # Create date sequence
  date_sequence <- seq(date_start, date_end, by = threadr::seconds_in_a_day())
  
  # Select a few draws
  df <- model$mcmc_post %>%
    tidybayes::tidy_draws() %>% 
    select(-.chain, 
           -.iteration)
  
  # Summarise or sample
  if (summarise) {
    df <- dplyr::summarise(df, dplyr::across(everything(), median, na.rm = TRUE))
  } else {
    df <- dplyr::sample_n(df, !!n)
  }
  
  # Expand and predict, use simulate function in the model
  df <- df %>% 
    tidyr::expand_grid(date = date_sequence) %>%
    mutate(
      value_predict = rlang::exec(model$simulate, !!!., type = "fitted"),
      across(c("date", dplyr::starts_with("cp_")), threadr::parse_unix_time, tz = tz)
    ) %>% 
    rename(draw = .draw) %>%
    dplyr::rename_with(~stringr::str_replace(., "int_", "intercept_")) %>% 
    dplyr::rename_with(~stringr::str_replace(., "cp_", "change_point_")) %>% 
    relocate(draw,
             date,
             value_predict)
  
  return(df)
  
}


#' Function to extract change point dates from 
#' \code{\link{predict_intercept_changes}}'s output.
#' 
#' @param df Data frame from \code{\link{predict_intercept_changes}}.
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
extract_change_point_dates <- function(df) {
  
  df %>% 
    select(draw,
           dplyr::matches("change_point")) %>% 
    tidyr::pivot_longer(-draw, names_to = "change_point", values_to = "date") %>% 
    mutate(change_point = stringr::str_remove(change_point, "change_point_"),
           change_point = as.integer(change_point)) %>% 
    distinct(draw,
             change_point,
             date)
  
}


#' Function to extract intercept change values from 
#' \code{\link{predict_intercept_changes}}'s output. 
#' 
#' @param df Data frame from \code{\link{predict_intercept_changes}}.
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
extract_intercept_change_values <- function(df) {
  
  df %>% 
    select(draw,
           dplyr::matches("intercept")) %>% 
    tidyr::pivot_longer(-draw, names_to = "intercept") %>% 
    mutate(intercept = stringr::str_remove(intercept, "intercept_"),
           intercept = as.integer(intercept)) %>% 
    distinct(draw,
             intercept,
             value)
  
}
