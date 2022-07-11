#' Formatters
#'
#' @param number number to turn into a percentage string, e.g. 10 to 10.0% (numeric)
#' @param decimal_places number of decimal places to give percentage to (numeric)
#'
#' @return percentage formatted string (character)
#' @export
#'
#' @examples
#' format_percent(5, decimal_places = 2) # returns 5%
format_percent <- function(number, decimal_places = 1){
  assertthat::assert_that(is.numeric(number), msg = utilitybeltfmt::fmterror("format_percent: expected number argument to be numeric, not [", class(number),"]"))
  paste0(format(round(number, decimal_places), nsmall = decimal_places), "%")
  #formatter = ("%0"f%%")
  #sprintf(fmt = formatter, proportion)
  #paste0(round(proportion, digits = decimal_places), "%")
}

#' Formatters
#'
#' @param number number to convert into scientific notation
#'
#' @return scientific notation form(character)
#' @export
#'
#' @examples
#' format_scientific(0.1)
format_scientific <- function(number){
  assertthat::assert_that(is.numeric(number), msg = utilitybeltfmt::fmterror("format_scientific: expected number argument to be numeric, not [", class(number),"]"))
  format(number, scientific = TRUE)
}
