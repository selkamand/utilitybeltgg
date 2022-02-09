
#' Include Infinite Values in Plot
#'
#' Will replace infinite values by the nearest limit
#'
#' @return ScaleContinuousPosition object that can be added to a ggplot object using '+'
#' @export
#'
#' @seealso [scale_show_infinites_y()]
#'
#' @examples
#' mtcars %>%
#'     dplyr::mutate(qsec2 = ifelse(qsec > 19, Inf, qsec)) %>%
#'     ggplot2::ggplot(ggplot2::aes(x=qsec2, y=carb)) +
#'     ggplot2::geom_point() +
#'     scale_show_infinites_x()
scale_show_infinites_x <- function(){
  ggplot2::scale_x_continuous(oob = scales::oob_squish_infinite)
}

#' Include Infinite Values in Plot
#'
#' @export
#' @inherit scale_show_infinites_x description return
#'
#' @seealso [scale_show_infinites_x()]
#'
#' @examples
#' mtcars %>%
#'     dplyr::mutate(qsec2 = ifelse(qsec > 19, Inf, qsec)) %>%
#'     ggplot2::ggplot(ggplot2::aes(x=qsec2, y=carb)) +
#'     ggplot2::geom_point() +
#'     scale_show_infinites_y()
scale_show_infinites_y <- function(){
  ggplot2::scale_y_continuous(oob = scales::oob_squish_infinite)
}
