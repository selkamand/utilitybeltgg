
#' Include Infinite Values in Plot
#'
#' Will replace infinite values by the nearest limit.
#'
#' This function also allows you to pass arguments along to [ggplot2::scale_x_continuous()] since once you add this call you won't be able to edit the continuous scale elsewhere.
#' Oftentimes you'll want to use the \code{trans} argument to, for example, visualize your data on a log scale.
#'
#'
#'
#'
#' @param ... Other arguments to ggplot2::scale_x_continuous or ggplot2::scale_y_continuous
#'
#' @return ScaleContinuousPosition object that can be added to a ggplot object using '+'
#' @export
#' @inheritParams ggplot2::scale_x_continuous
#' @seealso
#' [scale_show_infinites_y()]
#'
#' [ggplot2::scale_x_continuous()]
#'
#' @examples
#' \dontrun{
#' data = mtcars
#'     dplyr::mutate(qsec2 = ifelse(qsec > 19, Inf, qsec))
#'
#' # Plot with infinites set to nearest limit
#' data %>%
#'     ggplot2::ggplot(ggplot2::aes(x=qsec2, y=carb)) +
#'     ggplot2::geom_point() +
#'     scale_show_infinites_x()
#'
#' # Plot on a log10 scale with infinites set to nearest limit
#' data %>%
#'     ggplot2::ggplot(ggplot2::aes(x=qsec2, y=carb)) +
#'     ggplot2::geom_point() +
#'     scale_show_infinites_x(trans="log10")
#'  }
scale_show_infinites_x <- function(trans="identity", limits = NULL, position = "bottom", breaks = ggplot2::waiver(),...){
  ggplot2::scale_x_continuous(oob = scales::oob_squish_infinite, trans, limits, position, breaks, ...)
}

#' Include Infinite Values in Plot
#'
#' @export
#' @inherit scale_show_infinites_x description return
#' @inheritDotParams scale_show_infinites_x
#' @seealso [scale_show_infinites_x()]
#'
#' @examples
#' \dontrun{
#' data = mtcars %>%
#'     dplyr::mutate(qsec2 = ifelse(qsec > 19, Inf, qsec))
#'
#' # Plot with infinites set to nearest limit
#' data %>%
#'     ggplot2::ggplot(ggplot2::aes(x=carb, y=qsec2)) +
#'     ggplot2::geom_point() +
#'     scale_show_infinites_y()
#'
#' # Plot on a log10 scale with infinites set to nearest limit
#' data %>%
#'     ggplot2::ggplot(ggplot2::aes(x=carb, y=qsec2)) +
#'     ggplot2::geom_point() +
#'     scale_show_infinites_y(trans="log10")
#' }
scale_show_infinites_y <- function(trans="identity", limits = NULL, position = "bottom", breaks = ggplot2::waiver(),...){
  ggplot2::scale_y_continuous(oob = scales::oob_squish_infinite, trans, limits, position, breaks, ...)
}
