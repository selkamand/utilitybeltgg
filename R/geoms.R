#' geom barplot counts
#'
#' Add text labels indicating counts above barplot columns.
#' Works on plots that have a geom_bar() layer.
#' Works even if you flip axis with coord_flip().
#'
#' @param distance_from_bar distance between text and cbar
#' @param size size of text (number)
#' @param alpha transparancy (number)
#' @param color colour (string)
#' @param family font family (string)
#' @param fontface Font face ("plain", "italic", "bold", "bold.italic") (string)
#'
#' @return ggplot geom
#' @export
#'
#' @examples
#' mtcars %>%
#' ggplot2::ggplot(ggplot2::aes(x=as.character(cyl))) +
#' ggplot2::geom_bar() +
#' ggplot2::xlab("cylinders") +
#' geom_barplot_counts()
geom_barplot_counts <- function(distance_from_bar=1.5, orientation = "h",size = 4, fontface="bold", alpha = 0.8, color = "black", family="Helvetica"){

  assertthat::assert_that(tolower(orientation) %in% c("h", "horizontal", "v", "vertical"), msg = "orientation paramater must be set to 'h', 'horizontal', 'v', or 'vertical'")
  ..count..=NULL
  #names(plot$mapping)==x or ==y depending on barplot orientation. could automate but only way i know how to do it right now would lead to %>% dependency not +.
  if(tolower(orientation)[1] %in% c("v", "vertical")){
   nudge_y= distance_from_bar
   nudge_x = 0
  }
  else{
    nudge_x = distance_from_bar
    nudge_y = 0
  }

  ggplot2::geom_text(
    stat='count', ggplot2::aes(label=..count..),
    nudge_y = nudge_y,
    nudge_x=nudge_x,
    size = size,
    fontface=fontface,
    alpha = alpha,
    color=color,
    family=family
  )
}
