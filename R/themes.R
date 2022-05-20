#' Custom Themes
#'
#' ggplot theme that fixes a bunch of issues I tend to have with plots.
#'
#' By default, will bold center axis and plot titles and tweak distane of axis_titles to plot
#'
#' @param dist_from_plot_xlab distance between x axis title and plot (number)
#' @param dist_from_plot_ylab distance between y axis title and plot (number)
#' @param dist_from_plot_ggtitle distance between y axis title and plot (number)
#' @param subtitle_face Font face ("plain", "italic", "bold", "bold.italic")
#' @param no_background set all backgrounds to clear? (flag)
#'
#' @return ggtheme
#' @export
#' @examples
#' mtcars %>%
#'   ggplot2::ggplot(ggplot2::aes(cyl>6, mpg)) +
#'   ggplot2::geom_point() +
#'   theme_common_adjustments()
theme_common_adjustments <- function(dist_from_plot_xlab = 10, dist_from_plot_ylab=10, dist_from_plot_ggtitle = 10, no_background=FALSE, subtitle_face="plain"){
  custom_theme <- ggplot2::theme(
    plot.title = ggplot2::element_text(hjust=0.5, size = 18, face = "bold", margin = ggplot2::margin(b = dist_from_plot_ggtitle)),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, face = subtitle_face),
    axis.title = ggplot2::element_text(face="bold"),
    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t=dist_from_plot_xlab)),
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r=dist_from_plot_ylab))
  )


  if(no_background){
    custom_theme <- custom_theme + ggplot2::theme(panel.background = ggplot2::element_blank(), plot.background = ggplot2::element_blank())
  }

  return(custom_theme)
}




#' Add crossbar to ggplot
#'
#' Adds a crossbar to a ggplot.
#' Best used when comparing one categorical and one numeric variable using geom_point / geom_jitter(height=0).
#'
#' @param summaryfunction a function run on the y aesthetic to determine where line is drawn.
#' Options include median, mean, max, min, or any other function that summarises a numeric vector into a single number (function)
#' @param width width of crossbar
#' @param size,colour ggplot aesthetics
#'
#' @return ggplot geom
#' @export
#'
#' @examples
#' mtcars %>%
#'   ggplot2::ggplot(ggplot2::aes(cyl>6, mpg)) +
#'   ggplot2::geom_point() +
#'   geom_crossbar_predefined()
geom_crossbar_predefined <- function(summaryfunction=stats::median, width=0.4, size=0.3, colour){
  assertthat::assert_that(is.function(summaryfunction), msg = "summaryfunction must be a function (e.g. mean)")
  ggplot2::stat_summary(fun = summaryfunction, fun.min = summaryfunction, fun.max = summaryfunction,
                        geom = "crossbar", width = width, size=size)
}



#' Custom Themes
#'
#' @return ggtheme
#' @export
#'
theme_axis_titles_cleveland <- function(){
  ggpubr::theme_cleveland() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust=0.5, size = 18, face = "bold"),
                   axis.title = ggplot2::element_text(face="bold"),
                   axis.title.y = ggplot2::element_text())
}




#' Custom Themes
#'
#' @return ggtheme
#' @export
#'
theme_legend_none <- function(){
  ggplot2::theme(legend.position = "none")
}

#' Custom Themes
#'
#' @param direction "Vertical or Horizontal"
#'
#' @return ggtheme
#' @export
theme_legend_right <- function(direction = "vertical"){
  ggplot2::theme(legend.position = "right", legend.direction = direction)
}

#' Custom Themes
#'
#' @param direction "Vertical or Horizontal"
#'
#' @return ggtheme
#' @export
theme_legend_left <- function(direction = "vertical"){
  ggplot2::theme(legend.position = "left", legend.direction = direction)
}

#' Custom Themes
#'
#' @param direction "Vertical or Horizontal"
#'
#' @return ggtheme
#' @export
theme_legend_top <- function(direction = "horizontal"){
  ggplot2::theme(legend.position = "top", legend.direction = direction)
}


#' Custom Themes
#'
#' @param direction "Vertical or Horizontal"
#'
#' @return ggtheme
#' @export
theme_legend_bottom <- function(direction = "horizontal"){
  ggplot2::theme(legend.position = "bottom", legend.direction = direction)
}

#' Custom Themes
#'
#' @return ggtheme
#' @export
theme_legend_title_none <- function(){
  ggplot2::theme(legend.title = ggplot2::element_blank())
}


#' Custom Themes
#'
#' @return ggtheme
#' @export
#'
theme_fivethirtyeight_two <- function(){
  ggthemes::theme_fivethirtyeight() +
    theme_common_adjustments(no_background = TRUE)
}

#' Miminal theme with border
#'
#' A minimal theme with a border. Works well for faceted graphs
#'
#' @param border_color Colour of border
#' @param border_thickness border thickness in mm
#' @inheritDotParams ggplot2::theme_minimal
#'
#'
#' @export
#'
#' @examples
#' mtcars %>%
#'   ggplot2::ggplot(ggplot2::aes(x = mpg, y=disp)) +
#'   ggplot2::geom_point() +
#'   theme_minimal_bordered()
theme_minimal_bordered <- function(border_color = "grey40", border_thickness = NULL, ...){
  ggplot2::theme_minimal(...) +
  ggplot2::theme(panel.border=ggplot2::element_rect(fill=NA, colour=border_color, size = border_thickness))+
  theme_common_adjustments()
}


# Superseded functions ----------------------------------------------------

#' Remove theme legend
#'
#' `r lifecycle::badge("deprecated")`
#'
#'
#' @param ... no arguments are used. Included only so code written for older versions of package doesn't break
#'
#' @return theme
#' @export
#'
theme_no_legend <- function(...){
  .Deprecated("theme_legend_none")
  theme_legend_none()
}

#' Remove theme legend
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param ... no arguments are used. Included only so code written for older versions of package doesn't break
#'
#' @return theme
#' @export
#'
theme_no_legend_title <- function(...){
  .Deprecated("theme_legend_title_remove")
  theme_legend_none()
}


