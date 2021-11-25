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
#' @param orientation orientation of barplot ("h" / "horizontal" / "v" / "vertical")
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


# Entire Plots ------------------------------------------------------------

#' Plot
#'
#' @param data a dataset to plot (dataframe)
#' @param col column of dataframe to plot (don't quote)
#' @param orientation orientation of barplot ("h" / "horizontal" / "v" / "vertical")
#' @param fill column of dataframe to color based on (don't quote)
#' @param position see ?ggplot2::geom_bar for details (Usually one of: "stack", "dodge", "fill")
#' @param ...  set any other barplot property. See ?ggplot2::geom_bar for details. (e.g. alpha = 0.4)
#' @param title title of graph (string)
#'
#' @return
#' @export
#'
#' @examples
#' ggbarplot(iris, Species, fill = Species, orientation = "horizontal", title = "My Freq Graph")
ggbarplot <- function(data, col, orientation = "h", fill=NULL, title = NULL, position = "stack", ...){
  col <- rlang::enquo(col)
  col_string <- rlang::as_name(col)

  fill_column <- rlang::enquo(fill)

  if(orientation %in% c("h", "horizontal")) { aesthetic <- ggplot2::aes(y = forcats::fct_rev(forcats::fct_infreq(!!col)))}
  else if (orientation %in% c("v", "vertical")) {aesthetic <- ggplot2::aes(x = forcats::fct_infreq(!!col))}
  else
    stop("orientation must be one of 'h', 'horizontal', 'v' or 'vertical'")

  assertthat::assert_that(col_string %in% names(data), msg = paste0("[", col_string ,"] is not a name of the supplied dataframe\n\n[col] must be one of the following:\n", paste0(collapse = ", ", names(data))))
  ggplot2::ggplot(data, aesthetic) +
    ggplot2::ylab(col_string) +
    ggplot2::geom_bar(ggplot2::aes(fill=!!fill_column), position=position, ...) +
    ggplot2::xlab("Count") +
    ggplot2::ggtitle(title) +
    theme_fivethirtyeight_two()
}

#c("#FAAB18", "#1380A1","#990000", "#588300")

