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
#' @return ggplot
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
  gg <- ggplot2::ggplot(data, aesthetic) +
    ggplot2::ylab(col_string) +
    ggplot2::geom_bar(ggplot2::aes(fill=!!fill_column), position=position, ...) +
    ggplot2::xlab("Count") +
    ggplot2::ggtitle(title) +
    theme_fivethirtyeight_two()

  return(gg)
}


## Unfinished
#' Density Plot
#'
#' @param data datafrmae
#' @param col column (don't quote)
#' @param summary_stats add text with summary stats to plot  (boolean)
#' @param text_xpos x position of summary stat text (numeric)
#' @param text_ypos y position of summary stat text (numeric)
#' @param text_sigfigs number of significant figures to write summary statistic to (numeric)
#' @param ... other geom_density aesthetics (e.g. linetype = "dashed")
#' @param fill geom_density fill color
#' @param alpha geom_density alpha color
#'
#' @return gglot
#' @export
#'
#' @examples
#' ggdensity(mtcars, mpg, summary = TRUE)
ggdensity <- function(data, col, fill = "steelblue", alpha = 0.5, summary_stats = FALSE, text_xpos = NULL, text_ypos = 0.04, text_sigfigs = Inf, ...){
  col <- rlang::enquo(col)
  col_string <- rlang::as_name(col)

  gg <- ggplot2::ggplot(data, ggplot2::aes(!!col)) +
    ggplot2::geom_density(fill = fill, alpha = alpha, ...)

  if(summary_stats){
    text_xpos=ifelse(
      is.null(text_xpos),
      yes = lerp(min(data[[col_string]]), max(data[[col_string]]), 0.92),
      no = text_xpos
    )
    gg <- gg + ggplot2::geom_text(
      y=text_ypos,
      x=text_xpos,
      ggplot2::aes(label=descriptive_stats_string(!!col, sigfigs = text_sigfigs))
      )
  }

  gg <- gg + theme_fivethirtyeight_two()
  return(gg)
}



# Miscellaneous functions -------------------------------------------------
descriptive_stats_df <- function(vector){
  res=summary(vector)
  data.frame(
    Property=sub(pattern = "\\.$", replacement = "", x = names(res)), Value=as.vector(res)
  )
}

descriptive_stats_string <- function(vector, sigfigs = Inf){
  summary <- descriptive_stats_df(vector)
  paste0(summary[[1]], ":  ",signif(summary[[2]], digits = sigfigs), collapse = "\n")
}

lerp <- function(num1, num2, fraction){
  if(fraction > 1) message("[ler[] WARNING: trying to get the number ", fraction*100, "% between ", num1, " and ", num2)
  (num2-num1)*fraction+num1
}
