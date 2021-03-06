% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scales.R
\name{scale_show_infinites_x}
\alias{scale_show_infinites_x}
\title{Include Infinite Values in Plot}
\usage{
scale_show_infinites_x(
  trans = "identity",
  limits = NULL,
  position = "bottom",
  breaks = ggplot2::waiver(),
  ...
)
}
\arguments{
\item{trans}{For continuous scales, the name of a transformation object
or the object itself. Built-in transformations include "asn", "atanh",
"boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2",
"logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal",
"reverse", "sqrt" and "time".

A transformation object bundles together a transform, its inverse,
and methods for generating breaks and labels. Transformation objects
are defined in the scales package, and are called \verb{<name>_trans} (e.g.,
\code{\link[scales:boxcox_trans]{scales::boxcox_trans()}}). You can create your own
transformation with \code{\link[scales:trans_new]{scales::trans_new()}}.}

\item{limits}{One of:
\itemize{
\item \code{NULL} to use the default scale range
\item A numeric vector of length two providing limits of the scale.
Use \code{NA} to refer to the existing minimum or maximum
\item A function that accepts the existing (automatic) limits and returns
new limits. Also accepts rlang \link[rlang:as_function]{lambda} function
notation.
Note that setting limits on positional scales will \strong{remove} data outside of the limits.
If the purpose is to zoom, use the limit argument in the coordinate system
(see \code{\link[ggplot2:coord_cartesian]{coord_cartesian()}}).
}}

\item{position}{For position scales, The position of the axis.
\code{left} or \code{right} for y axes, \code{top} or \code{bottom} for x axes.}

\item{breaks}{One of:
\itemize{
\item \code{NULL} for no breaks
\item \code{waiver()} for the default breaks computed by the
\link[scales:trans_new]{transformation object}
\item A numeric vector of positions
\item A function that takes the limits as input and returns breaks
as output (e.g., a function returned by \code{\link[scales:breaks_extended]{scales::extended_breaks()}}).
Also accepts rlang \link[rlang:as_function]{lambda} function notation.
}}

\item{...}{Other arguments to ggplot2::scale_x_continuous or ggplot2::scale_y_continuous}
}
\value{
ScaleContinuousPosition object that can be added to a ggplot object using '+'
}
\description{
Will replace infinite values by the nearest limit.
}
\details{
This function also allows you to pass arguments along to \code{\link[ggplot2:scale_continuous]{ggplot2::scale_x_continuous()}} since once you add this call you won't be able to edit the continuous scale elsewhere.
Oftentimes you'll want to use the \code{trans} argument to, for example, visualize your data on a log scale.
}
\examples{
\dontrun{
data = mtcars
    dplyr::mutate(qsec2 = ifelse(qsec > 19, Inf, qsec))

# Plot with infinites set to nearest limit
data \%>\%
    ggplot2::ggplot(ggplot2::aes(x=qsec2, y=carb)) +
    ggplot2::geom_point() +
    scale_show_infinites_x()

# Plot on a log10 scale with infinites set to nearest limit
data \%>\%
    ggplot2::ggplot(ggplot2::aes(x=qsec2, y=carb)) +
    ggplot2::geom_point() +
    scale_show_infinites_x(trans="log10")
 }
}
\seealso{
\code{\link[=scale_show_infinites_y]{scale_show_infinites_y()}}

\code{\link[ggplot2:scale_continuous]{ggplot2::scale_x_continuous()}}
}
