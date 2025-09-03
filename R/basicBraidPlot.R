#' Wrapper for Basic Braid Plot
#'
#' @param data Dataset to use for plot. If not already a data.frame, will be
#' converted to one by `fortify()`
#' @param mapping List of aesthetic mappings to use for plot. `geom_braid`
#' requires at least the following aesthetics: `x`, `y`, and `fill`.
#' @param palette A Brewer color palette passed to the `scale_fill_distiller()`
#' function
#' @param direction The direction of the Brewer color palette, also passed to
#' `scale_fill_distiller()`
#' @param logscale Should x and y variables be plotted on a log scale (defaults
#' to `TRUE`). If `FALSE`, `scale_x_continuous()` and and `scale_y_continuous()`
#' will still be applied to ensure that a plot produced by this function always
#' has explicitly given scales.
#'
#' @details
#' Though the `geom_braid()` function allows for a great deal of flexibility
#' and keeps with the layer-based ethos of `ggplot`, we find that very often, it
#' is useful to just get a simple first pass view of a response surface, and
#' repeatedly typing out all the necessary layers to make the surface visible
#' can be extremely tiresome.  This function wraps a simple set of `ggplot`
#' functions with the `geom_braid()` function to produce a standard heatmap
#' plot that works for many of the surfaces we encounter.
#'
#' Note however that this is just meant to be a convenience function.  Any more
#' complex or sophisticated plots should be built using `ggplot` layers,
#' including `geom_braid()`.
#'
#' @return An object of class `ggplot` containing a `geom_braid()` rendering of
#' the respective x, y, and fill aesthetics.
#' @export
#'
#' @examples
#' surface <- synergisticExample
#' plot <- basicBraidPlot(surface,aes(x=concA,y=concB,fill=measure))
#' print(plot)
basicBraidPlot <- function(data, mapping, palette="RdYlBu", direction=-1, logscale=TRUE) {
	p <- ggplot(data,mapping) + geom_braid()
	if (logscale) {
		p <- p + scale_x_log10() + scale_y_log10()
	} else {
		p <- p + scale_x_continuous() + scale_y_continuous()
	}
	p <- p + scale_fill_distiller(palette=palette, direction=direction)

	p
}
