#Generate an empty plot sized to fit the input data
#Can be used before putting a grid in place, to draw the grid in the background
#x and y are lists of coordinates.
#If y is NULL, y values are specified in x, and x values will be index value

#' @export
plot_grid <- function(x, y=NULL, maintitle="", legend_space=NULL, width=8, height=5, nogrid = FALSE, ...){
  if (!is.null(legend_space)) make_legend_space(legend_text=legend_space, width=width)
  if (is.null(y)){
    if(class(x) != "list") x <- list(x)
    y <- lapply(x, function(q) 1:length(q))
    y_copy <- y
    y <- x
    x <- y_copy
  }
  x <- unlist(x)
  y <- unlist(y)
  plot(x=x, y=y, bty='n', ylab='',xlab='', col=par()$bg, main=maintitle, ...)
  if(!nogrid) grid(lty=1, lwd=1)
}

#' @export
make_legend_space <- function(legend_text, width){
  par(mai=c(0,0,0,0))
  plot_empty()
  leg <- legend("topleft", legend=legend_text, plot=FALSE, bty="n")
  width_prop <- leg$rect$w
  width_inch <- width_prop * width
  par(mai=get_mai() - c(0,0,0,get_mai()[4])/2 +  c(0,0,0,width_inch))
  par(new = TRUE)
  return(width_inch)
}

#' @export
get_inset <- function(width=8){
  #Right margin divided by plotting space (exclusing margins)
  inset_x <- - (par()$mai[4] - get_mai()[4] / 4) / (width - sum(par()$mai[c(2, 4)]))
  return(c(inset_x, 0))
}
