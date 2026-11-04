#Generate an empty plot sized to fit the input data
#Can be used before putting a grid in place, to draw the grid in the background
#x and y are lists of coordinates.
#If y is NULL, y values are specified in x, and x values will be index value

#' @export
legend_outside <- function(position="topright", ...){
  kslegend <- legend(position, plot=FALSE, ...)
  box_w_inches <- (8 - sum(par()$mai[c(2, 4)])) *
    kslegend$rect$w / (kslegend$rect$left + kslegend$rect$w)
  myinset <- c(- box_w_inches / 8, 0)
  par(mai = get_mai() + c(0,0,0, box_w_inches - get_mai()[4]))
  legend(position, inset=myinset, xpd=TRUE, bty="n", ...)
}
