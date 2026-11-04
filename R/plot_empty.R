#Generate an empty plot
#Can be used to put a legend in an empty space

#' @export
plot_empty <- function(){
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
}
