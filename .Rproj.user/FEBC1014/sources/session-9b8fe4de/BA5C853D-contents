# Prep a plot

#' @export
prep_plot <- function(textsize=1, mai_bltr=c(0.66, 0.66, 0.22, 0.42),
                      mai_add = c(0,0,0,0)){
  mycolors <- c("steelblue", "orange", "forestgreen", "indianred", "mediumpurple3",
                "brown", "pink", "darkgray", "black")
  #if(!("lato" %in% font_families())) font_add_google("Lato", "lato")
  #showtext_auto()
  parlist <-list(lwd=2, bty="n",
                 cex=textsize * 1.05, cex.lab=textsize * 1.3,
                 ann=FALSE,
                 mai=mai_bltr + mai_add)  #bottom, left, top, right
  do.call(par, parlist)
  palette(mycolors)
}

#If you want to know the default margins for ksplot
#' @export
get_marg <- function() return(c(b=3.3, l=3.3, t=1.1, r=2.1))

#' @export
get_mai <- function() return(c(0.66, 0.66, 0.22, 0.42))
