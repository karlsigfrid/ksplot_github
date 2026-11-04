# Prep a plot

prep_plot <- function(textsize=1, mar_bltr=c(3.3, 3.3, 1.1, 2.1)){
  mycolors <- c("steelblue", "orange", "forestgreen", "indianred", "mediumpurple3", "brown", "pink", "darkgray")
  #if(!("lato" %in% font_families())) font_add_google("Lato", "lato")
  #showtext_auto()
  parlist <-list(lwd=2, bty="n",
                 cex=textsize * 1.05, cex.lab=textsize * 1.3,
                 ann=FALSE,
                 mar=mar_bltr)  #bottom, left, top, right
  do.call(par, parlist)
  palette(mycolors)
}
