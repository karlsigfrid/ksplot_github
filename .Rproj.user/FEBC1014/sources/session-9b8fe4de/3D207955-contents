#Save a plot object save with the recordPlot function

#' @export
save_svg <- function(p, filename, width=8, height=5){
  svg(filename=paste0(filename, ".svg"), width=width, height=height)
  if (typeof(p) == "closure"){
    p()
  }else{
    print(p)
  }
  dev.off() |> invisible()
}

#' @export
save_eps <- function(p, filename, width=8, height=5){
  setEPS()
  postscript(file=paste0(filename, ".eps"), width=width, height=height)
  if (typeof(p) == "closure"){
    p()
  }else{
    print(p)
  }
  dev.off() |> invisible()
}

# width,height in pixels
#' @export
save_png <- function(p, filename, width=600, height=400){
  png(filename=paste0(filename, ".png"), width=width, height=height)
  if (typeof(p) == "closure"){
    p()
  }else{
    print(p)
  }
  dev.off() |> invisible()
}
