#' Function to extract spatial polygon for subset of map
#'
#' @description This Function was adapted from https://edzer.github.io/sp/
#'
#' @param xlim
#' @param ylim
#' @param l.out
#' @param clip
#' @param crs
#'
#' @return
#' @export
#'
#' @examples

maps2sp = function(xlim,
                   ylim,
                   l.out = 100,
                   clip = TRUE,
                   crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {

  m = maps::map(xlim = xlim, ylim = ylim, plot = FALSE, fill = TRUE)
  p = rbind(cbind(xlim[1], seq(ylim[1],ylim[2],length.out = l.out)),
            cbind(seq(xlim[1],xlim[2],length.out = l.out),ylim[2]),
            cbind(xlim[2],seq(ylim[2],ylim[1],length.out = l.out)),
            cbind(seq(xlim[2],xlim[1],length.out = l.out),ylim[1]))
  LL = sp::CRS(crs)
  bb = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(list(p))),"bb")), proj4string = LL)
  IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
  m <- maptools::map2SpatialPolygons(m, IDs=IDs, proj4string = LL)
  if (!clip)
    m
  else {
    rgeos::gIntersection(m, bb) # cut map slice in WGS84
  }
}
