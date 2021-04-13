#' Length of SpatialLinesDataFrames Elements
#'
#' @param spldf A SpatialLinesDataFrame with an arbitrary number of Lines
#'
#' @return
#' @export
#'
#' @examples
#' spldf(x)

spldflen <- function(spldf) {

  # Get type of object passed
  if(spldf@class[1]=="SpatialPolygonsDataFrame") {
    sapply(spldf@polygons, function(x) length(x@Polygons[[1]]@coords[,1]))
  } else if(spldf@class[1]=="SpatialLinesDataFrame") {
    sapply(spldf@lines[[1]]@Lines, function(x) length(x@coords[,1]))
  }

}

