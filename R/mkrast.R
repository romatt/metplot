#' Make raster from field
#'
#' @param field this is the field...
#' @param lonmin A float specifying min longitude
#' @param lonmax A float specifying max longitude
#' @param latmin A float specifying min latitude
#' @param latmax A float specifying max latitude
#' @param dellon A float specifying the delta longitude
#' @param dellat A float specifying the delta latitude
#' @param crsstring A string specifying the projection to be used
#' @param lons A vector of longitudes
#' @param lats A vector of latitudes
#'
#' @return
#' @export
#'
#' @examples
#' PVr <- mkrast(PV,lons=lons,lats=lats)

mkrast <- function(field,
                   lonmin=-180,
                   lonmax=180,
                   latmin=-90,
                   latmax=90,
                   dellon=1,
                   dellat=1,
                   crsstring="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                   lons=NA,
                   lats=NA) {

  # Check if longitudes and latitudes are provided
  # If so, base lonmin/max, latmin/max, dellon/lat on those arrays
  if(!is.na(lons) && !is.na(lats)) {
    lonmin=lons[1]
    lonmax=lons[length(lons)]
    latmin=lats[1]
    latmax=lats[length(lats)]
    dellon=lons[2]-lons[1]
    dellat=lats[2]-lats[1]
  }

  # Remove cyclic point if provided
  if(dim(field)[1]*dellon>360) {
    field<-field[1:(dim(field)[1]-1),,]
  }

  if(length(dim(field))==3) {

    # From https://stat.ethz.ch/pipermail/r-help/2009-May/391421.html
    ftrans <- array(NA,c(dim(field)[2],dim(field)[1],dim(field)[3]))

    for(z in 1:dim(field)[3]) {

      # Transpose field
      ftrans[,,z] <- t(field[,,z])

      # Reverse
      ftrans[,,z] <- apply(ftrans[,,z], 2, rev)
    }

    # Generate brick from multilayer data
    r <- raster::brick(ftrans, xmn=lonmin-(dellon/2), xmx=lonmax-(dellon/2), ymn=latmin-(dellat/2), ymx=latmax+(dellat/2), crs=raster::crs(crsstring))

  } else if(length(dim(field))==2) {

    # Transpose and reverse field
    ftrans <- apply(t(field), 2, rev)

    # Generate the raster
    r <- raster::raster(xmn=lonmin-(dellon/2), xmx=lonmax-(dellon/2), ymn=latmin-(dellat/2), ymx=latmax+(dellat/2), resolution=c(dellon,dellat),crs=raster::crs(crsstring))

    # Write field to raster
    r <- raster::setValues(r, ftrans)

  } else {
    cat("ERROR: INPUT FIELD HAS UNKNOWN NUMBER OF DIMENSIONS")
  }

  return(r)
}
