#' Read NetCDF File
#'
#' @param file A character string specifying the NetCDF file location
#' @param field A character string specifying the field to be read
#' @param ... An arbitrary number of character strings specifying additional fields to be read
#' @param navals A float specifying NA values
#' @param lonmin A float specifying longitude to start reading (optional)
#' @param lonmax A float specifying longitude to stop reading (optional)
#' @param latmin A float specifying latitude to start reading (optional)
#' @param latmax A float specifying latitude to stop reading (optional)
#' @param zstart An integer specifying first level to read (default: 1)
#' @param zcount An integer specifying number of levels to read (default: -1)
#' @param tstart An integer specifying first time-steps to read (default: 1)
#' @param tcount An integer specifying number of time-steps to read (default: -1)
#'
#' @return
#' @export
#'
#' @examples
#' library(zeallot) # Need this to unpack data from rfile
#'
#' # Read an entire field
#' c(lons, lats, APVTURBT)%<-%rfile("/net/thermo/atmosdyn2/atroman/stingjet/20140211_12/tra/APV12_20140212_07.nc","APVTURBT")
#'
#' # Only read a specific subset of field
#' c(lons,lats,PV)%<-%rfile("/net/thermo/atmosdyn/atroman/phd/MAR18/cdf/S20180302_21","PV",lonmin=-60,lonmax=-20,latmin=46,latmax=86,zstart=2,zcount=2)

rfile <-function(file,field,...,navals=NA,lonmin=NA,lonmax=NA,latmin=NA,latmax=NA,zstart=1,zcount=-1,tstart=1,tcount=-1) {

  # Check if file exists
  if(!file.exists(file)) {

    warning(paste("ERROR: File ",file," not found.",sep=""))

  } else {

    # Initialize output list
    out<-list()

    # Open NetCDF file
    ncin <- ncdf4::nc_open(file)

    # Read dimensions
    longitudes <- ncin$dim[[1]]$vals
    latitudes <- ncin$dim[[2]]$vals

    # Check if subset should be read
    if(!is.na(lonmin) & !is.na(lonmax) & !is.na(latmin) & !is.na(latmax)) {

      # Define lon/lat start and lon/lat count
      lonstart<-which(longitudes%==%lonmin)
      lonend<-which(longitudes%==%lonmax)
      latstart<-which(latitudes%==%latmin)
      latend<-which(latitudes%==%latmax)

      # Error handling
      if(length(lonstart)==0) warning(paste("ERROR: Latitude ",lonmin," not a subset of longitudes!",sep=""))
      if(length(lonend)==0) warning(paste("ERROR: Latitude ",lonmax," not a subset of longitudes!",sep=""))
      if(length(latstart)==0) warning(paste("ERROR: Latitude ",latmin," not a subset of latitudes!",sep=""))
      if(length(latend)==0) warning(paste("ERROR: Latitude ",latmax," not a subset of latitudes!",sep=""))

      # Calculate number of requested grid-points
      loncount<-lonend-lonstart
      latcount<-latend-latstart

      # Redefine longitude and latitude vectors
      longitudes<-longitudes[lonstart:lonend]
      latitudes<-latitudes[latstart:latend]
    } else {
      # Default definitions
      lonstart=1;loncount=-1;latstart=1;latcount=-1
    }

    # Save longitudes and latitudes to output list
    out[[1]]<-longitudes
    out[[2]]<-latitudes

    # Read subset or complete field
    tmp<-ncdf4::ncvar_get(ncin,field,start=c(lonstart,latstart,zstart,tstart),count=c(loncount,latcount,zcount,tcount))

    if(!is.na(navals)) tmp[tmp%==%navals]<-NA # Set NA values if required
    out[[3]]<-tmp # Add field to list

    i=4
    for(addfield in list(...)) {
      tmp<-ncdf4::ncvar_get(ncin,addfield,start=c(lonstart,latstart,zstart,tstart),count=c(loncount,latcount,zcount,tcount)) # Read additional field
      if(!is.na(navals)) tmp[tmp%==%navals]<-NA # Set NA values if required
      out[[i]]<-tmp # Add field to list
      i=i+1 # Increment additional field counter
    }

    ncdf4::nc_close(ncin)

    return(out)

  } # File exists

}
