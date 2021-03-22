#' Read NetCDF File
#'
#' @param file character string specifying the NetCDF file location
#' @param field character string specifying the field to be read
#' @param navals float specifying NA values
#' @param ... arbitrary number of character strings specifying additional fields to be read
#'
#' @return
#' @export
#'
#' @examples
#' library(zeallot) # Need this to unpack data from rfile
#' c(APVTURBT)%<-%rfile("/net/thermo/atmosdyn2/atroman/stingjet/20140211_12/tra/APV12_20140212_07.nc","APVTURBT")

rfile <-function(file,field,...,navals=-999) {

  # Check if file exists
  if(!file.exists(file)) {

    warning(paste("ERROR: File ",file," not found.",sep=""))

  } else {

    out<-list()

    ncin <- ncdf4::nc_open(file) # Open NetCDF file
    tmp<-ncdf4::ncvar_get(ncin,field) # Read field
    tmp[tmp%==%navals]<-NA # Set NA values
    out[[1]]<-tmp # Add field to list

    i=1
    for(addfield in list(...)) {
      tmp<-ncdf4::ncvar_get(ncin,addfield) # Read additional field
      tmp[tmp%==%navals]<-NA # Set NA values
      out[[i+1]]<-tmp # Add field to list
      i=i+1
    }

    ncdf4::nc_close(ncin)

    return(out)

  } # File exists

}
