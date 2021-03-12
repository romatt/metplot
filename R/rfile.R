#' Read NetCDF File
#'
#' @param file character string specifying the NetCDF file location
#' @param field character string specifying the field to be read
#' @param ... arbitrary number of additional character strings specifying additional fields to be read
#'
#' @return
#' @export
#'
#' @examples
#' readfile("/net/server/folder/somefile.nc","SLP","OMEGA")

rfile <-function(file,field,...) {

  # Check if file exists
  if(!file.exists(file)) {

    warning(paste("ERROR: File ",file," not found.",sep=""))

  } else {

    out<-list()

    # Read NetCDF file
    ncin <- ncdf4::nc_open(file)
    out[[1]]<-ncdf4::ncvar_get(ncin,field)
    i=1
    for(addfield in list(...)) {
      out[[i+1]]<-ncdf4::ncvar_get(ncin,addfield)
      i=i+1
    }

    ncdf4::nc_close(ncin)

    return(out)

  } # File exists

}
