#' metplot
#'
#' @param field field to be plotted
#' @param lons vector of longitudes
#' @param lats vector of latitudes
#' @param pbmin minimum plotting level
#' @param pbmax maximum plotting level
#' @param pbby delta plotting level
#' @param zeroinmid should center be white?
#' @param guide.title guide title
#'
#' @return plot
#' @export
#'
#' @examples
#' metplot(APVTOT,lons=lons,lats=lats,pbmin=-40,pbmax=40,pbby=5,zeroinmid=F,guide.title="APV (PVU)")

metplot<-function(field,
                  lons,
                  lats,
                  pbmin,
                  pbmax,
                  pbby,
                  zeroinmid,
                  panel.title,
                  guide.title,
                  save.dir=NA,
                  ...) {

  # Define coordinates
  lonmin=lons[1]
  lonmax=lons[length(lons)]
  latmin=lats[1]
  latmax=lats[length(lats)]

  # Extract landmask
  landcont = maps2sp(c(lonmin,lonmax), c(latmin,latmax))

  # Generate border of plot
  plotborder = sp::SpatialLines(list(sp::Lines(list(sp::Line(cbind(c(lonmin,lonmin,lonmax,lonmax,lonmin), c(latmin,latmax,latmax,latmin,latmin)))),ID="border")),proj4string=sp::CRS("+init=epsg:4326"))

  # Set up plotting colors
  colors = rev(RColorBrewer::brewer.pal(n = 11, name = "RdBu"))
  cmap = colorRampPalette(colors)

  # Set up plotting breaks
  if(zeroinmid) {
    # Need to take care of levels if zero in mid is requested
    plotbreaks = c(seq(pbmin,0-pbby,1),(0-pbby)/2,(0+pbby)/2,seq(0+pbby,pbmax,1))
    plotcolors = cmap(length(plotbreaks)-1)
    plotcolors[ceiling(length(plotcolors)/2)] = "#FFFFFF" # Set middle to white
  } else {
    # Else, simply use required min, max, diff
    plotbreaks = seq(pbmin,pbmax,pbby)
    plotcolors = cmap(length(plotbreaks)-1)
  }

  # Interpolate data
  #y <- raster::disaggregate(PVr, 6, method='bilinear')

  # Save plot to pdf if required
  if(!is.na(save.dir)) pdf(file = save.dir, width = 8,height=6)

  # Generate plot
  par(mar=c(0,0,0,0),mai=c(1,1,0,1),fg = "black")
  crs.longlat <- sp::CRS("+init=epsg:4326")
  sp::plot(as(landcont, "Spatial"), expandBB = c(.1, 0.05, .1, .1)) # Initialize plot
  #raster::filledContour(PVr,levels=plotbreaks,add=T,col=cmap(length(plotbreaks)))
  #raster::contour(PVr,levels=plotbreaks,drawlabels=F,add=T)
  #raster::plot(y, add=T, zlim=c(plotbreaks[1],plotbreaks[length(plotbreaks)]),breaks=plotbreaks,col=plotcolors,legend=FALSE,axes=T,colNA=plotcolors[length(plotcolors)]) # Add raster
  graphics::.filled.contour(x=lons,y=lats,z=field,levels = plotbreaks, col = plotcolors)
  sp::plot(landcont, border="gray24", add = TRUE) # Overlay landmask
  gl = sp::gridlines(plotborder, easts = seq(lonmin,lonmax,20), norths = seq(latmin,latmax,10)) # Generate grid lines
  sp::plot(gl, add=T,col="gray60",lty="dashed",axes=T) # Add grid lines
  sp::plot(plotborder,add=T,axes=T)
  graphics::text(labels(gl, crs.longlat), col = "gray24",offset=1)
  # add legend
  fields::image.plot(x=lons,y=lats,z=field,col=plotcolors,lab.breaks = plotbreaks,
                     legend.args=list(text=guide.title,pad=2, cex=1, side=3, line=2),
                     legend.only=TRUE,horizontal=TRUE,legend.width=1.5,legend.shrink=0.81,
                     zlim=c(plotbreaks[1],plotbreaks[length(plotbreaks)]))
  # Add title
  title(panel.title,line = -2)


  # Shift world map if required
  # if(lonmin>lonmax) {
  #   mapWorld <- ggplot2::map_data("world", wrap=c(-260,100))
  # } else {
  #   mapWorld <- ggplot2::map_data("world")
  # }
  #
  # # Define colors
  # cmap<-c("MAP"="darkgray",
  #         "NONE"="transparent")

  # Load world map
  #base_map <- rnaturalearth::ne_coastline()

  # Plot field
  # raster::plot(field,xaxs="i",yaxs="i",ext=extent(field))



  # Convert rater to DF
  # TH_df <- as.data.frame(field$layer.1, xy = TRUE)
  #
  # ggplot2::ggplot(data = mapWorld) +
  #   ggplot2::geom_raster(data = TH_df, aes(x = x, y = y, fill = layer.1)) +
  #   ggplot2::coord_sf(xlim = c(lonmin, lonmax), ylim = c(latmin,latmax)) +
  #   ggplot2::xlab("") +
  #   ggplot2::ylab("") +
  #   ggplot2::geom_vline(xintercept=seq(-150, 150, by=50),linetype="dashed",color="darkgray") +
  #   ggplot2::geom_hline(yintercept=seq(latmin, latmax, by=20),linetype="dashed",color="darkgray") +
  #   ggplot2::scale_fill_manual(terrain.colors(3)) +
  #   ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
  #                  panel.background = ggplot2::element_rect(fill = NA),
  #                  panel.border = ggplot2::element_rect(fill = NA))


  # Set up projection used in plot
  #crs <- paste("+proj=vandg +lon_0=0 +x_0=0 +y_0=0 +R_A +ellps=WGS84 +datum=WGS84")

  # Draw plot

  #ggplot2::ggplot(data = base_map, aes(geometry = geometry)) +
  #  geom_sf( color = 'grey70', fill=NA, size=0.15)


  # ggplot2::ggplot(data = mapWorld) +
  #   ggplot2::geom_polygon(ggplot2::aes(x=long, y = lat, group = group,color="MAP",fill="NONE"),size=0.1) +
  #   ggplot2::geom_raster(data = TH_df, aes(x = x, y = y, fill = layer.1)) +
  #   ggplot2::coord_sf(xlim = c(lonmin, lonmax), ylim = c(latmin,latmax)) +
  #   ggplot2::xlab("") +
  #   ggplot2::ylab("") +
  #   ggplot2::geom_vline(xintercept=seq(-150, 150, by=50),linetype="dashed",color="darkgray") +
  #   ggplot2::geom_hline(yintercept=seq(latmin, latmax, by=20),linetype="dashed",color="darkgray") +
  #   ggplot2::scale_color_manual(values=cmap) +
  #   ggplot2::scale_fill_manual(values=cmap) +
  #   ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
  #               panel.background = ggplot2::element_rect(fill = NA),
  #               panel.border = ggplot2::element_rect(fill = NA))

#ggtitle(titles[anom]) +
    #   ggspatial::geom_spatial_path(data=x) +


# geom_path(data=cytracks[[1]],aes(x=lon,y=lat,group=cyID,color=cycol,linetype=ltype),alpha=1,size=0.5) +
#   geom_point(aes(lon,lat,color=cycol),data=cystart[[1]],shape=21,alpha=1) +
#   geom_point(aes(lon,lat,color=cycol),data=cyend[[1]],shape=4,alpha=1) +
#   geom_path(data=cytracks[[2]],aes(x=lon,y=lat,group=cyID,color=cycol,linetype=ltype),alpha=1,size=0.5) +
#   geom_point(aes(lon,lat,color=cycol),data=cystart[[2]],shape=21,alpha=1) +
#   geom_point(aes(lon,lat,color=cycol),data=cyend[[2]],shape=4,alpha=1) +
#   geom_path(data=cytracks[[3]],aes(x=lon,y=lat,group=cyID,color=cycol,linetype=ltype),alpha=1,size=0.5) +
#   geom_point(aes(lon,lat,color=cycol),data=cystart[[3]],shape=21,alpha=1) +
#   geom_point(aes(lon,lat,color=cycol),data=cyend[[3]],shape=4,alpha=1) +
#   geom_path(data=cytracks[[4]],aes(x=lon,y=lat,group=cyID,color=cycol,linetype=ltype),alpha=1,size=0.5) +
#   geom_point(aes(lon,lat,color=cycol),data=cystart[[4]],shape=21,alpha=1) +
#   geom_point(aes(lon,lat,color=cycol),data=cyend[[4]],shape=4,alpha=1) +
#   scale_linetype_manual(breaks=c("a","b"), values=c("solid","longdash")) +
#   scale_color_manual("Process",values=cmap) +
#   scale_fill_manual("Process",values=cmap) +

}

# TESTING

#  library(zeallot) # Need this to unpack data from rfile
#  c(APVTURBT)%<-%rfile("/net/thermo/atmosdyn2/atroman/stingjet/20140211_12/tra/APV12_20140212_07.nc","APVTURBT")
# # #APVTURBT<-rfile("/net/thermo/atmosdyn2/atroman/stingjet/20140211_12/tra/APV12_20140212_07.nc","APVTURBT")
#  metplot(APVTURBT,-180,180,0,90,1)
