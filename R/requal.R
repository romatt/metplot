#' Test rough equality
#'
#' @param values close are any number of floating values to testval
#' @param testval value to be tested
#' @param epsilon float value indicating how large difference between values is allowed to be
#'
#' @return
#' @export
#'
#' @examples
#' c(0.01,0.02,0.011)%==%0.01 equals TRUE FALSE  TRUE

"%==%" <- function(values,testval,epsilon=0.01) {

  out<-sapply(values, function(v) {
    if (v+epsilon>testval && v-epsilon<testval){
      return(TRUE)
    } else {
      return(FALSE)
    }})

  return(out)

}
