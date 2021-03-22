#' Function to test rough equality
#'
#' @param values close are any number of floating values to testval
#' @param testval value to be tested
#' @param epsilon float value indicating how large difference between values is allowed to be
#'
#' @return
#' @export
#'
#' @examples
#' 0.011%==0.01 is TRUE

"%==%" <- function(values,testval,epsilon=0.01) {

  # Return vector with true / false
  out<-c()

  # Go over arbitrary number of values
  for (v in values) {
    if(v+epsilon>testval && v-epsilon<testval) {
      out=append(out,TRUE)
    } else {
      out=append(out,FALSE)
    }
  }

  return(out)

}
