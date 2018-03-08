#' 1st Wrapper function for ggplot2 for data d
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plotMyData(d)
plotMyData<-function(x){
  library(magrittr)
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_jitter(width = 30, height = 30)
}

#' 2nd Wrapper function for ggplot2 for data d
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plotMyData1(d)
plotMyData1<-function(x){
  library(magrittr)
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}

#' 3rd Wrapper function for ggplot2 for data d
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plotMyData2(d)
plotMyData2<-function(x){
  library(magrittr)
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_line()
}



#' Wrapper function for dplyer 
#'
#' This is a wrapper for acquiring marginals and other slices of data
#' 
#' @param x data.frame
#'
#' @return dplyr
#' @export
#' @examples
#' data(d)
#' dplyrWrapper(d)
dplyrWrapper <- function(t) {
  colnames(t) = NULL
  for(i in 1:ncol(t))
  {
    colnames(t)[i] = as.character(i)
  }
  dplyr::select(t,ncol(t))
}
