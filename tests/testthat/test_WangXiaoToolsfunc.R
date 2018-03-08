context("Homework functions")

test_that("func1 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func1(x), x_list)
})

test_that("func2 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func2(x), x_list)
  save<-try(func2(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})


test_that("func3 computes mean, var, sd", {
  x <- 1:10
  alpha <- pi
  log <- function(alpha)
    sum(dgamma(x, shape = alpha, log = TRUE))
  interval <- mean(x) + c(-1,1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)
  
  out<- optimize(log, maximum = TRUE, interval)
  expect_identical(func3(x), out$maximum)
})

test_that("func4 computes the weighted mean, var, sd", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  
  var2 <- function(d){sum(((d$x - sum(d$x * d$p))^2) * d$p)}
  x_list2 <- list(mean=sum(d$x * d$p),var=var2(d),sd=sqrt(var2(d)))
  expect_identical(func4(d), x_list2)

})

test_that("func5 computes the weighted mean, var, sd with user checks", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE) 
  
  var2 <- function(d){sum(((d$x - sum(d$x * d$p))^2) * d$p)}
  x_list2 <- list(mean=sum(d$x * d$p),var=var2(d),sd=sqrt(var2(d)))
  expect_identical(func5(d), x_list2)
  save<-try(func5(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: $ operator is invalid for atomic vectors\n")
  
})

test_that("func6 checks and throws error if not numeric, finit, zero lenth, NA, NAN",{
  x<- 1:10
  check1 <- function(x){tryCatch(stopifnot(is.numeric(x)), error=function(e){print("not numeric")})}
  save <-try(func6(NA),silent = TRUE)
  expect_identical(as.character(func6(NA)),"NA or NAN")
})

test_that("func7 computes the liklihood of a given distribution for data x",{
  x <- rgamma(100,3)
  func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
  func7 <- function(x, func, interval){
    
    f7 <- function(theta, x)
    {sum(func(theta, x))}
    
    
  interval <- c(0,3)  
  out<- optimize(f7, maximum = TRUE, interval, x=x)
  expect_identical(func7(x, func, interval), out$maximum)
 
}})

test_that("wrapper function for ggplot2 for data",{
  plotMyData2<-function(x){
    library(magrittr)
    save = x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_line()
    expect_is(save,"ggplot")
  
}}) 

test_that("wrapper function for dplyr for data",{
  dplyrWrapper <- function(t) {
    colnames(t) = NULL
    for(i in 1:ncol(t))
    {
      colnames(t)[i] = as.character(i)
    }
    save <- dplyr::select(t,ncol(t))
    expect_is(save, "dplyr")
  
  }}) 