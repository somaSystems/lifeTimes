devtools::has_devel()
xx <- c(TRUE, 2)

xx <- c(1.7, "a")
xx <- c(TRUE, 2)
xx <- c("a", TRUE)
typeof(xx)

studentBio <- list(studentName = "Harry Potter", studentAge = 19, studentContact="London")
class(studentBio) <- "StudentInfo"
studentBio

#This function is called contact
#The input is an object
#Then the function uses the method "contact"
#What is the <UseMethod("contact")>?
#

contact <- function(object) {
  UseMethod("contact")
}


#This function makes a method
#The method is called "contact"
#The method is for a particular class
#The class is student info
#The syntax is "method.class"
#Methods are for a class
#Methods are like functions for a class?

#cat(), means concatenate?
# object$studentContact
# This means when you use this function on a member ofa class,
# it will take the object in that class and
# look up the $studentContact (part of that obeject)

contact.StudentInfo <- function(object) {
  cat("Your contact is", object$studentContact, "\n")
}
contact(studentBio)

install.packages("available")
library(available)
available("lifetimes")
library("available")
suggest(text = "lifetimes")

install.packages("goodpractice")

library(goodpractice)
pkg_path <- system.file("bad1", package = "goodpractice")
g <- gp(pkg_path)
system.file()

getwd()
setwd("..")
pkg_path <- getwd()
g <- gp(pkg_path)
g

install.packages("lifeTimes")

callf2 <- function(a='A',b=NULL) {
  print( ifelse( is.null(a), '# a not specified', paste('# a =',a) ) )
  print( ifelse( is.null(b), '# b not specified', paste('# b =',b) ) )
  argList <-  as.list(match.call(expand.dots = TRUE)[-1])
  # Enforce inclusion of non-optional arguments
  argList$a <- a
  do.call(f2,argList)
}

argList
match.call(expand.dots = TRUE)

