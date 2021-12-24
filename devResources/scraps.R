x <- sample(1000)
x <- as.data.frame(x)
usethis::use_data(x, mtcars, overwrite = TRUE)

str(x)

nrow

dim

.Primitive

dim <-function(x) {c(1, 1)}
dim(mtcars)
nrow(mtcars)

search()

old <- search()
old
testthat::expect_equal(1, 1)
setdiff(search(), old)

expect_true(TRUE)



