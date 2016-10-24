context("testing 'to_date'")

test_that("'to_date' works", {
   expect_equal(to_date("2007-04-01"), as.Date("2007-04-01"))
   x <- c("2001-01-01", "2021-09-13", "", "2006-03-12", " ")
   y <- x
   y[c(3,5)] <- NA
   expect_equal(to_date(x), as.Date(y))
   x[5] <- "xkcd"
   expect_equal(to_date(x, not_dates=c("", "xkcd")), as.Date(y))
})
