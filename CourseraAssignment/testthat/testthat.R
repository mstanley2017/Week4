library(testthat)
library(CourseraAssignment)

test_check("CourseraAssignment")
x<-2020
fars_read_years(x)
expect_that(fars_read_years(x),gives_warning())

y<-2013
fars_read_years(y)
expect_that(fars_read_years(y),gives_warning())
