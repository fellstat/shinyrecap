
context("Main")
library(testthat)



test_that("disaggregate", {

  dat <- as.data.frame(structure(c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L,
                     0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 4L, 8L, 12L, 8L, 12L, 9L, 15L
  ), .Dim = c(7L, 4L), .Dimnames = list(NULL, c("OC1", "OC2", "OC3",
                                                "Frequency"))))

  d2 <- disaggregate(dat[-4], dat[[4]])
  d3 <- structure(list(V1 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                              1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                              0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                              0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), V2 = c(1L, 1L, 1L, 1L,
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                                                              0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L,
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                                              1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
                              ), V3 = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L,
                                        1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L,
                                        0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L,
                                        0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                        1L, 1L, 1L, 1L, 1L, 1L, 1L)), row.names = c(NA, -68L), class = "data.frame")
  expect_equivalent(d2,d3)
})

