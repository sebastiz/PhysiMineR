# Tests for cross-test merging utilities

test_that("testMerge: picks closest-in-time pair per patient", {
  hrm <- data.frame(
    HospNum_Id  = c("A", "A"),
    VisitDate.x = as.Date(c("2020-01-01", "2021-06-01")),
    IRP = c(8, 9)
  )
  bravo <- data.frame(
    HospNum_Id  = "A",
    VisitDate.y = as.Date("2020-01-15"),
    AET = 7.0
  )
  out <- testMerge(hrm, bravo)
  expect_equal(nrow(out), 1L)
  expect_equal(out$IRP, 8)   # Jan 2020 HRM is closer to Jan 2020 BRAVO
})

test_that("testMerge: max_days_apart drops distant pairs", {
  hrm <- data.frame(HospNum_Id = "A",
                    VisitDate.x = as.Date("2020-01-01"), IRP = 8)
  bravo <- data.frame(HospNum_Id = "A",
                      VisitDate.y = as.Date("2021-06-01"), AET = 7.0)
  expect_message(
    out <- testMerge(hrm, bravo, max_days_apart = 365),
    "dropped"
  )
  expect_equal(nrow(out), 0L)
})

test_that("filterByCompleteness: drops columns below threshold", {
  df <- data.frame(a = c(1, 2, NA, 4), b = c(NA, NA, NA, 4), c = 1:4)
  out <- suppressMessages(filterByCompleteness(df, col_complete_threshold = 0.5))
  expect_false("b" %in% names(out))
  expect_true("a" %in% names(out))
})
