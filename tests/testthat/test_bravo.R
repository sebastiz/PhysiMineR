# Tests for BRAVO data functions

test_that("dataBRAVODayLabeller: creates bravoDay* columns from Sierra columns", {
  df <- data.frame(
    BravoID = "X1",
    ReflDay1FractionTimepHLessThan4Total = 7.0,
    ReflDay2FractionTimepHLessThan4Total = 6.5
  )
  out <- dataBRAVODayLabeller(df)
  expect_true("bravoDay1" %in% names(out))
  expect_true("bravoDay2" %in% names(out))
  expect_equal(out$bravoNDays, 2L)
})

test_that("GORD_BravoWDAAndAverage: worstt is max of day columns", {
  df <- data.frame(bravoDay1 = 7.0, bravoDay2 = 5.5, bravoDay3 = 3.0)
  out <- GORD_BravoWDAAndAverage(df, n_days = 3)
  expect_equal(out$worstt, 7.0)
  expect_equal(out$WorstD_ayAnalysisGORDPositive, 1L)
})

test_that("GORD_BravoWDAAndAverage: handles NA days correctly", {
  df <- data.frame(bravoDay1 = 7.0, bravoDay2 = NA, bravoDay3 = NA)
  out <- GORD_BravoWDAAndAverage(df, n_days = 3)
  expect_equal(out$NumDaysBravoPositive, 1L)
  expect_equal(out$worstt, 7.0)
})

test_that("dataBRAVOClean: coerces numeric and drops duplicates", {
  df <- data.frame(
    BravoID  = c("B1", "B1"),
    VisitDate = c("2022-01-01", "2022-01-01"),
    ReflDay1FractionTimepHLessThan4Total = c("6.5", "6.5")
  )
  out <- dataBRAVOClean(df)
  expect_equal(nrow(out), 1L)
  expect_true(is.numeric(out$ReflDay1FractionTimepHLessThan4Total))
})
