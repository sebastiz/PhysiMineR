# Tests for Lyon Consensus 2.0 classification functions

test_that("GORD_AcidBRAVO_Lyon: conclusive GORD requires >= 2 days > 6%", {
  df <- data.frame(
    bravoDay1 = c(7.0, 7.0, 5.0, 3.0),
    bravoDay2 = c(7.0, 3.0, 5.0, 3.0),
    bravoDay3 = c(7.0, 3.0, 4.5, 3.0),
    bravoDay4 = c(3.0, 3.0, 5.2, 3.0)
  )
  out <- GORD_AcidBRAVO_Lyon(df)
  expect_equal(as.character(out$AcidRefluxBRAVO_Lyon),
               c("Conclusive GORD", "Inconclusive",
                 "Inconclusive",    "GORD excluded"))
  expect_equal(out$AcidRefluxBRAVO, c(1, 0, 0, 0))
})

test_that("GORD_AcidBRAVO_Lyon: GORD excluded when all days < 4%", {
  df <- data.frame(bravoDay1 = 3.0, bravoDay2 = 2.5)
  out <- GORD_AcidBRAVO_Lyon(df)
  expect_equal(out$AcidRefluxBRAVOTotalOnly, 1L)
  expect_equal(out$AcidRefluxBRAVO,          0L)
})

test_that("GORD_AcidBRAVO_Lyon: errors when no bravoDay* columns present", {
  df <- data.frame(HospNum_Id = "X", AET = 7.0)
  expect_error(GORD_AcidBRAVO_Lyon(df), "bravoDay")
})

test_that("GORD_AcidImp_Lyon: AET > 6 is Conclusive GORD", {
  df <- data.frame(
    MainAcidExpTotalClearanceChannelPercentTime = c(7.0, 5.0, 2.0)
  )
  out <- GORD_AcidImp_Lyon(df)
  expect_equal(as.character(out$AcidReflux_Lyon),
               c("Conclusive GORD", "Inconclusive", "GORD excluded"))
  expect_equal(out$AcidReflux_Imp, c(1L, 0L, 0L))
})

test_that("GORD_AcidImp_Lyon: adjunctive MNBI upgrades borderline to Conclusive", {
  df <- data.frame(
    MainAcidExpTotalClearanceChannelPercentTime = c(5.0, 5.0),
    mnbi = c(2000, 2500)   # 2000 < 2292 threshold => upgrade
  )
  out <- GORD_AcidImp_Lyon(df, mnbi_col = "mnbi")
  expect_equal(as.character(out$AcidReflux_Lyon),
               c("Conclusive GORD (adjunctive)", "Inconclusive"))
})

test_that("GORD_AcidImp: legacy 4.2% threshold works", {
  df <- data.frame(
    MainAcidExpTotalClearanceChannelPercentTime = c(4.3, 4.1, 4.2)
  )
  out <- GORD_AcidImp(df)
  expect_equal(out$AcidReflux_Imp, c(1L, 0L, 0L))
})
