# Tests for Chicago Classification v4.0

make_hrm <- function(irp, failed, panoesopha, premature,
                     hypercontract, dci_mean, large_brk, small_brk, simult) {
  data.frame(
    ResidualmeanmmHg                    = irp,
    failedChicagoClassification         = failed,
    panesophagealpressurization         = panoesopha,
    prematurecontraction                = premature,
    rapidcontraction                    = hypercontract,
    DistalcontractileintegralmeanmmHgcms = dci_mean,
    largebreaks                         = large_brk,
    smallbreaks                         = small_brk,
    Simultaneous                        = simult
  )
}

test_that("Achalasia Type II: elevated IRP + panoesopha >= 20%", {
  df  <- make_hrm(20, 50, 25, 0, 0, 200, 0, 0, 0)
  out <- HRMDiagnoses(df)
  expect_equal(out$ChicagoV4Diagnosis, "Achalasia Type II")
  expect_equal(out$ChicagoV4DiagnosisGroup, "Achalasia")
})

test_that("Achalasia Type I: elevated IRP + 100% failed + no panoesopha", {
  df  <- make_hrm(20, 100, 0, 0, 0, 60, 0, 0, 0)
  out <- HRMDiagnoses(df)
  expect_equal(out$ChicagoV4Diagnosis, "Achalasia Type I")
})

test_that("Achalasia Type III: elevated IRP + premature >= 20%", {
  df  <- make_hrm(18, 30, 0, 25, 0, 300, 0, 0, 0)
  out <- HRMDiagnoses(df)
  expect_equal(out$ChicagoV4Diagnosis, "Achalasia Type III")
})

test_that("DES: normal IRP + premature >= 20%", {
  df  <- make_hrm(8, 0, 0, 25, 0, 1200, 0, 0, 0)
  out <- HRMDiagnoses(df)
  expect_equal(out$ChicagoV4Diagnosis, "Distal Oesophageal Spasm")
  expect_equal(out$ChicagoV4DiagnosisGroup, "Major Peristalsis Disorder")
})

test_that("Jackhammer: >= 20% hypercontractile swallows", {
  df  <- make_hrm(12, 0, 0, 0, 25, 9000, 0, 0, 0)
  out <- HRMDiagnoses(df)
  expect_equal(out$ChicagoV4Diagnosis, "Hypercontractile Oesophagus (Jackhammer)")
})

test_that("Absent contractility: normal IRP + 100% failed", {
  df  <- make_hrm(6, 100, 0, 0, 0, 60, 0, 0, 0)
  out <- HRMDiagnoses(df)
  expect_equal(out$ChicagoV4Diagnosis, "Absent Contractility")
})

test_that("IEM: >= 70% ineffective swallows", {
  df  <- make_hrm(10, 80, 0, 0, 5, 200, 0, 0, 0)
  out <- HRMDiagnoses(df)
  expect_equal(out$ChicagoV4Diagnosis, "Ineffective Oesophageal Motility")
  expect_equal(out$ChicagoV4DiagnosisGroup, "Minor Peristalsis Disorder")
})

test_that("Normal: no criteria met", {
  df  <- make_hrm(10, 10, 5, 5, 3, 800, 5, 5, 0)
  out <- HRMDiagnoses(df)
  expect_equal(out$ChicagoV4Diagnosis, "Normal")
  expect_equal(out$ChicagoV4DiagnosisGroup, "Normal")
})
