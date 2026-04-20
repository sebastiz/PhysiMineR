# =============================================================================
# NegImpPredictorsOfAllPosBRAVO – Rewritten Analysis Script
# Uses PhysiMineR v1.0.0 (Lyon Consensus 2.0 / Chicago Classification v4.0)
# =============================================================================
#
# Study question:
#   Among patients with a NEGATIVE ambulatory pH-impedance study, which clinical
#   and physiological variables predict a POSITIVE wireless pH (BRAVO) result?
#
# Outcome variable:
#   AcidRefluxBRAVOAv – binary (1 = Conclusive GORD by average-day analysis,
#   AET mean > 6% per Lyon Consensus 2.0; 0 = not conclusive).
#
# Secondary outcome:
#   WorstD_ayAnalysisGORDPositive – 1 if worst single-day AET > 6%.
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
library(PhysiMineR)   # core physiology functions
library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readxl)
library(ggplot2)
library(ggbeeswarm)
library(gtsummary)
library(finalfit)
library(cutpointr)
library(ggpubr)
library(eeptools)
library(gt)

# ── 1. Import ─────────────────────────────────────────────────────────────────
## @knitr dataImport

oldw <- getOption("warn")
options(warn = -1)

data_dir <- here::here("NegImpPredictorsOfAllPosBRAVO",
                        "NegImpPredictorsOfAllPosBRAVO", "data")

# BRAVO data (two-day and four-day exports plus totals)
BravoDayOneAndTwo   <- read.csv(file.path(data_dir, "BravoDay1And2.csv"))
BravoDayThreeAndFour <- read.csv(file.path(data_dir, "BravoDay3And4.csv"))
BRAVOTotal          <- read.csv(file.path(data_dir, "BRAVOTotal.csv"))

# pH-impedance
ImpedanceTwo <- read.csv(file.path(data_dir, "Impedance2.csv"))
Imp_Symp     <- read.csv(file.path(data_dir, "Imp_Symp.csv"))

# HRM
HRMImportMain     <- read.csv(file.path(data_dir, "HRMImportMain.csv"))
HRMImportSwallows <- read.csv(file.path(data_dir, "HRMImportSwallows.csv"))

# Adjunctive impedance metrics
MNBI  <- read_excel(file.path(data_dir, "MNBI.xlsx"))
MNBI2 <- read_excel(file.path(data_dir, "MNBI2.xlsx"))
PSPW  <- readxl::read_xlsx(file.path(data_dir, "Seb_PSPW_list (003).xlsx")) |>
         dplyr::rename(HospitalNumber = HospitalNum)

# Endoscopy procedures linked to BRAVO placement
Procs <- read.csv(file.path(data_dir, "ProcByHospitalNumbers.csv"))

options(warn = oldw)

# ── 2. Clean each dataset ─────────────────────────────────────────────────────
## @knitr dataClean

# ── 2a. pH-impedance ──────────────────────────────────────────────────────────
AllImpedance <- merge(ImpedanceTwo, Imp_Symp, by = "Imp_Id", all = TRUE)
ImpedanceClean <- dataImpClean(AllImpedance)
ImpAll         <- dataImpSymptoms(ImpedanceClean)

# Classify impedance studies using Lyon Consensus 2.0
# (legacy 4.2% threshold kept as AcidReflux_Imp for backward comparison)
ImpAll <- GORD_AcidImp(ImpAll)                 # binary, legacy 4.2% cutoff
ImpAll <- GORD_AcidImp_Lyon(ImpAll,            # Lyon 2.0 three-tier
                              mnbi_col = NULL,   # adjunctive added after merge
                              pspw_col = NULL)

# ── 2b. BRAVO ─────────────────────────────────────────────────────────────────
AllBravo <- merge(BravoDayOneAndTwo, BravoDayThreeAndFour, by = "BravoID", all = TRUE) |>
            merge(BRAVOTotal, by = "BravoID", all = TRUE)

AllBravo <- dataBRAVOClean(AllBravo)
AllBravo <- dataBRAVODayLabeller(AllBravo, id_col = "HospNum_Id", date_col = "VisitDate")
AllBravo <- dataBRAVOSymptoms(AllBravo)

# Lyon Consensus 2.0 BRAVO classification
# (Conclusive GORD = AET > 6% on >= 2 days; GORD excluded = all days < 4%)
AllBravo <- GORD_AcidBRAVO_Lyon(AllBravo,
                                  pathological_aet  = 6,
                                  normal_aet        = 4,
                                  min_positive_days = 2)

# Worst-day and average-day analyses (n_days = 4 for standard 96 h BRAVO)
AllBravo <- GORD_BravoWDAAndAverage(AllBravo, n_days = 4, aet_threshold = 6)

# ── 2c. HRM ───────────────────────────────────────────────────────────────────
# Strip per-swallow raw trace columns (Num1 … NumN) before merging
HRMImportSwallows_trim <- HRMImportSwallows[
  , !grepl("Num\\d+", colnames(HRMImportSwallows))
]
# Remove swallows where key column is NA (data quality filter)
HRMImportSwallows_trim <- HRMImportSwallows_trim[
  !is.na(HRMImportSwallows_trim$panesophagealpressurizationMapSwallowsNum8),
]

AllHRM <- merge(HRMImportMain, HRMImportSwallows_trim, by = "HRM_Id", all = TRUE)
AllHRM <- AllHRM[, !grepl("Num\\d+", colnames(AllHRM))]   # final drop of trace cols
AllHRM <- HRMCleanUp1(AllHRM)

# Chicago Classification v4.0 motility diagnosis
AllHRM <- HRMDiagnoses(AllHRM)

# ── 2d. Adjunctive impedance metrics (MNBI + PSPW) ───────────────────────────
MNBI_all <- rbind(MNBI, MNBI2) |>
  dplyr::rename(HospNum_Id = HospitalNumber)

PSPW_slim <- PSPW |>
  dplyr::rename(HospNum_Id = HospitalNumber) |>
  dplyr::select(HospNum_Id, pspw_index)

MNBI_merged <- MNBI_all |>
  dplyr::left_join(PSPW_slim, by = "HospNum_Id") |>
  dplyr::rename(M1_av = MethodOne_average) |>
  dplyr::select(HospNum_Id, M1_av, pspw_index, comments) |>
  dplyr::mutate(M1_av = as.numeric(M1_av))

# ── 2e. BRAVO procedure / endoscopy data ─────────────────────────────────────
Procs <- data.frame(Procs, stringsAsFactors = FALSE)
names(Procs) <- c("HospNum_Id", "VisitDate.x", "Findings", "Oesophagitis")
Procs$VisitDate.x <- as.Date(Procs$VisitDate.x)

# ── 3. Cross-test merging ─────────────────────────────────────────────────────
## @knitr dataCrossTestMerge

# HRM + Impedance (closest visit dates)
ImpAndHRM <- testMerge(AllHRM, ImpAll, id_col = "HospNum_Id")

# HRM + BRAVO (closest visit dates)
BravoAndHRM <- testMerge(AllHRM, AllBravo, id_col = "HospNum_Id")

# Impedance + BRAVO (closest visit dates)
ImpAndBravo <- testMerge(AllBravo, ImpAll, id_col = "HospNum_Id")

# Triple merge: HRM × Impedance × BRAVO
# Uses inner joins; closest-date pair selected at each step
ImpAndBravoWithHRM <- testMerge(
  AllHRM, ImpAndBravo,
  id_col         = "HospNum_Id",
  join_type      = "inner",
  max_days_apart = 365    # enforce <= 1 year between BRAVO and impedance
)

# ── 4. Add adjunctive metrics and re-classify impedance with Lyon 2.0 ─────────
ImpAndBravoWithHRM <- dplyr::left_join(ImpAndBravoWithHRM, MNBI_merged,
                                        by = "HospNum_Id")

# Now that MNBI / PSPW are available, re-run Lyon classification with adjunctive
ImpAndBravoWithHRM <- GORD_AcidImp_Lyon(
  ImpAndBravoWithHRM,
  mnbi_col = "M1_av",
  pspw_col = "pspw_index"
)

# ── 5. Derive analysis dataset ────────────────────────────────────────────────
## @knitr dataForking

# Age at time of pH-impedance test
ImpAndBravoWithHRM$ageInYears <- tryCatch({
  eeptools::age_calc(
    na.omit(ImpAndBravoWithHRM$MainPtDataDateofBirth),
    ImpAndBravoWithHRM$MainPtDataDateofAdmission,
    precise = TRUE
  ) / 12
}, error = function(e) NA_real_)

# Restrict to patients with NEGATIVE impedance study (AcidReflux_Imp == 0)
NegImp <- ImpAndBravoWithHRM |>
  dplyr::filter(AcidReflux_Imp == 0)

# Also exclude hypersensitive oesophagus pattern:
# > 80 total reflux episodes in context of normal AET
NegImp <- NegImp |>
  dplyr::filter(
    (MainRflxEpisodeTotalAcid + MainRflxEpisodeTotalNonacid) < 80
  )

# Remove raw BRAVO day-by-day and swallow columns (not used as predictors)
NegImp <- NegImp[, !grepl("^bravoDay|[Ss]wallo", names(NegImp))]

# Add endoscopy procedure data (oesophagitis grade at BRAVO placement)
NegImp <- merge(NegImp, Procs, by = c("HospNum_Id", "VisitDate.x"), all.x = TRUE)
NegImp$Oesophagitis <- ifelse(grepl("Y", NegImp$Oesophagitis), "Y", "N")

# Impute missing gender (small proportion recorded as "null")
NegImp$Gender <- ifelse(
  NegImp$Gender == "null" | is.na(NegImp$Gender),
  sample(c("Male", "Female"), 1),
  NegImp$Gender
)

# Add adjunctive impedance metrics for descriptive analysis
NegImp <- dplyr::left_join(NegImp, MNBI_merged, by = "HospNum_Id")

# ── 6. BRAVO descriptive analysis ─────────────────────────────────────────────
## @knitr BRAVOdescription

# Proportion positive by each definition
pHWDA  <- mean(NegImp$worstt  >= 6, na.rm = TRUE) * 100
SAPWDA <- mean(apply(dplyr::select(NegImp, dplyr::contains("SAP")),
                     1, function(r) any(r > 94.9, na.rm = TRUE)),
               na.rm = TRUE) * 100
SIWDA  <- mean(apply(dplyr::select(NegImp, dplyr::contains("SI")),
                     1, function(r) any(r > 50, na.rm = TRUE)),
               na.rm = TRUE) * 100

message(sprintf("Positive by worst-day pH:  %.1f%%", pHWDA))
message(sprintf("Positive by worst-day SAP: %.1f%%", SAPWDA))
message(sprintf("Positive by worst-day SI:  %.1f%%", SIWDA))

# Sub-group: Lyon Conclusive GORD patients
NegImp_GORD <- NegImp[NegImp$AcidRefluxBRAVOAv == 1, ]

# Worst day distribution within GORD-positive patients
worstDayTable <- as.data.frame(table(NegImp_GORD$worstDaypH))
names(worstDayTable) <- c("Day", "Freq")

# Percentage adjusted for available monitoring days
day_avail_cols <- paste0("bravoDay", 1:6)
for (i in seq_len(nrow(worstDayTable))) {
  day_col <- day_avail_cols[i]
  if (day_col %in% names(NegImp_GORD)) {
    n_avail <- sum(!is.na(NegImp_GORD[[day_col]]))
    worstDayTable$Percentage[i] <- if (n_avail > 0)
      worstDayTable$Freq[i] / n_avail * 100
    else NA_real_
  } else {
    worstDayTable$Percentage[i] <- NA_real_
  }
}

# Plot: worst day
fig_worst_day <- ggplot(worstDayTable,
                         aes(x = Day, y = Percentage)) +
  geom_col(fill = "steelblue") +
  labs(x = "Worst day (AET pH < 4)",
       y = "% of GORD-positive studies",
       title = "Distribution of worst monitoring day in BRAVO-positive patients") +
  theme_bw()

# Plot: worst-day AET, GORD-positive vs negative
fig_worst_vs_group <- ggplot(
  NegImp |> dplyr::filter(!is.na(worstt) & worstt < 25),
  aes(x = factor(AcidRefluxBRAVOAv,
                  labels = c("BRAVO negative", "BRAVO positive")),
      y = worstt)
) +
  geom_violin(fill = "tomato", alpha = 0.4) +
  geom_beeswarm(size = 1, priority = "density", cex = 2) +
  geom_boxplot(width = 0.1, alpha = 0.3) +
  labs(x = "BRAVO outcome (Lyon average-day analysis)",
       y = "Worst single-day AET (%)",
       title = "Worst-day AET by BRAVO outcome") +
  theme_bw()

# Number of positive days distribution
numDaysTable <- as.data.frame(table(NegImp_GORD$NumDaysBravoPositive))
names(numDaysTable) <- c("NumPositiveDays", "Freq")
numDaysTable$Percentage <- numDaysTable$Freq / nrow(NegImp_GORD) * 100

fig_num_pos_days <- ggplot(numDaysTable,
                            aes(x = NumPositiveDays, y = Percentage)) +
  geom_col(fill = "steelblue") +
  labs(x = "Number of days with AET > 6%",
       y = "% of GORD-positive studies",
       title = "Number of positive BRAVO days") +
  theme_bw()

# Symptom frequency bar chart
sx_counts <- data.frame(
  Symptom   = c("Belching","ChestPain","Cough","Heartburn",
                "Regurgitation","Throat","Vomiting","Nausea","StomachPain"),
  Frequency = c(
    sum(grepl("Belch",         NegImp$AllSymps_BRAVO, ignore.case = TRUE), na.rm = TRUE),
    sum(grepl("ChestPain",     NegImp$AllSymps_BRAVO, ignore.case = TRUE), na.rm = TRUE),
    sum(grepl("Cough",         NegImp$AllSymps_BRAVO, ignore.case = TRUE), na.rm = TRUE),
    sum(grepl("Heartburn",     NegImp$AllSymps_BRAVO, ignore.case = TRUE), na.rm = TRUE),
    sum(grepl("Regurgitation", NegImp$AllSymps_BRAVO, ignore.case = TRUE), na.rm = TRUE),
    sum(grepl("Throat",        NegImp$AllSymps_BRAVO, ignore.case = TRUE), na.rm = TRUE),
    sum(grepl("Vomiting",      NegImp$AllSymps_BRAVO, ignore.case = TRUE), na.rm = TRUE),
    sum(grepl("Nausea",        NegImp$AllSymps_BRAVO, ignore.case = TRUE), na.rm = TRUE),
    sum(grepl("StomachPain|Epigastric", NegImp$AllSymps_BRAVO, ignore.case = TRUE), na.rm = TRUE)
  )
)
sx_counts$Percentage <- sx_counts$Frequency / nrow(NegImp) * 100

fig_symptoms <- ggplot(sx_counts, aes(x = Symptom, y = Percentage)) +
  geom_col(fill = "steelblue") +
  labs(x = "Symptom", y = "% of patients",
       title = "Presenting symptoms (negative impedance cohort)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combined Figure 1
ggarrange(fig_worst_day, fig_worst_vs_group,
          fig_num_pos_days, fig_symptoms,
          labels = c("a)", "b)", "c)", "d)"),
          ncol = 2, nrow = 2)

# ── 7. Select modelling columns ───────────────────────────────────────────────
## @knitr missingClean

keep_pattern <- paste0(
  "ageInYears|MainAcidEx|MainSx|SxMain|HospNum_Id|",
  "DistalLESfromnarescm|Gender|Height|LESlengthcm|PIPfromnarescm|",
  "Hiatalhernia|BasalrespiratoryminmmHg|BasalrespiratorymeanmmHg|",
  "DistalcontractileintegralmeanmmHgcms|Contractilefrontvelocitycms|",
  "IntraboluspressureATLESRmmHg|Distallatency|failedChicagoClassification|",
  "panesophagealpressurization|largebreaks|Simultaneous|prematurecontraction|",
  "rapidcontraction|smallbreaks|VisitDate|AcidReflux|Upright|Recumbent|SAP|",
  "M1_av|pspw_index|ChicagoV4Diagnosis"
)

NegImp_min <- NegImp[, grepl(keep_pattern, names(NegImp))]

# Drop columns not relevant to primary analysis
drop_pattern <- paste0(
  "Unrelated|AllReflux|^SITotal|^SIDay|RSSI|SxCorr|ChannelTime|",
  "Duration|ClearanceTime|Composite|PP|Meds|UESMean|ResidMean|",
  "MainProc|Catheter|Migration|BravoID|Stats|Procedure|FileCreation|",
  "TimeToNext|TimeSinceLast|Imp_Id|^id$|PatientScore|MainPtData|",
  "GastricChannel"
)
NegImp_min <- NegImp_min[, !grepl(drop_pattern, names(NegImp_min))]

# Remove columns with < 90% complete data
NegImp_QC <- filterByCompleteness(NegImp_min,
                                   col_complete_threshold = 0.9,
                                   verbose = TRUE)

# ── 8. Univariate analysis – summary table ────────────────────────────────────
## @knitr UnivariatePredictorsValue

# Factor-encode the two outcome variables
NegImp_QC$AcidRefluxBRAVOAv <- factor(
  NegImp_QC$AcidRefluxBRAVOAv,
  levels = c(0, 1), labels = c("Negative", "Positive")
)
NegImp_QC$WorstD_ayAnalysisGORDPositive <- factor(
  NegImp_QC$WorstD_ayAnalysisGORDPositive,
  levels = c(0, 1), labels = c("Negative", "Positive")
)

# Rearrange so outcome is first column
NegImp_QC <- NegImp_QC |>
  dplyr::select(AcidRefluxBRAVOAv, ageInYears, dplyr::everything())

# ── Average-day analysis summary table ────────────────────────────────────────
tbl_av <- tbl_summary(
  data      = dplyr::select(NegImp_QC, -WorstD_ayAnalysisGORDPositive),
  by        = AcidRefluxBRAVOAv,
  missing   = "no",
  type      = list(all_numeric() ~ "continuous"),
  statistic = list(all_continuous()   ~ "{mean} ({sd})",
                   all_categorical()  ~ "{n} / {N} ({p}%)")
) |>
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) |>
  bold_labels() |>
  italicize_levels() |>
  bold_p(t = 0.05) |>
  modify_header(stat_by = "**{level}**, N = {n} ({style_percent(p, symbol=TRUE)})")

# ── Worst-day analysis summary table ─────────────────────────────────────────
NegImp_WDA <- NegImp_QC |>
  dplyr::select(-AcidRefluxBRAVOAv)

tbl_wda <- tbl_summary(
  data      = NegImp_WDA,
  by        = WorstD_ayAnalysisGORDPositive,
  missing   = "no",
  type      = list(all_numeric() ~ "continuous"),
  statistic = list(all_continuous()   ~ "{mean} ({sd})",
                   all_categorical()  ~ "{n} / {N} ({p}%)")
) |>
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 2)) |>
  bold_labels() |>
  italicize_levels() |>
  bold_p(t = 0.05) |>
  modify_header(stat_by = "**{level}**, N = {n} ({style_percent(p, symbol=TRUE)})")

# Combined table
tbl_merge(list(tbl_av, tbl_wda),
           tab_spanner = c("Average-day analysis (Lyon 2.0)",
                           "Worst-day analysis (Lyon 2.0)"))

# ── 9. Logistic regression ─────────────────────────────────────────────────────
## @knitr HRM_MultipleLogRegression2

# ── Average-day analysis model ────────────────────────────────────────────────
# Select p < 0.05 predictors from univariate table
meta_av   <- as.data.frame(tbl_av$meta_data, stringsAsFactors = FALSE)
sig_vars  <- meta_av$var_label[meta_av$p.value < 0.05]

chosen_av <- NegImp_QC |>
  dplyr::select(AcidRefluxBRAVOAv,
                dplyr::any_of(
                  names(NegImp_QC)[Hmisc::label(NegImp_QC) %in% sig_vars]
                ))

chosen_av$AcidRefluxBRAVOAv <- as.factor(chosen_av$AcidRefluxBRAVOAv)
if ("Oesophagitis" %in% names(chosen_av)) {
  chosen_av$Oesophagitis <- as.integer(grepl("Y", chosen_av$Oesophagitis))
}

mod_av <- glm(AcidRefluxBRAVOAv ~ .,
              data   = dplyr::select_if(chosen_av, is.numeric),
              family = binomial(link = "logit"))
tbl_av_reg <- tbl_regression(mod_av, exponentiate = TRUE) |> bold_p(t = 0.05)
tbl_av_reg

# ── Worst-day analysis model ──────────────────────────────────────────────────
meta_wda  <- as.data.frame(tbl_wda$meta_data, stringsAsFactors = FALSE)
sig_wda   <- meta_wda$var_label[meta_wda$p.value < 0.05]

chosen_wda <- NegImp_QC |>
  dplyr::select(WorstD_ayAnalysisGORDPositive,
                dplyr::any_of(
                  names(NegImp_QC)[Hmisc::label(NegImp_QC) %in% sig_wda]
                ))

chosen_wda$WorstD_ayAnalysisGORDPositive <- as.factor(
  chosen_wda$WorstD_ayAnalysisGORDPositive
)
if ("Oesophagitis" %in% names(chosen_wda)) {
  chosen_wda$Oesophagitis <- as.integer(grepl("Y", chosen_wda$Oesophagitis))
}

mod_wda <- glm(WorstD_ayAnalysisGORDPositive ~ .,
               data   = dplyr::select_if(chosen_wda, is.numeric),
               family = binomial(link = "logit"))
tbl_wda_reg <- tbl_regression(mod_wda, exponentiate = TRUE) |> bold_p(t = 0.05)
tbl_wda_reg

# ── 10. Optimal AET cut-point for predicting BRAVO positivity ─────────────────
## @knitr CutPointForAcid

# Select AET column and outcome
aet_col <- "MainAcidExpTotalClearanceChannelPercentTime"
cut_df  <- chosen_av[, c("AcidRefluxBRAVOAv", aet_col)]
cut_df  <- cut_df[stats::complete.cases(cut_df), ]
cut_df[[aet_col]] <- as.numeric(cut_df[[aet_col]])

# cutpointr: maximise sensitivity + specificity
cp <- cutpointr(
  x      = cut_df[[aet_col]],
  class  = as.character(cut_df$AcidRefluxBRAVOAv),
  na.rm  = TRUE,
  method = maximize_metric,
  metric = sum_sens_spec
)
summary(cp)

fig_cp_density <- plot_x(cp) +
  geom_density(colour = "white", fill = "grey") +
  xlab("Impedance AET (%)") +
  ggtitle("AET distribution by BRAVO outcome")

fig_cp_roc <- plot_roc(cp) +
  ggtitle("ROC – impedance AET predicting BRAVO positivity")

ggarrange(fig_cp_density, fig_cp_roc, labels = c("a)", "b)"), ncol = 2)

# ── 11. Logistic probability curve: impedance AET → BRAVO positivity ──────────
## @knitr AETriskCurve

dat <- cut_df
dat$bravo_pos <- as.integer(dat$AcidRefluxBRAVOAv == "Positive")

fit_aet <- glm(bravo_pos ~ dat[[aet_col]], data = dat, family = binomial)
message("OR per 1% increase in impedance AET:")
print(exp(cbind(OR = coef(fit_aet), confint(fit_aet))))

pred_grid <- data.frame(
  aet = seq(floor(min(dat[[aet_col]], na.rm = TRUE)),
            ceiling(max(dat[[aet_col]], na.rm = TRUE)),
            by = 0.5)
)
names(pred_grid) <- aet_col
pred_obj <- predict(fit_aet, newdata = pred_grid, type = "link", se.fit = TRUE)

pred_grid$prob      <- plogis(pred_obj$fit)
pred_grid$lower_95  <- plogis(pred_obj$fit - 1.96 * pred_obj$se.fit)
pred_grid$upper_95  <- plogis(pred_obj$fit + 1.96 * pred_obj$se.fit)

ggplot(pred_grid, aes(x = .data[[aet_col]], y = prob)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = dat,
    aes(x = .data[[aet_col]], y = as.numeric(dat$bravo_pos) - 0.5),
    sides = "b", alpha = 0.3
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(x = "Impedance AET (%)",
       y = "Predicted probability of BRAVO-positive GORD",
       title = "Probability of Lyon-conclusive GORD on BRAVO\nby impedance acid exposure time") +
  theme_bw()

# ── 12. Violin plots for significant univariate predictors ────────────────────
## @knitr UnivariatePredictorsGraph

plot_predictor <- function(df, outcome_col, predictor_col, x_label, y_label) {
  ggplot(df, aes(x = .data[[outcome_col]], y = .data[[predictor_col]])) +
    geom_violin(fill = "tomato", alpha = 0.3) +
    geom_beeswarm(size = 1, priority = "density", cex = 2) +
    geom_boxplot(width = 0.1, alpha = 0.4) +
    labs(x = x_label, y = y_label) +
    theme_bw()
}

figs <- lapply(names(chosen_av)[-1], function(v) {
  plot_predictor(chosen_av, "AcidRefluxBRAVOAv", v,
                 x_label = "BRAVO outcome",
                 y_label = Hmisc::label(chosen_av[[v]]))
})
if (length(figs) >= 2) {
  ggarrange(plotlist = figs[1:min(4, length(figs))],
            ncol = 2, nrow = 2,
            labels = letters[seq_along(figs[1:min(4, length(figs))])])
}
