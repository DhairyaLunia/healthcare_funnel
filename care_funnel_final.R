# =============================================================================
# MILI 6963 Healthcare Analytics — DataWatch White Paper
# Title:  The Patient Care Funnel: Drop-Off Across Stages of
#         Type 2 Diabetes Care
# Data:   Uni_USA Synthetic Claims Files (2014)
# Author: [Your Name]
# =============================================================================

# -----------------------------------------------------------------------------
# STEP 0: Install packages (run once, then comment out)
# -----------------------------------------------------------------------------
# install.packages("haven")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("scales")

library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# -----------------------------------------------------------------------------
# STEP 1: Load data files
# -----------------------------------------------------------------------------
MEM_PATH <- "C:/Users/dhair/Downloads/usa_mem_syn_2014_fin.sas7bdat"
CLM_PATH <- "C:/Users/dhair/Downloads/synth5_us_clm_x_2014.sas7bdat"

cat("Loading membership file...\n")
mem <- read_sas(MEM_PATH)
cat("Membership:", nrow(mem), "rows |", ncol(mem), "columns\n\n")

cat("Loading claims file...\n")
clm <- read_sas(CLM_PATH)
cat("Claims:", nrow(clm), "rows |", ncol(clm), "columns\n\n")

# -----------------------------------------------------------------------------
# STEP 2: Rename claims columns to confirmed variable names
# -----------------------------------------------------------------------------
clm <- clm %>%
  rename(
    PERSON    = PERSON,
    SVC_RAW   = FIRST_DOS,      # character date "2014-05-07"
    SVC_SAS   = SAS_FIRST_DOS,  # numeric SAS date (days since 1960-01-01)
    DX1       = DIAG1_V9,
    DX2       = DIAG2_V9,
    DX3       = DIAG3_V9,
    PROC1     = CPT_HCPCS,
    PAY       = PROV_PAID,
    COINS     = COINSUR,
    COPAY     = COPAYMENT,       # FIX #2: include copayment in OOP
    DEDUCT    = DEDUCTIBLE,
    AMT_ALLOW = AMT_ALLOWED      # total negotiated cost
  )

# FIX #1: Recover Medicare patients whose FIRST_DOS is NA
# Use SAS_FIRST_DOS (numeric) as fallback — SAS dates = days since 1960-01-01
clm <- clm %>%
  mutate(
    SVC_DATE = coalesce(
      as.Date(SVC_RAW),                               # works for non-Medicare
      as.Date(SVC_SAS, origin = "1960-01-01")         # fallback for Medicare
    )
  )

cat("Date check — NAs after fix:", sum(is.na(clm$SVC_DATE)), "\n")
cat("Sample dates:", format(head(clm$SVC_DATE, 3)), "\n\n")

# -----------------------------------------------------------------------------
# STEP 3: Flag Type 2 Diabetes diagnosis (ICD-9: 250.x0 or 250.x2)
# Vectorized — runs in seconds on 8M+ rows
# -----------------------------------------------------------------------------
is_t2dm_vec <- function(code) {
  code      <- trimws(as.character(code))
  starts250 <- substr(code, 1, 3) == "250"
  fifth_ok  <- nchar(code) < 5 | substr(code, 5, 5) %in% c("0", "2")
  starts250 & fifth_ok & !is.na(code) & code != ""
}

# FIX #6: Flag common T2DM comorbidities while we're scanning diagnoses
# ICD-9: Hypertension 401.xx, Hyperlipidemia 272.xx, Obesity 278.xx
is_dx_vec <- function(code, prefix) {
  code <- trimws(as.character(code))
  substr(code, 1, nchar(prefix)) == prefix & !is.na(code) & code != ""
}

cat("Flagging diagnoses and comorbidities...\n")
clm <- clm %>%
  mutate(
    diab_dx  = is_t2dm_vec(DX1) | is_t2dm_vec(DX2) | is_t2dm_vec(DX3),
    htn_dx   = is_dx_vec(DX1,"401") | is_dx_vec(DX2,"401") | is_dx_vec(DX3,"401"),
    lipid_dx = is_dx_vec(DX1,"272") | is_dx_vec(DX2,"272") | is_dx_vec(DX3,"272"),
    obese_dx = is_dx_vec(DX1,"278") | is_dx_vec(DX2,"278") | is_dx_vec(DX3,"278"),
    # FIX #2: Full OOP = coinsurance + deductible + copay
    oop_claim = rowSums(cbind(COINS, DEDUCT, COPAY), na.rm = TRUE)
  )

cat("T2DM claims:", sum(clm$diab_dx), "\n")
cat("Unique T2DM patients:", n_distinct(clm$PERSON[clm$diab_dx]), "\n\n")

# -----------------------------------------------------------------------------
# STEP 4: Merge claims + membership on PERSON
# -----------------------------------------------------------------------------
cat("Merging claims with membership...\n")
merged <- clm %>%
  inner_join(mem, by = "PERSON")

cat("Merged:", nrow(merged), "rows |",
    n_distinct(merged$PERSON), "unique patients\n")
cat("Insurance distribution:\n")
print(table(merged$INS_TYPE, useNA = "ifany"))
cat("\n")

# -----------------------------------------------------------------------------
# STEP 5: Patient-level spending + cost exposure tier
# FIX #2: OOP now includes copayments
# FIX #7: AMT_ALLOW included as a secondary cost measure
# -----------------------------------------------------------------------------
pt_spend <- merged %>%
  group_by(PERSON) %>%
  summarise(
    total_plan_pay = sum(PAY,       na.rm = TRUE),
    total_coins    = sum(COINS,     na.rm = TRUE),
    total_copay    = sum(COPAY,     na.rm = TRUE),
    total_deduct   = sum(DEDUCT,    na.rm = TRUE),
    total_allowed  = sum(AMT_ALLOW, na.rm = TRUE),  # total negotiated cost
    .groups = "drop"
  ) %>%
  mutate(
    total_oop   = total_coins + total_copay + total_deduct,  # full OOP
    total_spend = total_plan_pay + total_oop,
    cost_tier   = case_when(
      total_oop < 500  ~ "Low",
      total_oop < 3000 ~ "Medium",
      TRUE             ~ "High"
    ),
    cost_tier = factor(cost_tier, levels = c("Low", "Medium", "High"))
  )

# -----------------------------------------------------------------------------
# STEP 6: Patient-level comorbidity flags
# FIX #6: Flag if patient had any claim with these comorbidities in the year
# -----------------------------------------------------------------------------
pt_comorbid <- merged %>%
  group_by(PERSON) %>%
  summarise(
    has_htn   = any(htn_dx,   na.rm = TRUE),
    has_lipid = any(lipid_dx, na.rm = TRUE),
    has_obese = any(obese_dx, na.rm = TRUE),
    comorbid_count = as.integer(any(htn_dx)) +
                     as.integer(any(lipid_dx)) +
                     as.integer(any(obese_dx)),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# STEP 7: Build care funnel stages
#
# Stage 1 = Diagnosed with T2DM
# Stage 2 = DM management/lab CPT claim 30+ days after first diagnosis
# Stage 3 = Any follow-up claim 90+ days after treatment
# FIX #3: Expanded CPT code list to capture more legitimate visits
# -----------------------------------------------------------------------------

# 7a. First diagnosis date per patient
dx_pts <- merged %>%
  filter(diab_dx) %>%
  group_by(PERSON) %>%
  summarise(first_dx_dt = min(SVC_DATE, na.rm = TRUE), .groups = "drop") %>%
  filter(is.finite(first_dx_dt))   # remove any Inf from all-NA groups

cat("Stage 1 — Diagnosed patients:", nrow(dx_pts), "\n")

# 7b. FIX #3: Expanded treatment CPT code list
tx_codes <- c(
  # Established & new patient office visits
  "99201","99202","99203","99204","99205",
  "99211","99212","99213","99214","99215",
  # HbA1c and glucose labs
  "83036","83037","82951","82950",
  # Diabetes self-management education
  "G0108","G0109","G0270","98960",
  # Telehealth E&M
  "99441","99442","99443",
  # Outpatient consultations
  "99241","99242","99243","99244","99245"
)

tx_claims <- merged %>%
  filter(PROC1 %in% tx_codes)

# 7c. First treatment >= 30 days after diagnosis
first_tx <- tx_claims %>%
  inner_join(dx_pts, by = "PERSON") %>%
  filter(SVC_DATE >= first_dx_dt + 30) %>%
  group_by(PERSON) %>%
  summarise(first_tx_dt = min(SVC_DATE, na.rm = TRUE), .groups = "drop")

cat("Stage 2 — Treated patients:", nrow(first_tx), "\n")

# 7d. Follow-up: any claim 90+ days after treatment
followup <- merged %>%
  inner_join(first_tx, by = "PERSON") %>%
  filter(SVC_DATE >= first_tx_dt + 90) %>%
  distinct(PERSON) %>%
  mutate(has_fu = TRUE)

cat("Stage 3 — Follow-up patients:", nrow(followup), "\n\n")

# -----------------------------------------------------------------------------
# STEP 8: Master patient-level analytic file
# One row per T2DM patient with all flags and demographics
# FIX #5: Include HHINCOMEFPL for income-based analysis
# -----------------------------------------------------------------------------
pt_analytic <- dx_pts %>%
  left_join(
    mem %>% distinct(PERSON, INS_TYPE, REGION, AGE_CAT,
                     FEMALE, NATION_WGT, BENE_STATE,
                     HH_INCOME, HHINCOMEFPL),   # FIX #5: add income variables
    by = "PERSON"
  ) %>%
  left_join(pt_spend   %>% select(PERSON, cost_tier, total_oop,
                                  total_spend, total_allowed),
            by = "PERSON") %>%
  left_join(pt_comorbid, by = "PERSON") %>%    # FIX #6: comorbidities
  left_join(first_tx %>% mutate(has_tx = TRUE) %>% select(PERSON, has_tx),
            by = "PERSON") %>%
  left_join(followup,   by = "PERSON") %>%
  mutate(
    has_dx = TRUE,
    has_tx = replace_na(has_tx, FALSE),
    has_fu = replace_na(has_fu, FALSE),
    # FIX #5: Income category using Federal Poverty Level
    income_cat = case_when(
      HHINCOMEFPL < 1.38 ~ "Below 138% FPL",
      HHINCOMEFPL < 2.50 ~ "138-250% FPL",
      HHINCOMEFPL < 4.00 ~ "250-400% FPL",
      TRUE               ~ "Above 400% FPL"
    ),
    income_cat = factor(income_cat, levels = c(
      "Below 138% FPL","138-250% FPL","250-400% FPL","Above 400% FPL"
    ))
  )

cat("Analytic file:", nrow(pt_analytic), "patients\n")
cat("Treatment rate:",  round(mean(pt_analytic$has_tx)*100, 1), "%\n")
cat("Follow-up rate:",  round(mean(pt_analytic$has_fu)*100, 1), "%\n\n")

# =============================================================================
# EXHIBIT GENERATION
# FIX #4: All rates weighted by NATION_WGT for national representativeness
# =============================================================================

# Helper: weighted percentage
wpct <- function(flag, wt) {
  round(100 * weighted.mean(flag, wt, na.rm = TRUE), 1)
}

# ── EXHIBIT 1: Overall Care Funnel ──────────────────────────────────────────
n_total <- nrow(pt_analytic)
n_tx    <- sum(pt_analytic$has_tx)
n_fu    <- sum(pt_analytic$has_fu)

# Weighted rates (FIX #4)
w_tx <- wpct(pt_analytic$has_tx, pt_analytic$NATION_WGT)
w_fu <- wpct(pt_analytic$has_fu, pt_analytic$NATION_WGT)

exhibit1 <- tibble(
  Stage        = c("Stage 1: Initial T2DM Diagnosis",
                   "Stage 2: Received Treatment",
                   "Stage 3: Had Follow-Up Care"),
  N_Patients   = c(n_total, n_tx, n_fu),
  Pct_Unwtd    = c(100,
                   round(100 * n_tx / n_total, 1),
                   round(100 * n_fu / n_total, 1)),
  Pct_Weighted = c(100, w_tx, w_fu)   # nationally representative
)

cat("=== EXHIBIT 1: Overall Care Funnel ===\n")
print(exhibit1)

# ── EXHIBIT 2: By Cost Exposure Tier ────────────────────────────────────────
exhibit2 <- pt_analytic %>%
  filter(!is.na(cost_tier)) %>%
  group_by(cost_tier) %>%
  summarise(
    N_Diagnosed   = n(),
    Pct_Treatment = wpct(has_tx, NATION_WGT),
    Pct_Followup  = wpct(has_fu, NATION_WGT),
    Dropoff_Pct   = round(100 - wpct(has_fu, NATION_WGT), 1),
    Avg_OOP       = round(weighted.mean(total_oop, NATION_WGT, na.rm=TRUE), 0),
    .groups = "drop"
  )

cat("\n=== EXHIBIT 2: By Cost Exposure Tier ===\n")
print(exhibit2)

# ── EXHIBIT 3: By Insurance Type ────────────────────────────────────────────
exhibit3 <- pt_analytic %>%
  filter(!is.na(INS_TYPE), INS_TYPE != "") %>%
  group_by(INS_TYPE) %>%
  summarise(
    N_Diagnosed   = n(),
    Pct_Treatment = wpct(has_tx, NATION_WGT),
    Pct_Followup  = wpct(has_fu, NATION_WGT),
    Dropoff_Pct   = round(100 - wpct(has_fu, NATION_WGT), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(Pct_Treatment))

cat("\n=== EXHIBIT 3: By Insurance Type ===\n")
print(exhibit3)

# Chart — Exhibit 3
ex3_long <- exhibit3 %>%
  select(INS_TYPE, Pct_Treatment, Pct_Followup) %>%
  pivot_longer(cols = c(Pct_Treatment, Pct_Followup),
               names_to = "Stage", values_to = "Percent") %>%
  mutate(Stage = recode(Stage,
                        Pct_Treatment = "% Treated",
                        Pct_Followup  = "% Follow-Up"))

p3 <- ggplot(ex3_long,
             aes(x = reorder(INS_TYPE, -Percent), y = Percent, fill = Stage)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c("% Treated" = "#1B4F72", "% Follow-Up" = "#5D9CCC")) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title    = "Exhibit 3. Treatment and Follow-Up Rates by Insurance Type",
       subtitle = "2014 Uni_USA Synthetic Claims | T2DM Patients | Population-Weighted",
       x = "Insurance Type", y = "% of Diagnosed Patients", fill = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

print(p3)
ggsave("exhibit3_insurance.png", p3, width = 8, height = 5, dpi = 150)
cat("Exhibit 3 saved.\n")

# ── EXHIBIT 4: By Age Category and Region ───────────────────────────────────
ex4_age <- pt_analytic %>%
  filter(!is.na(AGE_CAT), AGE_CAT != "") %>%
  group_by(Subgroup = AGE_CAT) %>%
  summarise(
    Category    = "Age Group",
    N           = n(),
    Pct_Tx      = wpct(has_tx, NATION_WGT),
    Pct_FU      = wpct(has_fu, NATION_WGT),
    Dropoff_Pct = round(100 - wpct(has_fu, NATION_WGT), 1),
    .groups     = "drop"
  )

ex4_reg <- pt_analytic %>%
  filter(!is.na(REGION), REGION != "") %>%
  group_by(Subgroup = REGION) %>%
  summarise(
    Category    = "Region",
    N           = n(),
    Pct_Tx      = wpct(has_tx, NATION_WGT),
    Pct_FU      = wpct(has_fu, NATION_WGT),
    Dropoff_Pct = round(100 - wpct(has_fu, NATION_WGT), 1),
    .groups     = "drop"
  )

exhibit4 <- bind_rows(ex4_age, ex4_reg) %>%
  select(Category, Subgroup, N, Pct_Tx, Pct_FU, Dropoff_Pct)

cat("\n=== EXHIBIT 4: By Age Category and Region ===\n")
print(exhibit4)

# ── BONUS EXHIBIT 5: By Income Category (FIX #5) ───────────────────────────
exhibit5 <- pt_analytic %>%
  filter(!is.na(income_cat)) %>%
  group_by(income_cat) %>%
  summarise(
    N_Diagnosed   = n(),
    Pct_Treatment = wpct(has_tx, NATION_WGT),
    Pct_Followup  = wpct(has_fu, NATION_WGT),
    Dropoff_Pct   = round(100 - wpct(has_fu, NATION_WGT), 1),
    .groups = "drop"
  )

cat("\n=== EXHIBIT 5: By Income Level (% FPL) ===\n")
print(exhibit5)

# ── BONUS EXHIBIT 6: By Comorbidity Count (FIX #6) ─────────────────────────
exhibit6 <- pt_analytic %>%
  filter(!is.na(comorbid_count)) %>%
  group_by(Comorbidities = comorbid_count) %>%
  summarise(
    N_Diagnosed   = n(),
    Pct_Treatment = wpct(has_tx, NATION_WGT),
    Pct_Followup  = wpct(has_fu, NATION_WGT),
    Dropoff_Pct   = round(100 - wpct(has_fu, NATION_WGT), 1),
    .groups = "drop"
  )

cat("\n=== EXHIBIT 6: By Number of Comorbidities (HTN / Lipids / Obesity) ===\n")
print(exhibit6)

# ── Export all exhibits to CSV ───────────────────────────────────────────────
write.csv(exhibit1, "exhibit1_funnel.csv",     row.names = FALSE)
write.csv(exhibit2, "exhibit2_cost.csv",       row.names = FALSE)
write.csv(exhibit3, "exhibit3_insurance.csv",  row.names = FALSE)
write.csv(exhibit4, "exhibit4_age_region.csv", row.names = FALSE)
write.csv(exhibit5, "exhibit5_income.csv",     row.names = FALSE)
write.csv(exhibit6, "exhibit6_comorbid.csv",   row.names = FALSE)

cat("\nAll exhibits exported to:", getwd(), "\n")
cat("\n=== ANALYSIS COMPLETE ===\n")
