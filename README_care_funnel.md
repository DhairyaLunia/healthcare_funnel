# 🏥 The Patient Care Funnel: Drop-Off Across Stages of Type 2 Diabetes Care

> **Course:** MILI 6963 — Healthcare Analytics  
> **Data:** Uni_USA Synthetic Claims Files (2014)  
> **Language:** R  
> **Type:** DataWatch White Paper Analysis

---

## Overview

This project analyzes patient drop-off across three stages of Type 2 Diabetes (T2DM) care using 2014 synthetic insurance claims data. Starting from initial diagnosis, it tracks how many patients receive treatment and how many return for follow-up — then breaks that funnel down by cost exposure, insurance type, age, region, income, and comorbidities.

All rates are population-weighted using `NATION_WGT` for national representativeness.

---

## The Care Funnel

```
Stage 1 │ Initial T2DM Diagnosis
        │   ICD-9: 250.x0 / 250.x2 across any of 3 diagnosis fields
        ▼
Stage 2 │ Received Treatment
        │   Qualifying CPT claim ≥ 30 days after first diagnosis
        ▼
Stage 3 │ Had Follow-Up Care
        │   Any claim ≥ 90 days after first treatment
```

---

## Data Sources

| File | Description |
|---|---|
| `usa_mem_syn_2014_fin.sas7bdat` | Membership file — demographics, insurance type, region, income |
| `synth5_us_clm_x_2014.sas7bdat` | Claims file — diagnosis codes, CPT codes, dates, payments |

Both files are loaded via `haven::read_sas()` and joined on `PERSON`.

---

## Exhibits Produced

| # | Exhibit | Output |
|---|---|---|
| 1 | Overall care funnel (unweighted + weighted) | `exhibit1_funnel.csv` |
| 2 | Drop-off by OOP cost exposure tier | `exhibit2_cost.csv` |
| 3 | Treatment & follow-up by insurance type | `exhibit3_insurance.csv` + `exhibit3_insurance.png` |
| 4 | Drop-off by age category and region | `exhibit4_age_region.csv` |
| 5 | Drop-off by income level (% FPL) | `exhibit5_income.csv` |
| 6 | Drop-off by comorbidity count | `exhibit6_comorbid.csv` |

---

## Key Design Decisions

**ICD-9 T2DM flagging** — A claim is flagged as T2DM if any of the three diagnosis fields contains a code starting with `250` where the 5th character (if present) is `0` or `2` (type 2 / uncontrolled type 2). Vectorized for performance on 8M+ row files.

**CPT treatment codes** — Expanded list covering office visits (99201–99215), HbA1c/glucose labs (83036, 83037, 82951, 82950), diabetes self-management education (G0108, G0109, G0270, 98960), telehealth E&M (99441–99443), and outpatient consultations (99241–99245).

**Medicare date fix** — `FIRST_DOS` is `NA` for Medicare patients. Fallback uses `SAS_FIRST_DOS` (SAS numeric date, days since 1960-01-01) via `coalesce()`.

**Full OOP calculation** — Out-of-pocket = coinsurance + deductible + copayment (copayment was missing from earlier versions).

**OOP cost tiers:**
- Low: < $500
- Medium: $500–$2,999
- High: ≥ $3,000

**Income categories (% FPL):**
- Below 138% FPL
- 138–250% FPL
- 250–400% FPL
- Above 400% FPL

**Comorbidities flagged (ICD-9):**
- Hypertension: 401.xx
- Hyperlipidemia: 272.xx
- Obesity: 278.xx

---

## Requirements

```r
install.packages(c("haven", "dplyr", "ggplot2", "tidyr", "scales"))
```

| Package | Use |
|---|---|
| `haven` | Read `.sas7bdat` files |
| `dplyr` | Data wrangling, joins, summaries |
| `ggplot2` | Exhibit 3 bar chart |
| `tidyr` | `pivot_longer` for chart reshaping |
| `scales` | Axis formatting |

---

## How to Run

1. Update the file paths at the top of `care_funnel_final.R`:
   ```r
   MEM_PATH <- "path/to/usa_mem_syn_2014_fin.sas7bdat"
   CLM_PATH <- "path/to/synth5_us_clm_x_2014.sas7bdat"
   ```
2. Run end-to-end in RStudio or via terminal:
   ```bash
   Rscript care_funnel_final.R
   ```
3. All 6 exhibit CSVs and the Exhibit 3 PNG will be saved to your working directory.

---

## Project Structure

```
├── care_funnel_final.R          # Main analysis script
├── exhibit1_funnel.csv          # Overall funnel table
├── exhibit2_cost.csv            # By OOP cost tier
├── exhibit3_insurance.csv       # By insurance type
├── exhibit3_insurance.png       # Bar chart — insurance type
├── exhibit4_age_region.csv      # By age & region
├── exhibit5_income.csv          # By income (% FPL)
└── exhibit6_comorbid.csv        # By comorbidity count
```

---

## Notes

- All percentages are **population-weighted** via `NATION_WGT` to represent the national insured population.
- The dataset is synthetic; results are not intended for clinical or policy inference.
- `AMT_ALLOWED` (total negotiated cost) is retained as a secondary cost measure alongside plan payments and OOP.
