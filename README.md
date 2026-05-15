# 🛒 Retail Sales Analytics

> **Professional retail sales data analysis in R** — from transaction-level data to strategic business insights.

---

## 📋 About the Project

This project covers the complete analytical processing of retail sales data using R. Based on approximately 10 million transaction records, it delivers descriptive analysis, anomaly detection, forecasting, and actionable business recommendations.

**Author:** Ramazan Mahammad  
**Date:** February 2026  
**Language:** R (RStudio)

---

## 🗂️ Project Structure

```
retail-sales-analytics/
│
├── DA Yakun Layiha MahammadR 20260215.Rmd   # Main analysis file (R Markdown)
├── DA Yakun Layiha MahammadR20260215.pptx   # Presentation slides
├── DA-Yakun-Layiha-MahammadR-20260215.html  # HTML output report
├── DataCleaning.R                           # Data cleaning script
├── map_sample.csv                           # fp_map dataset sample
├── receipts_sample.csv                      # fp_receipts dataset sample
└── README.md
```

---

## 📦 Libraries Used

```r
library(ggplot2)      # Data visualization
library(data.table)   # Fast data processing
library(dplyr)        # Data manipulation
library(readr)        # CSV reading
library(readxl)       # Excel files
library(lubridate)    # Date operations
library(xlsx)         # Excel writing
library(plotly)       # Interactive charts
library(kableExtra)   # Table formatting
library(DT)           # Interactive tables
library(stringr)      # String processing
library(scales)       # Axis formatting
library(tidyr)        # Data reshaping
```

---

## 🗄️ Data Sources

### `fp_receipts`
Transaction-level sales data (~10 million observations).

| Variable | Description |
|---|---|
| `pos` | Point of sale |
| `rec_date` | Date |
| `rec_id` | Receipt ID |
| `product_name` | Product name |
| `qnt` | Quantity |
| `unit_price` | Unit price |

### `fp_map`
Additional attributes by product and time period (~118,000 observations).

| Variable | Description |
|---|---|
| `period` | Time period |
| `product_type` | Product type |
| `product` | Product |
| `profit_margin` | Profit margin |

---

## 🔗 Join Operation

The `fp_receipts` and `fp_map` datasets were joined on product + period keys.

| Metric | Value |
|---|---|
| Initial observations (fp_receipts) | 10,000,909 |
| After join (joined_data) | 9,812,609 |
| Unmatched observations | ~188,000 |

> ⚠️ Approximately 188,000 observations were excluded because no matching key was found in `fp_map`, indicating that some product–period combinations are absent from the mapping data.

---

## 📊 Report Sections

### 1. Descriptive Analysis
- Statistical summary of the dataset (`summary`)
- Distribution of key quantitative variables
- Sales structure by day of week (Friday: 3.36M observations)

### 2. Anomalies
- Outlier detection in `qnt`, `unit_price`, and `Total`
- Identification of right-skewed distributions
- NA analysis on `profit_margin` (9.37M observations are NA)

### 3. Forecasting & Hypotheses
- Sales variability over time
- Regression model: `log(Total)` ~ `profit_margin`
  - Negative relationship identified (statistically significant)
  - Low explanatory power: **R² ≈ 0.018**

### 4. Summary
- High sales volume does not necessarily yield high margin
- Sales behavior depends on temporal and structural factors
- Additional variables are needed for profitability analysis

---

## 🔍 Key Findings

```
📅 Period covered:         19.04.2024 – 31.05.2024
📦 Total observations:     9,812,609
📈 Median sales amount:    6.18
📈 Mean sales amount:      13.21
📉 Median profit margin:   0.20
⚠️  Margin NA rate:        ~95.5%
```

---

## ▶️ Getting Started

1. Clone this repository:
   ```bash
   git clone https://github.com/ramazanlimahammadd/retail-sales-analytics.git
   ```

2. Open `DA Yakun Layiha MahammadR 20260215.Rmd` in RStudio.

3. Install required packages:
   ```r
   install.packages(c("ggplot2", "data.table", "dplyr", "readr",
                       "readxl", "lubridate", "plotly", "kableExtra",
                       "DT", "stringr", "scales", "tidyr"))
   ```

4. Run `DataCleaning.R` first, then knit the `.Rmd` file.

> 📌 **Note:** Configure the `RMariaDB` database connection settings to match your own environment.

---

## 📌 Recommendations

- Completing the `fp_map` dataset is recommended to enable full profit margin analysis
- A time series model (ARIMA / Prophet) could forecast future weekly sales
- Segmentation by point of sale (POS) could generate additional analytical value

---

## 📄 License

This project was developed for academic and research purposes.
