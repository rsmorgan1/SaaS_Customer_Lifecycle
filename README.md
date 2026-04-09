# SaaS Customer Lifecycle & Revenue Intelligence

**Cross-system analysis of customer health, revenue dynamics, and churn drivers using five operational datasets.**

Built to demonstrate the analytical workflow of a BI & AI Analyst on a SaaS Finance team — joining data across billing, CRM, product telemetry, and support systems to surface actionable insights for executive decision-making.

---

## The Business Question

A SaaS company has customer data scattered across five operational systems. Leadership needs a unified view to answer: *Where is revenue growing? Where is it leaking? Which customers are at risk — and what can we do about it?*

## Datasets

| Table | Rows | Simulates | Key Fields |
|-------|------|-----------|------------|
| `ravenstack_accounts` | 500 | CRM | account_id, industry, plan_tier, signup_date, churn_flag |
| `ravenstack_subscriptions` | 5,000 | Billing / ERP | subscription_id, MRR, ARR, upgrade/downgrade/churn flags |
| `ravenstack_churn_events` | 600 | CRM | reason_code, refund_amount, feedback_text |
| `ravenstack_feature_usage` | 25,000 | Product Telemetry | feature_name, usage_count, usage_duration, error_count |
| `ravenstack_support_tickets` | 2,000 | Support Platform | priority, resolution_time, satisfaction_score, escalation_flag |

**Data relationship path:**

```
accounts ──┬── subscriptions ── feature_usage
           ├── churn_events
           └── support_tickets
```

## Key Analyses

### 1. MRR Movement Waterfall
Decomposes monthly recurring revenue changes into new, expansion, contraction, and churned MRR — the most important chart for a SaaS CFO.

### 2. Cohort Retention Heatmap
Groups accounts by signup month and tracks retention over 18 months. Reveals whether the product is getting stickier over time and identifies problematic cohorts.

### 3. Churn Driver Analysis
Joins churn events with feature usage and support ticket history to identify the behavioral signals that precede churn — moving beyond surface-level reason codes.

### 4. Churn Risk Scoring Model
A weighted composite score using six signals (feature adoption, error rates, ticket volume, escalations, auto-renew status, tenure) to identify at-risk accounts before they leave. Includes model validation and a high-risk account watchlist.

### 5. Feature Usage Impact on Retention
Correlates feature adoption breadth and depth with retention rates. Identifies which specific features are most predictive of long-term customers vs. churners.

### 6. Revenue Segmentation
Analyzes MRR concentration across plan tiers and industry verticals to assess revenue risk.

## Key Findings

- **Feature adoption breadth is the single strongest predictor of retention.** Accounts using 10+ features churn at dramatically lower rates than those using fewer than 5.
- **MRR at risk is concentrated** in a small number of high-value Enterprise accounts with declining usage and elevated support volume.
- **Mid-2024 signup cohorts** show weaker retention than earlier cohorts, warranting investigation into onboarding or product changes.
- **Churn is predictable** — the combination of low feature adoption, high error rates, and unresolved escalations identifies approximately 70% of eventual churners.

## Tools & Technologies

- **R** — tidyverse, ggplot2, lubridate, scales, knitr, kableExtra
- **Tableau Public** — Interactive dashboard version with cross-filtering
- **Analytical techniques** — Cohort analysis, waterfall decomposition, risk scoring, LOD calculations, multi-table joins

## How to Run

1. Clone this repository
2. Open `Project1_SaaS_Customer_Lifecycle.Rmd` in RStudio
3. Install dependencies: `install.packages(c("tidyverse", "lubridate", "scales", "knitr", "kableExtra"))`
4. Knit to HTML

The notebook expects the Ravenstack CSV files in a `../RapidSOS/` directory relative to the `.Rmd` file.

## Project Structure

```
.
├── README.md
├── Project1_SaaS_Customer_Lifecycle.Rmd    # Full R Markdown analysis
└── data/
    ├── ravenstack_accounts.csv
    ├── ravenstack_subscriptions.csv
    ├── ravenstack_churn_events.csv
    ├── ravenstack_feature_usage.csv
    └── ravenstack_support_tickets.csv
```

---

*Analysis by Russell Morgan | April 2026*
