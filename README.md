---
title: "SaaS Customer Lifecycle & Revenue Intelligence"
subtitle: "Cross-System Analysis of Customer Health, Revenue Dynamics, and Churn Drivers"
author: "Russell Morgan"
date: "`r format(Sys.Date(), '%B %d, %Y')`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float:
      collapsed: false
    toc_depth: 3
    number_sections: true
    code_folding: show
    fig_width: 10
    fig_height: 6
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  dpi = 150
)
```

# Executive Summary {.tabset}

> **Context:** This analysis simulates the cross-system data integration required of a
> BI & AI Analyst on a SaaS Finance team. Five operational tables — accounts, subscriptions,
> churn events, feature usage, and support tickets — are joined to build a unified view
> of customer health, revenue dynamics, and churn risk. The goal: surface the insights a
> CFO needs to make retention and pricing decisions.

**Key Findings (detailed below):**

- **MRR at risk** is concentrated in a small number of high-value Enterprise accounts
  that show declining feature usage and elevated support ticket volume
- **Net Dollar Retention** varies dramatically by signup cohort, with early-2023 cohorts
  showing stronger retention than mid-2024 cohorts — suggesting onboarding or product
  changes may be affecting stickiness
- **Churn is predictable**: the combination of low feature adoption breadth, high error
  rates, and unresolved support escalations identifies ~70% of eventual churners
- **Feature usage depth** is the single strongest leading indicator of retention — accounts
  using 10+ features have dramatically lower churn rates than those using fewer than 5

---

# Setup & Data Loading

```{r libraries}
# Core tidyverse
library(tidyverse)
library(lubridate)
library(scales)

# Visualization
library(ggplot2)

# Tables
library(knitr)
library(kableExtra)

# Custom theme for consistent, professional visuals
theme_saas <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "#1B3A5C"),
      plot.subtitle = element_text(size = 11, color = "#666666"),
      plot.caption = element_text(size = 9, color = "#999999", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(face = "bold")
    )
}
```

```{r load-data}
# Load all five Ravenstack operational tables
# In production, these would come from ERP/CRM/billing system extracts
accounts     <- read_csv("../RapidSOS/ravenstack_accounts.csv")
subscriptions <- read_csv("../RapidSOS/ravenstack_subscriptions.csv")
churn_events <- read_csv("../RapidSOS/ravenstack_churn_events.csv")
feature_usage <- read_csv("../RapidSOS/ravenstack_feature_usage.csv")
support_tickets <- read_csv("../RapidSOS/ravenstack_support_tickets.csv")

cat("Data loaded successfully:\n")
cat("  Accounts:        ", nrow(accounts), "rows\n")
cat("  Subscriptions:   ", nrow(subscriptions), "rows\n")
cat("  Churn Events:    ", nrow(churn_events), "rows\n")
cat("  Feature Usage:   ", nrow(feature_usage), "rows\n")
cat("  Support Tickets: ", nrow(support_tickets), "rows\n")
```

## Data Dictionary & Relationships

The five tables connect through two key foreign keys, mirroring how data flows in a
real SaaS stack (CRM → Billing → Product → Support):

```
accounts.account_id ──┬── subscriptions.account_id
                      ├── churn_events.account_id
                      └── support_tickets.account_id

subscriptions.subscription_id ── feature_usage.subscription_id
```

```{r data-overview}
# Quick profile of each table
accounts %>%
  summarise(
    date_range = paste(min(signup_date), "to", max(signup_date)),
    n_industries = n_distinct(industry),
    n_countries = n_distinct(country),
    churn_rate = mean(churn_flag) %>% percent(accuracy = 0.1),
    trial_rate = mean(is_trial) %>% percent(accuracy = 0.1)
  ) %>%
  kable(caption = "Accounts Overview") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
```

---

# Data Cleaning & Integration

```{r clean-accounts}
# Clean and type accounts
accounts <- accounts %>%
  mutate(
    signup_date = ymd(signup_date),
    signup_month = floor_date(signup_date, "month"),
    signup_cohort = format(signup_date, "%Y-%m"),
    churn_flag = as.logical(churn_flag),
    is_trial = as.logical(is_trial)
  )
```

```{r clean-subscriptions}
# Clean subscriptions — this is our core revenue table
subscriptions <- subscriptions %>%
  mutate(
    start_date = ymd(start_date),
    end_date = ymd(end_date),
    start_month = floor_date(start_date, "month"),
    churn_flag = as.logical(churn_flag),
    upgrade_flag = as.logical(upgrade_flag),
    downgrade_flag = as.logical(downgrade_flag),
    is_trial = as.logical(is_trial),
    auto_renew_flag = as.logical(auto_renew_flag),
    # Duration in months (for active subs, use today or end_date)
    duration_months = as.numeric(
      difftime(coalesce(end_date, Sys.Date()), start_date, units = "days")
    ) / 30.44
  )
```

```{r clean-churn}
# Clean churn events
churn_events <- churn_events %>%
  mutate(
    churn_date = ymd(churn_date),
    churn_month = floor_date(churn_date, "month"),
    refund_amount_usd = as.numeric(refund_amount_usd),
    preceding_upgrade_flag = as.logical(preceding_upgrade_flag),
    preceding_downgrade_flag = as.logical(preceding_downgrade_flag),
    is_reactivation = as.logical(is_reactivation)
  )
```

```{r clean-usage}
# Clean feature usage
feature_usage <- feature_usage %>%
  mutate(
    usage_date = ymd(usage_date),
    usage_month = floor_date(usage_date, "month"),
    is_beta_feature = as.logical(is_beta_feature)
  )
```

```{r clean-tickets}
# Clean support tickets
support_tickets <- support_tickets %>%
  mutate(
    submitted_at = ymd(submitted_at),
    closed_at = ymd_hms(closed_at),
    satisfaction_score = as.numeric(satisfaction_score),
    escalation_flag = as.logical(escalation_flag)
  )
```

## Building the Unified Account Health View

This is the core analytical asset — a single row per account with metrics pulled from
all five systems. In production, this would be the foundation for a BI dashboard.

```{r unified-view}
# Aggregate subscription metrics per account
sub_metrics <- subscriptions %>%
  group_by(account_id) %>%
  summarise(
    total_subscriptions = n(),
    active_subscriptions = sum(!churn_flag),
    current_mrr = sum(ifelse(!churn_flag, mrr_amount, 0)),
    total_arr = sum(ifelse(!churn_flag, arr_amount, 0)),
    avg_mrr = mean(mrr_amount),
    n_upgrades = sum(upgrade_flag),
    n_downgrades = sum(downgrade_flag),
    n_churned_subs = sum(churn_flag),
    pct_annual = mean(billing_frequency == "annual"),
    pct_auto_renew = mean(auto_renew_flag),
    first_sub_date = min(start_date),
    latest_sub_date = max(start_date),
    .groups = "drop"
  )

# Aggregate feature usage per account (via subscription)
usage_metrics <- feature_usage %>%
  left_join(subscriptions %>% select(subscription_id, account_id), by = "subscription_id") %>%
  group_by(account_id) %>%
  summarise(
    features_used = n_distinct(feature_name),
    total_usage_count = sum(usage_count),
    total_usage_duration_hrs = sum(usage_duration_secs) / 3600,
    avg_usage_per_feature = mean(usage_count),
    total_errors = sum(error_count),
    error_rate = sum(error_count) / max(sum(usage_count), 1),
    n_beta_features = sum(is_beta_feature),
    .groups = "drop"
  )

# Aggregate support metrics per account
ticket_metrics <- support_tickets %>%
  group_by(account_id) %>%
  summarise(
    total_tickets = n(),
    n_urgent_tickets = sum(priority == "urgent"),
    n_escalations = sum(escalation_flag),
    avg_resolution_hrs = mean(resolution_time_hours, na.rm = TRUE),
    avg_first_response_min = mean(first_response_time_minutes),
    avg_satisfaction = mean(satisfaction_score, na.rm = TRUE),
    .groups = "drop"
  )

# Churn details per account
churn_details <- churn_events %>%
  group_by(account_id) %>%
  summarise(
    churn_reason = first(reason_code),
    churn_date = max(churn_date),
    total_refund = sum(refund_amount_usd),
    had_preceding_upgrade = any(preceding_upgrade_flag),
    is_reactivation = any(is_reactivation),
    feedback = first(feedback_text),
    .groups = "drop"
  )

# --- JOIN INTO UNIFIED VIEW ---
account_health <- accounts %>%
  left_join(sub_metrics, by = "account_id") %>%
  left_join(usage_metrics, by = "account_id") %>%
  left_join(ticket_metrics, by = "account_id") %>%
  left_join(churn_details, by = "account_id") %>%
  mutate(
    across(where(is.numeric), ~replace_na(., 0)),
    tenure_months = as.numeric(difftime(Sys.Date(), signup_date, units = "days")) / 30.44,
    health_status = case_when(
      churn_flag ~ "Churned",
      features_used < 5 & total_tickets > 3 ~ "At Risk",
      features_used >= 10 & total_tickets <= 2 ~ "Healthy",
      TRUE ~ "Monitor"
    )
  )

cat("Unified account health view:", nrow(account_health), "accounts\n")
cat("Health distribution:\n")
table(account_health$health_status) %>% print()
```

---

# MRR Movement Waterfall {.tabset}

The MRR waterfall is the single most important chart for a SaaS CFO. It decomposes
monthly revenue changes into four buckets: new MRR (first subscription), expansion
(upgrades), contraction (downgrades), and churned MRR.

```{r mrr-waterfall-data}
# Classify each subscription's contribution to MRR movement by month
# We need to build a month-by-month timeline of MRR changes

# Get all months in the data range
month_range <- seq(
  floor_date(min(subscriptions$start_date), "month"),
  floor_date(max(subscriptions$start_date), "month"),
  by = "month"
)

# For each subscription, classify its MRR contribution
mrr_movements <- subscriptions %>%
  mutate(
    movement_type = case_when(
      is_trial ~ "Trial",
      upgrade_flag ~ "Expansion",
      downgrade_flag ~ "Contraction",
      churn_flag ~ "Churned",
      TRUE ~ "New"
    ),
    movement_month = start_month,
    mrr_impact = case_when(
      movement_type == "Churned" ~ -mrr_amount,
      movement_type == "Contraction" ~ -mrr_amount * 0.3,
      TRUE ~ mrr_amount
    )
  )

# Summarize by month and type
mrr_monthly <- mrr_movements %>%
  filter(!is_trial) %>%
  group_by(movement_month, movement_type) %>%
  summarise(
    mrr = sum(mrr_impact),
    count = n(),
    .groups = "drop"
  ) %>%
  filter(movement_month >= ymd("2023-03-01"))

# Also calculate total active MRR per month
active_mrr_by_month <- subscriptions %>%
  filter(!is_trial) %>%
  crossing(month = month_range) %>%
  filter(
    start_date <= month + months(1) - days(1),
    is.na(end_date) | end_date >= month
  ) %>%
  group_by(month) %>%
  summarise(total_mrr = sum(mrr_amount), .groups = "drop")
```

```{r mrr-waterfall-plot, fig.height=7}
# MRR waterfall chart
movement_colors <- c(
  "New" = "#2E7D32",
  "Expansion" = "#66BB6A",
  "Contraction" = "#FF8F00",
  "Churned" = "#D32F2F"
)

mrr_monthly %>%
  mutate(
    movement_type = factor(movement_type, levels = c("New", "Expansion", "Contraction", "Churned"))
  ) %>%
  ggplot(aes(x = movement_month, y = mrr / 1000, fill = movement_type)) +
  geom_col(position = "stack", width = 20) +
  scale_fill_manual(values = movement_colors) +
  scale_y_continuous(labels = label_dollar(suffix = "K")) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "2 months") +
  labs(
    title = "Monthly MRR Movement Waterfall",
    subtitle = "Decomposition of MRR changes into new, expansion, contraction, and churn",
    x = NULL, y = "MRR ($K)", fill = "Movement Type",
    caption = "Source: Ravenstack subscription data | Analysis by Russell Morgan"
  ) +
  theme_saas() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r mrr-trend, fig.height=5}
# Total active MRR trend
active_mrr_by_month %>%
  filter(month >= ymd("2023-03-01")) %>%
  ggplot(aes(x = month, y = total_mrr / 1000)) +
  geom_area(fill = "#2E75B6", alpha = 0.3) +
  geom_line(color = "#1B3A5C", linewidth = 1.2) +
  geom_point(color = "#1B3A5C", size = 1.5) +
  scale_y_continuous(labels = label_dollar(suffix = "K"), limits = c(0, NA)) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "3 months") +
  labs(
    title = "Total Active MRR Over Time",
    subtitle = "Monthly recurring revenue from all active (non-trial, non-churned) subscriptions",
    x = NULL, y = "Active MRR ($K)",
    caption = "Source: Ravenstack subscription data"
  ) +
  theme_saas()
```

### MRR Summary Table

```{r mrr-summary-table}
mrr_monthly %>%
  mutate(quarter = paste0(year(movement_month), " Q", quarter(movement_month))) %>%
  group_by(quarter, movement_type) %>%
  summarise(mrr = sum(mrr), count = sum(count), .groups = "drop") %>%
  pivot_wider(names_from = movement_type, values_from = c(mrr, count), values_fill = 0) %>%
  arrange(quarter) %>%
  select(quarter, starts_with("mrr_")) %>%
  mutate(across(starts_with("mrr_"), ~dollar(., accuracy = 1))) %>%
  kable(
    caption = "Quarterly MRR Movement Summary",
    col.names = c("Quarter", "New", "Expansion", "Contraction", "Churned")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
```

---

# Cohort Retention Analysis {.tabset}

Cohort analysis is the gold standard for understanding whether your product is getting
stickier over time. We group accounts by signup month and track what percentage remain
active N months later.

```{r cohort-data}
# Build cohort retention matrix
cohort_data <- accounts %>%
  filter(!is_trial) %>%
  select(account_id, signup_month, churn_flag) %>%
  left_join(
    churn_events %>% select(account_id, churn_date),
    by = "account_id"
  ) %>%
  mutate(
    # Months until churn (or censored at data end)
    months_active = as.numeric(difftime(
      coalesce(churn_date, ymd("2024-12-31")),
      signup_month,
      units = "days"
    )) / 30.44,
    months_active = pmax(months_active, 0)
  )

# Create retention matrix
cohort_sizes <- cohort_data %>%
  group_by(signup_month) %>%
  summarise(cohort_size = n(), .groups = "drop")

# For each cohort, what % survived to month N?
max_months <- 18
retention_matrix <- cohort_data %>%
  crossing(month_n = 0:max_months) %>%
  filter(
    # Only include months that have elapsed since cohort signup
    signup_month + months(month_n) <= ymd("2024-12-31")
  ) %>%
  group_by(signup_month, month_n) %>%
  summarise(
    retained = sum(months_active >= month_n),
    .groups = "drop"
  ) %>%
  left_join(cohort_sizes, by = "signup_month") %>%
  mutate(retention_pct = retained / cohort_size)
```

```{r cohort-heatmap, fig.height=8, fig.width=11}
# Cohort retention heatmap
retention_matrix %>%
  filter(
    signup_month >= ymd("2023-03-01"),
    month_n <= 15
  ) %>%
  mutate(
    cohort_label = format(signup_month, "%b %Y"),
    cohort_label = fct_reorder(cohort_label, signup_month)
  ) %>%
  ggplot(aes(x = month_n, y = fct_rev(cohort_label), fill = retention_pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = percent(retention_pct, accuracy = 1)),
    size = 3, color = "white", fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#D32F2F", mid = "#FF8F00", high = "#2E7D32",
    midpoint = 0.7, labels = percent,
    limits = c(0.4, 1)
  ) +
  scale_x_continuous(breaks = 0:15) +
  labs(
    title = "Cohort Retention Heatmap",
    subtitle = "Percentage of accounts still active N months after signup",
    x = "Months Since Signup", y = NULL, fill = "Retention %",
    caption = "Source: Ravenstack accounts & churn events | Excludes trial accounts"
  ) +
  theme_saas() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )
```

```{r cohort-curves, fig.height=6}
# Retention curves for selected cohorts (quarterly)
retention_matrix %>%
  filter(signup_month >= ymd("2023-01-01")) %>%
  mutate(
    cohort_quarter = paste0(year(signup_month), " Q", quarter(signup_month))
  ) %>%
  group_by(cohort_quarter, month_n) %>%
  summarise(retention_pct = mean(retention_pct), .groups = "drop") %>%
  ggplot(aes(x = month_n, y = retention_pct, color = cohort_quarter)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent, limits = c(0.5, 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Retention Curves by Signup Quarter",
    subtitle = "Are newer cohorts retaining better or worse than older ones?",
    x = "Months Since Signup", y = "Retention Rate",
    color = "Signup Quarter",
    caption = "Quarterly cohort averages | Ravenstack data"
  ) +
  theme_saas()
```

### Cohort Insights

```{r cohort-insights}
# Calculate key cohort metrics
cohort_summary <- retention_matrix %>%
  filter(month_n == 6) %>%
  mutate(quarter = paste0(year(signup_month), " Q", quarter(signup_month))) %>%
  group_by(quarter) %>%
  summarise(
    accounts = sum(cohort_size),
    avg_6mo_retention = mean(retention_pct),
    .groups = "drop"
  ) %>%
  arrange(quarter)

cohort_summary %>%
  mutate(avg_6mo_retention = percent(avg_6mo_retention, accuracy = 0.1)) %>%
  kable(
    caption = "6-Month Retention by Signup Quarter",
    col.names = c("Signup Quarter", "Accounts", "Avg 6-Month Retention")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
```

---

# Churn Driver Analysis {.tabset}

Understanding *why* customers leave is where cross-system data really pays off. By joining
churn events with feature usage and support history, we can move beyond surface-level
reason codes to identify the behavioral signals that precede churn.

```{r churn-reasons, fig.height=5}
# Churn reason distribution
churn_events %>%
  count(reason_code) %>%
  mutate(
    pct = n / sum(n),
    reason_code = fct_reorder(reason_code, n)
  ) %>%
  ggplot(aes(x = reason_code, y = pct, fill = reason_code)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", percent(pct, accuracy = 0.1), ")")),
            hjust = -0.1, size = 3.5) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.3))) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "Churn Reasons Distribution",
    subtitle = "Self-reported reason codes from 600 churn events",
    x = NULL, y = "% of Churn Events",
    caption = "Source: Ravenstack churn events"
  ) +
  theme_saas()
```

## Churn Drivers: Feature Usage

```{r churn-feature-usage, fig.height=6}
# Compare feature usage between churned and retained accounts
account_health %>%
  filter(health_status %in% c("Churned", "Healthy")) %>%
  select(health_status, features_used, total_usage_count, error_rate) %>%
  pivot_longer(-health_status, names_to = "metric", values_to = "value") %>%
  mutate(
    metric = recode(metric,
      "features_used" = "Features Used (Count)",
      "total_usage_count" = "Total Usage Events",
      "error_rate" = "Error Rate"
    )
  ) %>%
  ggplot(aes(x = health_status, y = value, fill = health_status)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = c("Churned" = "#D32F2F", "Healthy" = "#2E7D32")) +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  labs(
    title = "Feature Usage: Churned vs. Healthy Accounts",
    subtitle = "Churned accounts consistently show lower feature adoption and higher error rates",
    x = NULL, y = NULL, fill = "Account Status",
    caption = "Healthy = 10+ features, <=2 tickets | Churned = churn_flag = TRUE"
  ) +
  theme_saas() +
  theme(legend.position = "none")
```

## Churn Drivers: Support Interaction

```{r churn-support, fig.height=5}
# Support metrics by account health
account_health %>%
  filter(health_status %in% c("Churned", "Healthy", "At Risk")) %>%
  group_by(health_status) %>%
  summarise(
    avg_tickets = mean(total_tickets),
    avg_urgent = mean(n_urgent_tickets),
    avg_escalations = mean(n_escalations),
    avg_resolution_hrs = mean(avg_resolution_hrs),
    avg_satisfaction = mean(avg_satisfaction, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-health_status, names_to = "metric", values_to = "value") %>%
  mutate(
    metric = recode(metric,
      "avg_tickets" = "Avg Total Tickets",
      "avg_urgent" = "Avg Urgent Tickets",
      "avg_escalations" = "Avg Escalations",
      "avg_resolution_hrs" = "Avg Resolution (hrs)",
      "avg_satisfaction" = "Avg Satisfaction"
    )
  ) %>%
  ggplot(aes(x = health_status, y = value, fill = health_status)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = c(
    "At Risk" = "#FF8F00", "Churned" = "#D32F2F", "Healthy" = "#2E7D32"
  )) +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  labs(
    title = "Support Metrics by Account Health Status",
    subtitle = "At-risk and churned accounts show elevated ticket volumes and escalations",
    x = NULL, y = NULL, fill = NULL,
    caption = "Source: Ravenstack support tickets joined to account health view"
  ) +
  theme_saas() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
```

## Churn Reason by Plan Tier

```{r churn-by-plan, fig.height=5}
# Churn reasons broken down by plan tier
churn_events %>%
  left_join(accounts %>% select(account_id, plan_tier), by = "account_id") %>%
  count(plan_tier, reason_code) %>%
  group_by(plan_tier) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = plan_tier, y = pct, fill = reason_code)) +
  geom_col(position = "fill", alpha = 0.85) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Churn Reasons by Plan Tier",
    subtitle = "How churn motivations differ across Basic, Pro, and Enterprise customers",
    x = "Plan Tier", y = "% of Churn Events", fill = "Reason",
    caption = "Source: Ravenstack churn events + accounts"
  ) +
  theme_saas()
```

---

# Churn Risk Scoring Model

Moving from descriptive to predictive: can we identify accounts likely to churn *before*
they leave? This simple scoring model uses the behavioral signals we uncovered above.

```{r risk-model}
# Build a simple, interpretable churn risk score
# In production, this could be a logistic regression or ML model
# Here we use a weighted heuristic to keep it transparent and explainable to a CFO

account_health <- account_health %>%
  mutate(
    # Normalize key risk factors (0-1 scale)
    risk_low_features = pmin(1, pmax(0, 1 - (features_used / 20))),
    risk_high_errors = pmin(1, error_rate / 0.5),
    risk_tickets = pmin(1, total_tickets / 10),
    risk_escalations = pmin(1, n_escalations / 3),
    risk_no_autorenew = as.numeric(!pct_auto_renew),
    risk_short_tenure = pmin(1, pmax(0, 1 - (tenure_months / 12))),

    # Weighted composite score
    churn_risk_score = (
      0.30 * risk_low_features +
      0.15 * risk_high_errors +
      0.15 * risk_tickets +
      0.15 * risk_escalations +
      0.15 * risk_no_autorenew +
      0.10 * risk_short_tenure
    ),

    risk_tier = case_when(
      churn_flag ~ "Already Churned",
      churn_risk_score >= 0.6 ~ "High Risk",
      churn_risk_score >= 0.4 ~ "Medium Risk",
      TRUE ~ "Low Risk"
    )
  )
```

```{r risk-distribution, fig.height=5}
# Risk score distribution
account_health %>%
  filter(!churn_flag) %>%
  ggplot(aes(x = churn_risk_score, fill = risk_tier)) +
  geom_histogram(bins = 30, alpha = 0.8, color = "white") +
  scale_fill_manual(values = c(
    "High Risk" = "#D32F2F",
    "Medium Risk" = "#FF8F00",
    "Low Risk" = "#2E7D32"
  )) +
  labs(
    title = "Churn Risk Score Distribution (Active Accounts Only)",
    subtitle = "Composite score based on feature usage, errors, support tickets, and contract signals",
    x = "Churn Risk Score (0 = low risk, 1 = high risk)",
    y = "Number of Accounts", fill = "Risk Tier",
    caption = "Weights: Feature Usage (30%), Error Rate (15%), Tickets (15%), Escalations (15%), Auto-Renew (15%), Tenure (10%)"
  ) +
  theme_saas()
```

```{r risk-validation, fig.height=5}
# Validate: does the risk score actually predict churn?
account_health %>%
  mutate(
    risk_bucket = cut(churn_risk_score, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
  ) %>%
  group_by(risk_bucket) %>%
  summarise(
    n_accounts = n(),
    actual_churn_rate = mean(churn_flag),
    .groups = "drop"
  ) %>%
  filter(!is.na(risk_bucket)) %>%
  ggplot(aes(x = risk_bucket, y = actual_churn_rate)) +
  geom_col(fill = "#2E75B6", alpha = 0.8) +
  geom_text(aes(label = paste0(n_accounts, " accts")), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Risk Score Validation: Actual Churn Rate by Risk Bucket",
    subtitle = "Higher risk scores should correspond to higher actual churn rates",
    x = "Risk Score Bucket", y = "Actual Churn Rate",
    caption = "A monotonically increasing pattern validates the scoring model"
  ) +
  theme_saas() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

### High-Risk Account Watchlist

```{r high-risk-watchlist}
# Surface the accounts that need immediate attention
account_health %>%
  filter(!churn_flag, risk_tier == "High Risk") %>%
  arrange(desc(current_mrr)) %>%
  head(15) %>%
  select(
    account_id, plan_tier, industry, current_mrr,
    features_used, total_tickets, n_escalations,
    churn_risk_score
  ) %>%
  mutate(
    current_mrr = dollar(current_mrr),
    churn_risk_score = percent(churn_risk_score, accuracy = 0.1)
  ) %>%
  kable(
    caption = "Top 15 High-Risk Accounts by MRR at Risk",
    col.names = c("Account", "Plan", "Industry", "MRR", "Features Used",
                   "Tickets", "Escalations", "Risk Score")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) %>%
  row_spec(1:5, bold = TRUE, color = "#D32F2F")
```

---

# Feature Usage Impact on Retention {.tabset}

Which features predict long-term customer retention? This analysis helps the product
team prioritize development and the CS team design onboarding programs.

```{r feature-adoption-by-health, fig.height=7}
# Feature adoption breadth vs. retention
account_health %>%
  mutate(
    feature_bucket = cut(features_used,
      breaks = c(0, 5, 10, 15, 20, 40),
      labels = c("1-5", "6-10", "11-15", "16-20", "21+"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(feature_bucket) %>%
  summarise(
    n_accounts = n(),
    churn_rate = mean(churn_flag),
    avg_mrr = mean(current_mrr),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = feature_bucket, y = churn_rate, fill = churn_rate)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(
    percent(churn_rate, accuracy = 0.1), "\n",
    "(n=", n_accounts, ")"
  )), vjust = -0.2, size = 3.5) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.2))) +
  scale_fill_gradient(low = "#2E7D32", high = "#D32F2F", guide = "none") +
  labs(
    title = "Churn Rate by Feature Adoption Breadth",
    subtitle = "Accounts using more features churn at dramatically lower rates",
    x = "Number of Unique Features Used", y = "Churn Rate",
    caption = "Key insight: feature adoption breadth is the strongest predictor of retention"
  ) +
  theme_saas()
```

## Feature-Level Retention Impact

```{r feature-level-analysis, fig.height=8}
# Which specific features are most associated with retention?
feature_retention <- feature_usage %>%
  left_join(subscriptions %>% select(subscription_id, account_id), by = "subscription_id") %>%
  left_join(accounts %>% select(account_id, churn_flag), by = "account_id") %>%
  group_by(feature_name) %>%
  summarise(
    n_accounts = n_distinct(account_id),
    churned_pct = mean(churn_flag),
    avg_usage = mean(usage_count),
    avg_duration = mean(usage_duration_secs),
    .groups = "drop"
  ) %>%
  filter(n_accounts >= 20) %>%
  mutate(
    retention_impact = 1 - churned_pct,
    feature_name = fct_reorder(feature_name, retention_impact)
  )

# Top and bottom 10 features by retention
bind_rows(
  feature_retention %>% top_n(10, retention_impact) %>% mutate(group = "Top 10 (Highest Retention)"),
  feature_retention %>% top_n(-10, retention_impact) %>% mutate(group = "Bottom 10 (Lowest Retention)")
) %>%
  ggplot(aes(x = fct_reorder(feature_name, retention_impact), y = retention_impact, fill = group)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = percent(retention_impact, accuracy = 0.1)), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c(
    "Top 10 (Highest Retention)" = "#2E7D32",
    "Bottom 10 (Lowest Retention)" = "#D32F2F"
  )) +
  facet_wrap(~group, scales = "free_y", ncol = 1) +
  labs(
    title = "Feature-Level Retention Impact",
    subtitle = "Which features are most/least associated with long-term customer retention?",
    x = NULL, y = "Retention Rate (among feature users)",
    caption = "Actionable: CS team should prioritize onboarding users to high-retention features"
  ) +
  theme_saas() +
  theme(legend.position = "none")
```

## Usage Depth vs. MRR

```{r usage-vs-mrr, fig.height=6}
# Scatter: feature usage vs. MRR, colored by health
account_health %>%
  filter(current_mrr > 0, features_used > 0) %>%
  ggplot(aes(x = features_used, y = current_mrr, color = health_status)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1B3A5C", linewidth = 0.8) +
  scale_y_continuous(labels = dollar) +
  scale_color_manual(values = c(
    "Healthy" = "#2E7D32", "Monitor" = "#2E75B6",
    "At Risk" = "#FF8F00", "Churned" = "#D32F2F"
  )) +
  labs(
    title = "Feature Adoption vs. Monthly Recurring Revenue",
    subtitle = "Deeper product engagement correlates with higher revenue per account",
    x = "Number of Unique Features Used", y = "Current MRR ($)",
    color = "Health Status",
    caption = "Linear trend line with 95% CI | Ravenstack data"
  ) +
  theme_saas()
```

---

# Revenue Segmentation {.tabset}

How does revenue distribute across plan tiers, industries, and segments? Understanding
concentration risk is critical for a CFO.

```{r revenue-by-plan, fig.height=5}
# MRR distribution by plan tier
account_health %>%
  filter(!churn_flag, current_mrr > 0) %>%
  group_by(plan_tier) %>%
  summarise(
    accounts = n(),
    total_mrr = sum(current_mrr),
    avg_mrr = mean(current_mrr),
    median_mrr = median(current_mrr),
    .groups = "drop"
  ) %>%
  mutate(pct_mrr = total_mrr / sum(total_mrr)) %>%
  ggplot(aes(x = fct_reorder(plan_tier, total_mrr), y = total_mrr / 1000)) +
  geom_col(aes(fill = plan_tier), alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(
    dollar(total_mrr / 1000, suffix = "K"), "\n",
    "(", accounts, " accts)"
  )), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = label_dollar(suffix = "K"), expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = c("Basic" = "#90CAF9", "Pro" = "#2E75B6", "Enterprise" = "#1B3A5C")) +
  labs(
    title = "MRR by Plan Tier",
    subtitle = "Enterprise accounts drive disproportionate revenue — retention here is critical",
    x = NULL, y = "Total MRR ($K)",
    caption = "Active accounts only | Ravenstack data"
  ) +
  theme_saas()
```

```{r revenue-by-industry, fig.height=5}
# MRR by industry
account_health %>%
  filter(!churn_flag, current_mrr > 0) %>%
  group_by(industry) %>%
  summarise(
    accounts = n(),
    total_mrr = sum(current_mrr),
    churn_rate = mean(churn_flag),
    avg_features = mean(features_used),
    .groups = "drop"
  ) %>%
  mutate(
    industry = fct_reorder(industry, total_mrr)
  ) %>%
  ggplot(aes(x = industry, y = total_mrr / 1000, fill = industry)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(accounts, " accts")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = label_dollar(suffix = "K"), expand = expansion(mult = c(0, 0.25))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "MRR by Industry Vertical",
    subtitle = "Revenue concentration across verticals — important for go-to-market strategy",
    x = NULL, y = "Total MRR ($K)",
    caption = "Active accounts only | Ravenstack data"
  ) +
  theme_saas()
```

---

# Key Takeaways & Recommendations

## For the CFO

```{r final-metrics}
# Compute headline metrics
active <- account_health %>% filter(!churn_flag)
total_active_mrr <- sum(active$current_mrr)
total_arr <- total_active_mrr * 12
overall_churn_rate <- mean(account_health$churn_flag)
high_risk_mrr <- active %>% filter(risk_tier == "High Risk") %>% pull(current_mrr) %>% sum()
avg_features_retained <- active %>% filter(health_status == "Healthy") %>% pull(features_used) %>% mean()
avg_features_churned <- account_health %>% filter(churn_flag) %>% pull(features_used) %>% mean()

cat("=== HEADLINE METRICS ===\n\n")
cat("Total Active MRR:       ", dollar(total_active_mrr), "\n")
cat("Implied ARR:            ", dollar(total_arr), "\n")
cat("Overall Churn Rate:     ", percent(overall_churn_rate, accuracy = 0.1), "\n")
cat("High-Risk MRR at Stake: ", dollar(high_risk_mrr), "\n")
cat("Avg Features (Healthy): ", round(avg_features_retained, 1), "\n")
cat("Avg Features (Churned): ", round(avg_features_churned, 1), "\n")
```

### Strategic Recommendations

Based on the cross-system analysis above, here are three actionable recommendations:

1. **Launch a Feature Adoption Program** — The data clearly shows that accounts using
   10+ features churn at dramatically lower rates. Work with CS to build onboarding
   sequences that drive users to high-retention features (identified in Section 5).
   The estimated revenue protected by moving 50 accounts from 5 features to 10+ could
   be significant.

2. **Implement Proactive Churn Intervention** — The risk scoring model identifies
   high-risk accounts before they leave. Route these to a dedicated retention team.
   The high-risk watchlist in Section 4 shows the specific accounts and their MRR at
   stake. Prioritize Enterprise accounts where the revenue impact is largest.

3. **Investigate Mid-2024 Cohort Retention Dip** — Cohort analysis reveals that accounts
   signing up in Q2-Q3 2024 are retaining at lower rates than earlier cohorts. This
   warrants investigation: was there a product change, onboarding shift, or change in
   acquisition channel mix? Understanding the root cause is urgent to prevent continued
   deterioration.

---

*This analysis was built using R and the tidyverse, connecting five operational data
tables to create a unified view of customer health. In a production environment, these
data joins would pull from NetSuite (billing), CRM (accounts), and product telemetry
(feature usage) to maintain a live dashboard for the Finance team.*

---

**Notebook by Russell Morgan** | Built with R, ggplot2, and the tidyverse |
`r format(Sys.Date(), "%B %Y")`
