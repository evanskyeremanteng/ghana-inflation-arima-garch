# Modelling and Forecasting of Monthly Inflation Rate in Ghana (ARIMA-GARCH)

Undergraduate thesis, BSc Mathematics (Applied Mathematics), Kwame Nkrumah University of Science and Technology (KNUST). Supervised by Dr. Maxwell Akwasi Boateng.

## Overview

This project models and forecasts Ghana's monthly inflation rate using ARIMA-GARCH models in R. The data are split into three periods so that the effect of COVID-19 on inflation can be examined:

| Period | Years |
|---|---|
| Pre-COVID-19 | 2017 - 2018 |
| During COVID-19 | 2019 - 2020 |
| Post-COVID-19 | 2021 - 2024 |

## Data

- **Series:** Ghana monthly inflation rate, 2017 - 2024
- **Source:** Ghana Statistical Service

## Methods

1. Exploratory analysis of trend and stationarity for each period
2. ARIMA model identification and fitting (`forecast` package)
3. ARCH-LM test for ARCH effects in the residuals
4. GARCH modelling where appropriate (`rugarch` package)
5. 12-month-ahead forecasts of monthly inflation

## Key Findings

- The ARCH-LM test showed **no ARCH effect** in the data.
- **No single model** described inflation across all three periods; different models fitted the pre-, during and post-COVID-19 periods.
- COVID-19 affected the behaviour of inflation over the years studied.
-Model forecasted inflation accurately within 90% confidence level 

## Repository Structure

```
.
├── README.md
├── code/       R scripts 
├── thesis/     Thesis document (PDF)
└── figures/    Key plots
```

## How to Run

Requires R and the packages `forecast`, `rugarch` and `tseries`.

```
install.packages(c("forecast", "rugarch", "tseries"))
```

## Author

Evans Kyeremanteng - Department of Mathematics, KNUST
Email: sandokyeremanteng@gmail.com
