# Methods of Payment in M&A Transactions

## Overview

This project examines whether the method of payment in mergers and acquisitions is associated with the acquiring firm’s announcement-period stock return.

The analysis focuses on the relationship between all-stock transactions and the bidder’s cumulative abnormal return, with separate specifications for public and private target companies.

## Research Question

Does the use of stock as the method of payment have a different association with bidder announcement returns when the acquisition target is public rather than private?

## Methodology

The empirical analysis was conducted in R and includes:

* Descriptive analysis of M&A transactions by year
* Summary statistics for the regression variables
* Winsorisation of continuous variables at the 0.5th and 99.5th percentiles
* Separate regressions for public and private targets
* Full-sample regressions with interaction terms
* Control variables for bidder and deal characteristics
* Heteroskedasticity-robust standard errors

## Variables

The dependent variable is the bidder’s cumulative abnormal return around the transaction announcement.

The main explanatory variables are:

* An indicator for all-stock transactions
* An indicator for public targets
* An interaction between all-stock payment and public-target status

The extended specifications also control for:

* Relative deal size
* Bidder size
* Pre-announcement stock-price run-up
* Free cash flow
* Leverage
* Stock-return volatility
* Hostile takeover status
* Tender-offer status

## Empirical Specifications

The analysis estimates:

1. Separate bivariate regressions for public and private targets
2. A full-sample regression with an interaction between all-stock payment and public-target status
3. Separate multivariate regressions with bidder and transaction controls
4. A full-sample model allowing the effects of the control variables to differ between public and private targets

## Requirements

Install the required R packages before running the analysis:

```r
install.packages(c(
  "DescTools",
  "knitr",
  "tidyverse",
  "stargazer",
  "lmtest",
  "car",
  "vtable"
))
```

## Data Availability

The original dataset is not included in this repository.

To run the analysis, place an authorised copy of the dataset in the following location:

```text
data/CAR_MA.RData
```

The `.RData` file must contain an object named:

```text
CAR_MA
```

## Running the Analysis

Set the repository as the working project directory and run:

```r
source("Methods of Payment in M&A transactions.R")
```

The script will stop with an explanatory message when the required dataset is not available.

## Repository Structure

```text
.
├── .gitignore
├── Methods of Payment in M&A transactions.R
└── README.md

Required but not included:
data/CAR_MA.RData
```

## Tools

R, DescTools, knitr, tidyverse, stargazer, lmtest, car, and vtable.
