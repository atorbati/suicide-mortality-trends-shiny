# suicide-mortality-trends-shiny
Interactive R Shiny dashboard and course project analyzing U.S. suicide mortality trends by state, sex, and race using CDC WONDER data (ICD-10 X60-X84), with visualizations, statistical tests, and policy-focused conclusions.
# Suicide Mortality Trends Dashboard

An interactive **R Shiny dashboard** and course project exploring **U.S. suicide mortality trends** using **CDC WONDER mortality data**. The project examines how suicide mortality rates vary over time, across states, and across demographic groups such as sex and race. It combines exploratory analysis, statistical testing, policy interpretation, and an interactive dashboard.

## Project Overview

This project was developed to answer three core research questions:

1. **How do suicide mortality rates change over time?**  
2. **Which states have the highest rates, and which are increasing the fastest?**  
3. **How do suicide mortality rates differ by sex and race?**

The dashboard is designed to support public health discussion and prevention planning by making trends easy to explore and interpret.

## Features

- **Interactive state-level trend analysis** across a user-selected year range
- **Optional demographic filters** for sex and race
- **State ranking plot** for the most recent year in the selected range
- **Fastest-increasing states plot** based on estimated linear slopes
- **Statistical testing tab** with:
  - Poisson regression trend model with population offset
  - Spearman correlation between year and crude rate
  - Poisson rate ratio comparison for first vs. last year
  - Optional interaction testing for demographic trend differences
- **Conclusions and policy tab** summarizing findings, limitations, and future work
- **Download options** for filtered data and trend plots

## Data Source

This project uses **CDC WONDER mortality data** based on death certificate records from the National Vital Statistics System (NVSS).

- **Outcome of interest:** suicide mortality
- **ICD-10 codes:** **X60–X84** (intentional self-harm)
- **Measures used:** deaths, population, and crude rate per 100,000

## Repository Contents

```text
.
├── app_rubric_updated_v3.R          # Main Shiny app file
├── W3/                              # Early exploratory analysis files
├── W4/                              # Intermediate course project files
├── W5/                              # Statistical analysis files
├── W6/                              # Final project write-up files
├── archive/                         # Archived source data and supporting files
├── shiny_app_presentation_FINAL.pptx
└── R Shiny App Presentation.mp4
```

## How to Run the App

### 1. Install required packages

```r
install.packages(c("shiny", "tidyverse"))
```

### 2. Prepare the data file

The app expects the main dataset at:

```text
data/NEWDATASET_VER1.csv
```

If you are using this repository as uploaded, you may need to:

- create a `data/` folder in the project root
- place your cleaned CDC WONDER dataset inside it
- rename the main app file from `app_rubric_updated_v3.R` to `app.R` for standard Shiny deployment

### 3. Run locally

In RStudio or R:

```r
shiny::runApp()
```

Or open the app file directly and click **Run App** in RStudio.

## Methods Summary

The dashboard uses descriptive and inferential methods to support the research questions:

- **Trend visualization:** crude suicide mortality rate over time for the selected state and filters
- **State comparison:** top states by crude suicide mortality rate in the selected year
- **Slope estimation:** identifies states with the steepest increases over time
- **Poisson regression:** models annual changes in deaths while accounting for population size
- **Spearman correlation:** evaluates monotonic association between year and crude rate

## Interpretation Notes

- Rates shown in the dashboard are **crude rates per 100,000**
- Rows with **small counts** may produce unstable estimates
- The app includes an option to exclude rows flagged as unreliable or with **deaths < 20**
- This is a **descriptive, population-level analysis** and should not be interpreted as causal or as an individual-level risk model

## Limitations

- Small subgroup counts can make some rates unstable
- Grouped categories may mask within-group heterogeneity
- Results depend on available CDC WONDER stratification choices and reporting structure
- Crude rates are shown; age-adjusted analyses would strengthen comparisons

## Future Improvements

- Add age-group and age-adjusted analyses
- Incorporate contextual variables such as rurality, unemployment, or policy-related factors
- Explore changepoint or joinpoint-style trend analysis
- Expand demographic subgroup comparisons where data availability allows

## Public Health Relevance

This project is intended to help identify high-rate or fast-rising patterns in suicide mortality so that analysts, students, and policymakers can prioritize further investigation and prevention planning.

## Author

**Abigail Torbatian**  
Carnegie Mellon University

## Important Note

This repository contains academic project materials, supporting reports, and presentation files in addition to the Shiny app itself. If you plan to make the repository public, review the included files and remove any materials you do not want published.
