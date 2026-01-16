# Heat Exchanger Analysis

This project analyzes the performance of a heat exchanger by examining the relationship between various operating parameters and clogging events ("Stuck" / NoS).

## Overview

The analysis focuses on:
- **U-Value (UA):** Overall heat transfer coefficient.
- **Jacket Pressure:** Pressure in the jacket.
- **Pump Outlet Pressure:** Pressure at the pump outlet.
- **Clogging (NoS):** Number of "Stuck" events.

The goal is to determine if there are correlations between the maximum values of these parameters (at the beginning and end of campaigns) and the frequency of clogging.

## Project Structure

- `Analysis_1st.Rmd`: The main R Markdown file containing the analysis code. It loads data, processes it, and generates visualizations and regression models.
- `data/`: Directory containing the input Excel files:
    - `20251210_u_value.xlsx`: Time series data for U-value and pressures.
    - `NoS_20251201.xlsx`: Data on clogging events (NoS).
- `sample01.R`, `sample02.R`: Sandbox R scripts for testing code snippets.

## Requirements

To run this analysis, you need R installed with the following packages:

- `tidyverse`
- `readxl`
- `DT`
- `gt`
- `lubridate`
- `GGally` (used in sample scripts)
- `e1071` (used in sample scripts)

You can install the necessary packages using R:

```r
install.packages(c("tidyverse", "readxl", "DT", "gt", "lubridate", "GGally", "e1071"))
```

## Usage

1. Open `project.Rproj` in RStudio.
2. Open `Analysis_1st.Rmd`.
3. Click the "Knit" button to generate the analysis report (HTML).

## Author

Ryuta
