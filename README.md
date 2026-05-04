# Exploration-of-demographic-shift-in-EU-and-its-effects-on-taxes
This repository contains R scripts used for the empirical part of a bachelor thesis focused on the relationship between demographic and macroeconomic variables and tax revenues in European countries.

The project analyzes panel data for European economies and estimates the effect of demographic change, macroeconomic conditions, and fiscal variables on three categories of tax revenue:

- `TT` – total tax revenue
- `DT` – direct tax revenue
- `IT` – indirect tax revenue

The workflow is divided into three main scripts: descriptive statistics, panel model estimation, and diagnostic testing.

---

## Repository structure

```text
.
├── BP - descriptive statistics.R
├── BP - panel analysis.R
├── BP -diagnostic tests.R
├── finaldata.xlsx              # required input data
├── BP - data.xlsx              # required input data for descriptive statistics
└── README.md
```

---

## Scripts overview

### 1. `BP - descriptive statistics.R`

This script prepares basic descriptive statistics for the dataset.

Main steps:

- loads the input Excel file,
- removes the `Year` column from the descriptive-statistics table,
- identifies numeric variables,
- calculates:
  - mean,
  - median,
  - minimum,
  - maximum,
  - standard deviation,
  - variance,
  - skewness,
  - kurtosis,
  - number of missing values.

Main packages:

```r
readxl
dplyr
moments
ggplot2
corrplot
```

Expected input file:

```r
BP - data.xlsx
```

Expected sheet:

```r
List1
```

---

### 2. `BP - panel analysis.R`

This script performs the main panel econometric analysis.

Main steps:

- loads panel data from Excel,
- converts the dataset into a `pdata.frame`,
- defines the panel structure using:
  - `Country`
  - `Year`
- calculates average VIF values by country,
- estimates pooled OLS, fixed effects, and random effects models,
- applies HAC/Newey-West robust standard errors,
- performs model specification and diagnostic tests,
- compares fixed and random effects using the Hausman test,
- checks heteroskedasticity and serial correlation,
- includes an Elastic Net section for additional variable-selection support.

Dependent variables:

```r
TT
DT
IT
```

Core explanatory variables:

```r
GDP
INF
UNM
GE
GNS
POP
DEP_15n
FER_18
GDP_h
GFCF
```

Main packages:

```r
readxl
plm
lmtest
car
outliers
dplyr
sandwich
glmnet
pglm
boot
```

Expected input file:

```r
finaldata.xlsx
```

Expected sheet:

```r
List1
```

---

### 3. `BP -diagnostic tests.R`

This script focuses on additional diagnostics for the panel dataset and estimated models.

Main steps:

- loads the panel dataset,
- performs stationarity tests:
  - ADF test,
  - KPSS test,
  - Levin-Lin-Chu panel unit root test,
  - IPS panel unit root test,
  - Hadri-type stationarity check,
- calculates a correlation matrix,
- visualizes correlations with `corrplot`,
- calculates pooled VIF values,
- calculates country-specific VIF values,
- performs panel diagnostic tests:
  - Wooldridge test for serial correlation,
  - Wald test,
  - Pesaran CD test for cross-sectional dependence.

Main packages:

```r
plm
dplyr
readxl
tseries
urca
pdR
lmtest
sandwich
car
stargazer
pcse
pscl
corrplot
```

Expected input file:

```r
finaldata.xlsx
```

---

## Data requirements

The scripts assume that the input Excel files contain a balanced or near-balanced country-year panel dataset.

Required panel identifiers:

```r
Country
Year
```

Main dependent variables:

```r
TT   # total tax revenue
DT   # direct tax revenue
IT   # indirect tax revenue
```

Main explanatory variables used in the scripts:

```r
GDP      # GDP-related variable
INF      # inflation
UNM      # unemployment
GE       # government expenditure
GNS      # gross national savings
POP      # population growth / demographic variable
DEP_15n  # dependency-related demographic indicator
FER_18   # fertility-related demographic indicator
GDP_h    # GDP per capita or GDP-related control
GFCF     # gross fixed capital formation
EMU      # Economic and Monetary Union indicator
```

Column names in the Excel files must match the variable names used in the scripts.

---

## How to run the analysis

1. Clone the repository:

```bash
git clone <repository-url>
cd <repository-folder>
```

2. Place the required Excel files in the project folder:

```text
BP - data.xlsx
finaldata.xlsx
```

3. Update the file paths in the scripts if needed.

The current scripts use local Windows paths such as:

```r
C:/Users/Jindřich/Desktop/finaldata.xlsx
C:/Users/Jindřich/Desktop/BP - data.xlsx
```

For better reproducibility, it is recommended to replace these paths with relative paths:

```r
file_path <- "finaldata.xlsx"
file_path <- "BP - data.xlsx"
```

4. Run the scripts in the following order:

```r
source("BP - descriptive statistics.R")
source("BP -diagnostic tests.R")
source("BP - panel analysis.R")
```

---

## Econometric methodology

The project applies standard panel-data techniques to examine the relationship between demographic shifts and tax revenues.

The main model structure can be summarized as:

```text
TaxRevenue_it = f(Demographic variables_it, Macroeconomic controls_it, Fiscal controls_it)
```

where:

- `i` denotes country,
- `t` denotes year,
- `TaxRevenue` is represented by `TT`, `DT`, and `IT`.

The analysis compares:

- pooled OLS models,
- fixed effects models,
- random effects models.

The preferred specification is evaluated using:

- Hausman test,
- heteroskedasticity diagnostics,
- serial-correlation diagnostics,
- cross-sectional dependence diagnostics,
- robust standard errors.

---

## Notes on reproducibility

The scripts automatically install missing R packages. However, for stable reproducibility, it is recommended to install all required packages manually before running the scripts.

Some sections may require minor path adjustments depending on the local working directory.

The script `BP - panel analysis.R` contains one section where `lm_model` is referenced in diagnostic tests. If this object is not available in the environment, replace it with the appropriate model object, for example:

```r
lm(TT ~ ., data = data[, c("TT", independent_vars)])
```

---

## Suggested citation / academic context

This code was developed as part of an empirical bachelor thesis project analyzing demographic change and its effects on taxation in European countries using panel regression methods.

---

## Author

Jindřich Bernard

---

## License

This repository is intended for academic and educational use. Add a license file if the repository is meant to be publicly reused.
