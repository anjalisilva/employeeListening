# Aerodyne Employee Listening and Job Architecture Analysis

A reproducible R workforce-analytics project examining where employees 
experience structural career ceilings and how those conditions are
associated with career-growth sentiment, recent promotion, and voluntary
turnover. The analysis combines employee records, job architecture, and 
listening-survey data; validates the career-growth measure; evaluates 
adjusted relationships; assesses equity exposure; and tests the sensitivity
of key assumptions.

## Dataset

The analysis requires the following workbook to be placed in the project’s `data/` subdirectory:

```text
data/Case Dataset Aerodyne.xlsx
```

The workbook is not made available in the public repository. Users must obtain a copy and place it at the path above before running the analysis.

The workbook must contain these tabs:

| Tab | Purpose |
|---|---|
| `README` | Dataset overview and supporting documentation |
| `Data_Dictionary` | Variable definitions and coding details |
| `Employees` | Employee, job, compensation, promotion, and turnover records |
| `Job_Architecture` | Job-family levels, ordering, and salary-band information |
| `Listening_Survey` | Employee listening-survey responses |

The script stops with an informative error when the workbook is missing, a required tab is absent, or employee records do not match the job architecture.

## Language and Tool Versions

| Component | Version |
|---|---|
| R | R version 4.6.1 (2026-06-24) |
| Platform | x86_64-apple-darwin20 |
| Operating system | macOS Ventura 13.7.8 |
| Environment management | `renv` |

The analysis uses the following R packages:

```text
readxl
dplyr
janitor
here
broom
psych
ggplot2
tidyr
```

Exact package versions are recorded in `renv.lock`.

