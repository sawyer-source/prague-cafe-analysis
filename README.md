# Prague Café Analysis

Data Analytics semester project, topic **Café Size**: how big Prague cafés are and how their size relates to prices and ratings.

## How to reproduce

1. Open `prague-cafe-analysis.Rproj` in RStudio (this sets the working directory to the repository root).
2. Run the scripts in order:
   - `analysis/01_cleaning.R` reads `analysis/raw_data.csv` and writes `analysis/dataset.csv`
   - `analysis/02_descriptive.R` writes the descriptive table and plots
   - `analysis/03_analysis.R` runs analyses A1 to A10 and writes their tables and figures
3. Compile `report/report.tex`.

All paths are relative to the repository root.

## Structure

```
analysis/       raw data, R scripts, cleaned data set
report/         LaTeX report, exported tables/ and figures/
presentation/   slides (LaTeX source and PDF)
```
