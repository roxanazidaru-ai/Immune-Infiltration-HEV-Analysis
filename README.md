# Immune Infiltration and HEV Analysis

This repository contains the R script and datasets used for the immune infiltration and high endothelial venule (HEV) analysis in my MSc dissertation.

## Repository contents

- `01_Process_Immune_HEV_Data.R`: R script used to process QuPath measurements and prepare mouse-level data for statistical analysis.
- `Raw_data/`: input dataset containing tumour and lymphoid aggregate measurements obtained from QuPath.
- `Processed_data/`: processed dataset prepared for statistical analysis and plotting in GraphPad Prism.
- `Immune_HEV_Analysis.Rproj`: RStudio project file.

## Analysis outputs

The R workflow generated the following mouse-level metrics:

- Lymphoid aggregate density (aggregates/mm² tumour area)
- Lymphoid aggregate area (% tumour area)
- Mean lymphoid aggregate area (µm²)
- HEV-positive area (% tumour area)
- HEV-positive area (% lymphoid aggregate area)

## Data processing workflow

| Step | What was done |
|---|---|
| Input | Tumour-level and lymphoid aggregate measurements generated in QuPath were imported into R from the raw Excel workbook. |
| Data validation | Mouse IDs, treatment groups, required worksheets and numerical measurements were checked before processing. |
| Missing-data handling | Mice without lymphoid aggregates were retained for tumour-level analyses, while aggregate-specific measurements were treated as not applicable. |
| Aggregate processing | Individual aggregate measurements were grouped by mouse to calculate aggregate number, total aggregate area, mean aggregate area and total HEV-positive area within aggregates. |
| Area conversion | Tumour area was converted from µm² to mm² for aggregate density calculations. |
| Metric calculation | Aggregate density, aggregate area as a percentage of tumour area, mean aggregate area, tumour HEV-positive area percentage and aggregate-associated HEV-positive area percentage were calculated. |
| Mouse-level processing | One final value per mouse was generated for each endpoint, preserving the mouse as the independent biological replicate. |
| Spatial information | Intratumoural and peripheral aggregate counts were retained in the processed dataset but were not used as primary statistical endpoints. |
| Output | Mouse-level data and separate Prism-ready endpoint tables were exported into a single Excel workbook. |
| Statistics | Statistical testing and graph generation were performed separately in GraphPad Prism. |

## Running the analysis

1. Click **Code → Download ZIP**.
2. Extract the downloaded ZIP file.
3. Open `Immune_HEV_Analysis.Rproj` in RStudio.
4. Open `01_Process_Immune_HEV_Data.R`.
5. Run the complete script from the beginning.

The original folder structure must be retained so that the relative file paths work correctly.
