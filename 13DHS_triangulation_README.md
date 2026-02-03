# 13DHS_triangulation.R Script Documentation

## Overview
This script creates GT summary tables showing the risk of inadequate micronutrient intake for **Vitamin A** and **Iron** in Rwanda, based on EICV7 (Rwanda Integrated Household Living Conditions Survey 7, 2023-24) data.

## Purpose
The script generates publication-ready tables that display the percentage of the population at risk of inadequate intake for each micronutrient, stratified by:
- National level (overall Rwanda estimate)
- Administrative level 1 (Provinces)
- Residence (Urban/Rural)
- Socioeconomic quintile (1-5)

## Requirements

### R Packages
- `readr` - Reading CSV files
- `tidyverse` - Data manipulation
- `ggplot2` - Required by other packages
- `srvyr` - Survey-weighted analysis
- `gt` - Creating formatted tables
- `webshot2` - Saving tables as PNG images

### Input Data
The script requires the following data files (in `processed_data/` directory):
- `rwa_eicv2324_base_ai.csv` - Base apparent intake data
- `rwa_eicv2324_hh_information.csv` - Household information
- `shapefiles/rwa_hh_adm.rds` - Household administrative boundaries

### EAR Thresholds
The script uses Estimated Average Requirements (EAR) defined in `src/05base_model_functions.R`:
- **Vitamin A**: 490 μg RAE
- **Iron**: 22.4 mg (assuming low absorption)

## Output

### Tables Generated
1. **Vitamin A Inadequacy Table**
   - Saved as: `figures/vita_inadequacy_table.png`
   - Shows % at risk of inadequate Vitamin A intake

2. **Iron Inadequacy Table**
   - Saved as: `figures/fe_inadequacy_table.png`
   - Shows % at risk of inadequate Iron intake

### Table Structure
Each table includes:
- **Stratification column**: Type of grouping (National, Province, Residence, Socioeconomic Quintile) - shown only once per group to reduce repetition
- **Category column**: Specific category within stratification
- **% At Risk column**: Percentage at risk of inadequate intake (rounded to 1 decimal place)

**Note**: The stratification labels appear only once per group for cleaner presentation. For example:
```
| Stratification | Category        | % At Risk |
|----------------|-----------------|-----------|
| Province       | City of Kigali  | XX.X      |
|                | Eastern         | XX.X      |
|                | Northern        | XX.X      |
```

## Usage

To run the script:
```R
source("src/13DHS_triangulation.R")
```

The script will:
1. Load required packages (installing if necessary)
2. Load and merge data from multiple sources
3. Calculate inadequacy indicators using EAR thresholds
4. Generate survey-weighted prevalence estimates
5. Create and display GT tables
6. Save tables as PNG images
7. Print summary statistics to console

## Technical Details

### Survey Weighting
All prevalence estimates are calculated using survey weights (`survey_wgt`) via the `srvyr` package to ensure nationally representative estimates.

### Inadequacy Definition
A household is considered at risk of inadequate intake if:
- Vitamin A: apparent intake < 490 μg RAE per AFE per day
- Iron: apparent intake < 22.4 mg per AFE per day

### AFE (Adult Female Equivalent)
All intake values are expressed per Adult Female Equivalent (AFE), which normalizes household consumption by household composition and nutritional requirements.

## Notes
- The script follows the structure and style of existing scripts in this repository, particularly `08overall_inadequacy.R`
- Although the filename includes "DHS", the analysis uses EICV7 data
- The script cleans up the R environment at the end
