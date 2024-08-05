# Exploratory Data Analysis of Preterm Birth DREAM Challenge

## Description
This repository contains an RMarkdown document that performs an exploratory data analysis (EDA) of the preterm birth DREAM samples metadata. The objective is to gain insights into the demographic characteristics and clinical data of the participants, as well as to understand the relationship between various demographic factors and pregnancy outcomes.

## Data
The metadata file used for this analysis can be downloaded from the Challenge full data description [here](https://www.synapse.org/Synapse:syn26133770/wiki/618025) or [here](https://www.immport.org/shared/study/SDY2187).

## Files
- `EDA_Preterm_Birth.Rmd`: The main RMarkdown file containing the exploratory data analysis.
- `EDA_Preterm_Birth.html`: The output of the RMarkdown file.
- `metadata.csv`: The metadata file (this file should be placed in the same directory as the RMarkdown file for the code to run properly).
- `metadata_normalized.csv`: The metadata file with boolean columns of ethnics groups, without preterm data.
- Tabla de razas: Python script to make a ethnics description table.

## Requirements
The following R packages are required to run the analysis:
- `ggplot2`
- `dplyr`
- `polycor`
- `knitr`

## Usage
To generate the EDA report, follow these steps:
1. Clone this repository to your local machine.
2. Place the `metadata.csv` file in the same directory as the `EDA_Preterm_Birth.Rmd` file.
3. Open the `EDA_Preterm_Birth.Rmd` file in RStudio or any other R IDE.
4. Run the chunks sequentially or knit the document to produce an HTML report.

## Analysis Overview
The EDA document performs the following analyses:
1. **Data Loading and Cleaning**: Loads the metadata file and performs basic data cleaning and summarization.
2. **Demographic Analysis**: Analyzes the distribution of participants' ages, NIH racial categories, and their relationship with term status.
3. **Pregnancy and Delivery Details**: Investigates the distribution of term, preterm, and early preterm statuses among participants, as well as the distribution of delivery and sample collection weeks.

## Example Plots
The document includes various plots such as:
- Age Participant Distribution
- Age Participant Distribution by Term Status
- Distribution of Delivery Week
- Distribution of Sample Collection Week
- Distribution of NIH Racial Category with Preterm Proportions

## Additional Information
The chi-squared test results and correlation analyses are also included to understand the relationships between different demographic and clinical factors.

## References
- 2023 March of Dimes report card for United States: [March of Dimes Report](https://www.marchofdimes.org/peristats/reports/united-states/report-card)
