# Summary

A compilation of data sources, ingestion scripts and aggregation processes used to compile the Compound Risk Monitor

## Main scripts and data architecture

Individual indicator datasets are stored in the 'Indicator_dataset' folder, with many of the source materials compiled using scripts in the main repository (e.g. 'GDACS scrape.r', 'FAO scrape.r', 'COVID scrape data.r', 'Debt scrape.r') 

'Coumpound_Risk_database.r' is used to compile the various indicator datasets  the eight seperate risk component datasets. These can each be found as .csv files in the 'Risk_sheets' folder.

'Global database and summary sheet.r' takes the various risk component datasets and aggregates them into a summary sheet of country-level compound risk scores. This can be found as 'Compound_Risk_Flags_Sheet.csv' in the 'Risk_sheets" folder 

## Main Outcome file

Alongside the individual datasets, the primary output is the 'Global_compound_risk_database.csv' file. A replica full dataset of the Compound Risk Monitor is produced as an .xls file as 'Global_compound_risk_database.xls', with conditional formatting outlined in 'Global database and summary sheet.r'


