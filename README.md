# Summary

A compilation of data sources, ingestion scripts and aggregation processes used to compile the Compound Risk Monitor

## Main scripts and data architecture

Individual indicator datasets are stored in the 'Indicator_dataset' folder, with many of the source materials compiled using scripts in the main repository

'Coumpound_Risk_database.r' is used to compile the various indicator datasets into the eight risk component datasets. These can be found as .csv files in the 'Risk_sheets' folder.

'Global database and summary sheet.r' takes the various risk component datasets and aggregates them into a summary sheet of country-level compound risk scores. This can be found in the 'Risk_sheets" folder as 'Compound_Risk_Flags_Sheet.csv'

## Main Outcome file

Alongside the individual datasets, the primary output is the 'Global_compound_risk_database.csv' file. A replica dataset is also produced as an .xls file, with conditional formatting outlined in 'Global database and summary sheet.r'


