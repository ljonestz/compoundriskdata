# Summary

A compilation of data sources, ingestion scripts and aggregation processes used to compile the Compound Risk Monitor.

## Main scripts and data architecture

Individual indicator datasets are stored as separate files in the 'Indicator_dataset' folder, with many of the source materials generated using scripts in the main repository (e.g. 'GDACS scrape.r', 'FAO scrape.r', 'COVID scrape data.r', 'Debt scrape.r'). 

The 'Coumpound_Risk_database.r' script is used to compile the various indicator datasets into the eight seperate risk component sheets. These can each be found as .csv files in the 'Risk_sheets' folder.

## Main Outcome files

'Global database and summary sheet.r' takes the various risk component datasets and aggregates them into a summary sheet of country-level compound risk scores ('Compound_Risk_Flags_Sheet.csv'), as well as a global database featuring all of the indicators feeding into the Compound Risk Monitor ('Global_compound_risk_database.csv'). Finally, a replica full dataset of the Compound Risk Monitor is produced as an .xls file. ('Global_compound_risk_database.xls').

 All three datasets are compiled using the 'Global database and summary sheet.r' script. Outputs databasets can be found in the 'Risk_sheets" folder. 



