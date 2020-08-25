# Replicating and running the Compound Risk Monitor

A compilation of data sources, ingestion scripts and aggregation processes used to compile the Compound Risk Monitor.

## Main scripts and data architecture

**Coumpound_Risk_database.r** is used to compile datasets for each of the indicators that feed into the eight seperate risk component sheets. Many of the ingestion commands described above also feature as part of the script and automatically generate real-time risk information. The script saves each of the individual source indicators as separate .csv files saved in the **Indicator_dataset** folder. In addition, eight summary files compiling all source indicators that feed into each respective risk component are generated and saved in the **Risk_sheets** folder. *Note: a number of errors are thrown out when the Natural Hazards sheet is created - though these should not be consequential to the results.*

**Global database and summary sheet.R** takes inputs from each of the risk components sheets and calculates overall risk scores (both for the eight risk components as well as a total compound risk score for each country). In addition, a series of alternative risk calculations and reliability scores are generated for comparison. Lastly, the script is used to generate a summary Excel file designed to mimic the original CRM database.

**Compound Plots.r** generates a series of comparative plots and visuals that are subsequently saved to the **Plots** folder.

Alongside the above, separate scirpts are included in the main folder and can be used to scrape and generate source datasets (e.g. 'GDACS scrape.r', 'FAO scrape.r', 'COVID scrape data.r', 'Debt scrape.r'). *Note: many of these of these are included in the Coumpound_Risk_database.r script already and do not need to be run in advance.*


## Outcome databases

The main databases can be found in the **Risk_Sheets** folders. These include:

**Global_compound_risk_database.csv** is a compilation of all raw source indicators that feed into the Compound Risk Monitor. These are labelled according to each of the eight risk components (see labelling codes below). 

**Compound_Risk_Flags_Sheet.csv** is a dataset of all summarised country-level compound risk scores. The database also includes all normalised indicators that are used to generate the total risk scores.

**Compound_Risk_Monitor.xslx** is an Excel file that presents all summarised country-level compound risk scores. It is designed to mimic the style used in developing the original Excel CRM, and features separate tabs for each of the eight risk componenets - alongside reliability scores and alternative risk calculations.

**reliabilitysheet.csv** presents the proportion of missing values used in calculating risk scores for each country. It should be seen as a measure of reliability in the overall scores generated.

In addition, separate csv files are generated for each of the each risk components. Each file includes the source indicators, as well as the normalised scores used to calculate the various risk scores.

### Indicator labelling and indexing

All source indicators used in the Compound Risk Monitor are labelled according to the eight separate risk categories. Short tags are included at the start of the variable label, with the indicator description immediately thereafter. For example, F_ is assigned to all food security indicators, meaning that that the FEWSNET score is classified as: F_Fewsnet_score). 

### Labels are classified as follows:

C_ -> Conflict.  
D_ ->  Debt.  
FR_ -> Fragility and Institutions.  
H_ -> Health / COVID Response Capacity.  
F_ -> Food Security.  
M_ -> Macro-economic vulnerability.  
NH_ -> Natural Hazards.  
S_ -> Socioeconomic vulnerability.  
RELIABILITY_ -> Reliability scores.  
AV_ -> Average scores.  
SQ_ -> Geometric average scores.  
TOTAL_ -> Total risk scores.  
EMERGING_ -> Emerging risk scores.  
EXISTING_ -> Existing risk scores.  

## Sequence in generating CRM outputs

To replicate the databases and plots used int he Compound Risk Monitor start by running the **Coumpound_Risk_database.r**. This will generate all necessary indicator datasets and risk sheets. Then run **Global database and summary sheet.R** that calculates component and overall risk scores for each country. Lastly, run **Compound Plots.r** to generate summary plots and comparison graphs.

