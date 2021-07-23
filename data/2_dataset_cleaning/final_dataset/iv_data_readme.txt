The IntoValue dataset contains clinical trials conducted at a German UMC and registered on ClinicalTrials.gov or the German Clinical Trials Registry (DRKS). All trials were reported as complete between 2009 and 2017 on the trial registry at the time of data collection. The dataset also includes a results publication found via manual searches; if multiple results publications were found, the earliest was included.

Trials were associated with a German UMC by searching trials with UMC listed as responsible party, lead sponsor or with a principle investigator (PI) from a UMC ('lead_city'), or with a UMC only listed as facility (`facility_city`; version 1 only). A lookup table of German UMC regexes is available under `generated_samples/city_search_terms_no_abbrev.csv`.

Trials include all interventional studies and are not limited to investigational medical product trials, as regulated by the EU's Clinical Trials Directive or Germany's Arzneimittelgesetz (AMG) or Novelle des Medizinproduktegesetzes (MPG).

DRKS data were searched and downloaded as CSVs from the DRKS website (https://www.drks.de/). ClinicalTrials.gov data were downloaded as CSVs from Clinical Trials Transformation Initiative (CTTI) Aggregate Content of ClinicalTrials.gov (AACT) (https://aact.ctti-clinicaltrials.org/). DRKS and ClinicalTrials.gov use different terminology for various trial aspects, such as phase and masking; these different levels are captured in the data dictionary as `levels_drks` and `levels_ctgov`. For later analyses requiring parity across registries, levels for some variables were collapsed and a lookup table is provided in `iv_data_lookup_registries.csv`.

These data were generated and used for two publications (Wieschowski et al., 2019; Riedel et al. 2021) and therefore comprises two versions (indicated as `iv_version`).

For version 1, registry data was collected on April 17, 2017 from ClinicalTrials.gov and on July 27, 2017 for DRKS and was limited to trials with a completion date on DRKS and primary completion date on ClinicalTrials.gov between 2009 and 2013. Version 1 manual searches for results publications were conducted from 2017-07-01 to 2017-12-01.
For version 2, registry data was collected on June 3, 2020 and was limited to trials with a completion date on DRKS and ClinicalTrials.gov between 2014 and 2017. Version 2 manual searches for results publications were conducted from 2020-07-01 to 2020-09-01.

Publication identifiers (DOI, PMID, URL) were manually entered during the publication search and then further enhanced using the API of Internet Archive's open-source Fatcat catalog of research publications, to add PMIDs based on DOIs, and vice versa.

Partially different manual search steps were used in the two versions and are indicated and described in `identification_step`.
Version 1 includes trials with a German UMC as either a `lead_city` or a `facility_city`, whereas version 2 is limited to trials a German UMC as a `lead_city`.

Each row indicates a single trial registration. Due to changes in completion dates, some trials are duplicated between versions as indicated in `is_dupe`. Deduplication of cross-registration was manually performed and some trials are cross-registered duplicates remain (e.g., DRKS00004156 and NCT00215683) and are not indicated in the dataset.

All dates are provided as `yyyy-mm-dd`.

Additional documentation on each variable (type, description, levels) is provided in `iv_data_dictionary.csv`.

Additional information on the project and methods for generating the dataset is available in associated publications and at the project's OSF page (https://doi.org/10.17605/osf.io/fh426).

References:
- Wieschowski, S., Riedel, N., Wollmann, K., Kahrass, H., Müller-Ohlraun, S., Schürmann, C., Kelley, S., Kszuk, U., Siegerink, B., Dirnagl, U., Meerpohl, J., & Strech, D. (2019). Result dissemination from clinical trials conducted at German university medical centers was delayed and incomplete. Journal of Clinical Epidemiology, 115, 37–45. https://doi.org/10.1016/j.jclinepi.2019.06.002
- Riedel, N. (2021). IntoValue 2. https://github.com/quest-bih/IntoValue2