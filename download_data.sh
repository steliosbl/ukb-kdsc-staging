#!/bin/bash

# Download common data from DNAnexus platform
unset DX_WORKSPACE_ID
dx cd $DX_PROJECT_CONTEXT_ID

mkdir data
mkdir data/extracted
dx download common/Demographics/demographics.csv -o data/extracted
dx download common/Biomarkers/biomarkers.csv -o data/extracted
dx download common/"Follow-up"/follow_up.csv -o data/extracted
dx download common/"Hospital Records"/diagnoses.csv -o data/extracted
dx download common/"Hospital Records"/operations.csv -o data/extracted
dx download common/"Hospital Records"/icd10_codes.csv -o data/extracted
dx download common/"Hospital Records"/opcs4_codes.csv -o data/extracted
dx download common/"Medical History"/medical_history.csv -o data/extracted
dx download common/"Primary Care"/gp_registrations.csv -o data/extracted
dx download common/"Primary Care"/gp_clinical_records.csv -o data/extracted

dx download ukb-kdsc-staging/ -o data/ -r 

git pull 
Rscript -e "renv::restore()"
Rscript -e "source(knitr::purl('R00-parameters.rmd', quiet = TRUE, output = tempfile()))"
