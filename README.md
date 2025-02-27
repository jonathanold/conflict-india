# The effect of political reservations on conflict in India

This repository contains the code, data, and analysis for studying patterns of conflict in India. The project how political reseravtions affect conflict in India. By leveraging cutting-edge methods in econometrics, machine learning, geospatial analysis, and natural language processing (NLP), this work aims to provide actionable insights for policymakers and researchers.
The full research paper is in `JOld_Reservations-and-Conflict.pdf`.
Data cleaning is done in Stata, and the analysis is done in R and Stata. Parts of the code for geocoding and natural language processing are done in Python (using BERT and tensorflow). 

---
## `analysis_stata/`

This folder contains Stata `.do` files for cleaning, preparing, and analyzing conflict data in India. The scripts implement various econometric analyses, balance checks, and comparisons across administrative levels (constituencies, districts, and states). Below is a description of the files:

### **Files and Descriptions**

- **`00_globals.do`**
Defines global parameters, file paths, and macros used across the project. This script standardizes settings for consistent execution of subsequent `.do` files.
- **`01_prep_delim_data.do`**
Prepares delimited data files (e.g., CSV or TSV) for analysis by importing, cleaning, and formatting raw datasets. It ensures compatibility with Stata's data structure.
- **`02_prep_all.do`**
Consolidates multiple datasets into a unified format (from the `prep/` foder, see below). It handles merging, deduplication, and variable standardization to create a master dataset for analysis.
- **`03_analysis_constituency.do`**
Performs constituency-level analysis of conflict data. This includes regressions to assess the impact of socio-economic variables on conflict outcomes.
- **`03_analysis_constituency_new.do`**
An updated version of `03_analysis_constituency.do`, incorporating additional variables or methods for enhanced analysis.
- **`04_analysis_district.do`**
Conducts district-level analysis of conflict patterns. It evaluates relationships between district characteristics and conflict intensity or frequency.
- **`05_ac_balance.do`**
Implements balance checks for Assembly Constituencies (ACs) to ensure comparability across treatment and control groups in randomized analyses.
- **`07_satp_anaylsis.do`**
Analyzes conflict data from the South Asia Terrorism Portal (SATP), focusing on trends and patterns specific to terrorism-related events.
- **`70_state_level.do`**
Aggregates conflict data at the state level. It performs state-level regressions and visualizations to identify macro-level patterns.
- **`99_acled_vs_bert.do`**
Compares conflict event data from ACLED (Armed Conflict Location \& Event Data Project) with machine learning outputs (e.g., BERT models). This script evaluates classification accuracy and consistency between datasets.

---
## `prep/`

The `prep/` folder contains Stata `.do` files used for cleaning, processing, and preparing raw datasets for analysis. These scripts handle tasks such as variable labeling, merging datasets, and creating structured data files for downstream statistical and econometric analyses.

### **Files and Descriptions**

- **`acled.do`**
Processes ACLED (Armed Conflict Location \& Event Data Project) data by cleaning and restructuring it for integration with other datasets. Includes steps like filtering relevant events and standardizing variable names.
- **`label_vars.do`**
Standardizes variable labels across datasets to ensure consistency in naming conventions. Useful for improving dataset readability and compatibility.
- **`delimitation_data.do`**
Processes delimitation data to map electoral boundaries to conflict events. This includes cleaning constituency-level identifiers and merging them with district-level data.
- **`prep_*.do`**
Prepares data from various sources. Includes merging and formatting steps to facilitate later merges.

---
## `analysis_r/`

This folder contains R scripts for statistical and spatial analysis of conflict data in India. The scripts implement various econometric and geospatial methods to explore patterns of conflict, assess policy impacts, and generate visualizations. Below is a description of each file:

### **Files and Descriptions**

- **`conley.R`**
Implements Conley standard errors to account for spatial and serial correlation in regression models. It uses Rcpp for efficient computation and supports flexible kernel functions for distance-based corrections.
- **`gen_ml_training_data.R`**
Prepares machine learning training datasets by filtering and processing conflict event data. It includes text preprocessing for event descriptions, random sampling by year, and assignment of tasks for manual coding.
- **`rdd.R`**
Conducts a regression discontinuity design (RDD) analysis using the `RDHonest` package. It evaluates policy impacts on outcomes such as conflict intensity and fatalities.
- **`spatial_caste.R`**
Performs spatial analysis of caste-related conflicts. It merges conflict data with geospatial constituency maps, calculates event densities, and executes regressions to explore relationships between caste reservations and conflict patterns.
- **`spatial_caste_slides.R`**
Generates regression tables and visualizations for presentations on caste-related conflict. Includes robustness checks using Poisson and negative binomial models.
- **`map_states.R`**
Creates state-level maps of conflict events and fatalities using geospatial data. Outputs include visualizations highlighting spatial patterns of conflict intensity.
- **`gsynth.R`**
Implements generalized synthetic control (GSynth) methods to estimate the causal effects of policies on conflict outcomes. Produces gap plots and counterfactual analyses.
- **`simulation.R`**
Simulates difference-in-differences (DiD) scenarios to evaluate the robustness of treatment effect estimates under varying assumptions about treatment assignment and fixed effects.
- **`spatial_new.R`**
Extends spatial analysis with updated data on caste-related conflicts. Includes advanced mapping techniques and spatial regressions to assess the impact of socio-economic variables on conflict patterns.
- **`spatial.R`**
Conducts comprehensive spatial econometric analyses, including Moran's I tests, spatial lag models, and spatial Durbin models. Generates maps and stability plots to visualize relationships between conflict events, fatalities, and socio-economic factors.

---
## `SATP code/`

This folder contains scripts and resources for analyzing conflict data from the South Asia Terrorism Portal (SATP). The focus is on extracting, cleaning, and processing SATP data to study patterns of terrorism-related events in India. Below is a description of the files:

### **Files and Descriptions**

- **`satp_scraper.py`**
A Python script for scraping conflict event data from the SATP website. It extracts details such as event dates, locations, actors involved, and event descriptions into a structured format (e.g., CSV).
- **`satp_cleaning.ipynb`**
A Jupyter Notebook that processes raw SATP data. It includes steps for handling missing values, standardizing column names, and filtering events based on relevance (e.g., terrorism-related incidents).
- **`satp_analysis.do`**
A Stata `.do` file that performs statistical analysis on cleaned SATP data. It includes regressions to explore relationships between terrorism events and socio-economic or geographic factors.
- **`satp_visualizations.ipynb`**
Generates visualizations such as time-series plots, heatmaps, and geographic maps to illustrate trends in terrorism-related incidents across India.
- **`satp_metadata.txt`**
A text file describing the structure of the SATP dataset, including variable definitions and data sources.


### **How to Use**

1. Run `satp_scraper.py` to collect raw data from the SATP website.
2. Use `satp_cleaning.ipynb` to preprocess and clean the scraped data.
3. Analyze trends using `satp_analysis.do` or create visualizations with `satp_visualizations.ipynb`.

---
## `BERT/`

This folder contains scripts and notebooks for applying BERT (Bidirectional Encoder Representations from Transformers) to classify and analyze conflict-related events in India. The focus is on leveraging pre-trained BERT models for natural language processing (NLP) tasks, including text classification and feature extraction.

### **Files and Descriptions**

- **`conflict_classify_text_with_bert.ipynb`**
Implements a pipeline to classify conflict events using BERT. The workflow includes:

1. Loading a BERT model from TensorFlow Hub.
2. Fine-tuning the model on conflict data.
3. Training the classifier on variables from ACLED (Armed Conflict Location \& Event Data Project).
4. Saving and reloading the trained model for predictions on SATP (South Asia Terrorism Portal) data.
- **`acled_nlp_prep.ipynb`**
Prepares ACLED data for NLP analysis by cleaning and tokenizing text fields. It uses libraries like NLTK and TextBlob for preprocessing, including lemmatization, stemming, and stopword removal. The notebook also generates word frequency distributions and sentiment analyses.
- **`acled_csv.ipynb`**
Reads and processes raw ACLED CSV files to extract relevant columns for analysis. It includes basic exploratory data analysis (EDA) to understand event types, locations, and actor descriptions.
- **`acled_prep.ipynb`**
Refines ACLED data by creating binary variables for event types (e.g., riots, protests) and sub-event types (e.g., violent demonstrations). It also filters rows based on specific keywords related to caste or communal conflicts.


### **Key Features**

1. **Text Classification**: Fine-tunes BERT to classify conflict events based on textual descriptions.
2. **Data Preparation**: Scripts preprocess conflict data for compatibility with NLP pipelines.
3. **NLP Tools**: Utilizes NLTK, TextBlob, TensorFlow Hub, and TensorFlow Text for advanced text processing.

### **How to Use**

1. Run `acled_csv.ipynb` or `acled_prep.ipynb` to prepare raw ACLED data.
2. Use `acled_nlp_prep.ipynb` to clean and tokenize text fields.
3. Train and evaluate the classification model using `conflict_classify_text_with_bert.ipynb`.

---
## `Geocoding/`

This folder contains scripts and notebooks for geocoding conflict-related data, converting location information into geographic coordinates (latitude and longitude), and performing spatial analysis. The tools leverage geocoding services like Google Maps API, GeoNames, and Mordecai for forward and reverse geocoding tasks. Below is a description of the key files:

### **Files and Descriptions**

- **`geocode_mines.R`**
An R script that uses the Google Maps API to geocode coal mine locations in India. It processes location data from Excel files, extracts latitude and longitude coordinates, and saves the geocoded results as a CSV file.
- **`geocoding_conflicts.ipynb`**
A Python notebook that geocodes conflict-related data using GeoNames and other geocoding APIs. It calculates distances between actual and estimated coordinates and outputs detailed location data for further analysis.
- **`geocoding-mordecai3.ipynb`**
This notebook uses Mordecai, a full-text geoparser, to extract geographic information from textual descriptions of conflict events. It integrates Elasticsearch for querying GeoNames data and outputs structured geographic data.
- **`GED_choropleth.ipynb`**
Creates choropleth maps to visualize conflict intensity (e.g., fatalities) across different administrative boundaries in India. The notebook uses geospatial libraries like GeoPandas and Matplotlib to map geocoded data.


### **Key Features**

1. **Forward Geocoding**: Converts place names or addresses into geographic coordinates using APIs like Google Maps and GeoNames.
2. **Reverse Geocoding**: Extracts location names from latitude/longitude coordinates using Mordecai.
3. **Spatial Visualization**: Generates maps to visualize spatial patterns of conflict intensity.
4. **Integration with Elasticsearch**: Uses Elasticsearch containers (via `es-geonames-master`) to query large-scale geographic datasets efficiently.

### **How to Use**

1. Install required dependencies:
    - R libraries: `ggmap`, `readxl`
    - Python libraries: `geopandas`, `matplotlib`, `mordecai3`, `elasticsearch`
2. Run the scripts or notebooks:
    - Use `geocode_mines.R` for geocoding coal mine data.
    - Use `geocoding_conflicts.ipynb` or `geocoding-mordecai3.ipynb` for conflict-related geocoding tasks.
    - Use `GED_choropleth.ipynb` to create visualizations of conflict data.

---

## `archived_code/`

This folder serves as a repository for deprecated or experimental code that is no longer actively used in the project. The scripts and files here may contain useful methods, functions, or ideas that informed earlier stages of development but were later replaced or superseded by updated approaches. Below is a description of the key files:

### **Files and Descriptions**

- **`deprecated-conley.R`**
Implements Conley standard errors with spatial and serial correlation adjustments using Rcpp for efficient computation. This script was used to calculate robust standard errors for regression models but has since been replaced by updated methods.
- **`deprecated-cpp-functions.cpp`**
Contains C++ functions used in conjunction with `deprecated-conley.R`. Includes implementations of distance calculations (e.g., Haversine formula) and kernel-based spatial correlation adjustments for econometric models.

These files are retained for reference but are not part of the active analysis pipeline.

---
