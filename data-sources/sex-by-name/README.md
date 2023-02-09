# UCI Gender by Name Dataset
This file compiles counts of first names and "genders" using data from US, UK, Canada, and Australia governmental datasets.
While the dataset is named and described as collating gender information, the data used is based on baby names and therefore
more accurately describes the sex associated with given names, so that is the nomenclature used within this analysis.

## File used for this analysis
Sourced from: https://archive.ics.uci.edu/ml/machine-learning-databases/00591/name_gender_dataset.csv via https://archive.ics.uci.edu/ml/datasets/Gender+by+Name# <br />
License: N/A<br />
Dataset creator and donator: Arun Rao -  Skydeck, UC Berkeley, Berkeley, CA<br />
File: name_gender_dataset.csv as donated to UCI Machine Learning Repository on 2020-03-15<br />
Downloaded on: January 30, 2023

## Data dictionary

| Column      | Datatype | Description
| ----------- | ----------- | ------------|
| name     | Text | baby's first/given name
| sex   | Text | M/F
| count | Integer | Number of instances of `name` for `sex` found in the data
| probability | Float | Probability of encountering the name in the dataset given the aggregate count

## Potential alternative dataset collection method
Use source institutional websites as listed on the dataset's webpage:
* US: [Baby Names from Social Security Card Applications - National Data, 1880 to 2019](https://catalog.data.gov/dataset/baby-names-from-social-security-card-applications-national-level-data)
* UK: [Baby names in England and Wales Statistical bulletins, 2011 to 2018](https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/bulletins/babynamesenglandandwales/previousReleases)
* Canada: [British Columbia 100 Years of Popular Baby names, 1918 to 2018](https://www2.gov.bc.ca/gov/content/life-events/statistics-reports/bc-s-most-popular-baby-names)
* Australia: [Popular Baby Names, Attorney-General's Department, 1944 to 2019](https://data.gov.au/dataset/ds-sa-9849aa7f-e316-426e-8ab5-74658a62c7e6/details?q=)

