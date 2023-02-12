# InfluNet

## Contents

This repository contains data extracted from the [Italian National Institute of Health (ISS)](https://www.epicentro.iss.it/influenza/influnet) bulletins starting from the [2003-2004 seasonal flu](https://w3.iss.it/site/rmi/influnet/pagine/stagioni.aspx). The repository will be updated every Friday.

*IMPORTANT* - As of Jan. 21st, data can be downloaded directly from [the official portal for European data](https://data.europa.eu/data/datasets/influenza-stagionale?locale=it)


**DATA UPDATE** -  As of Feb. 12th, national and regional vaccination rates by age group are available for each flu season.




## Repository structure
```

influnet/
├── flu-season/
│   ├── 2003-2004/
│   │   ├── epidemiological_data/
│   │       ├── national_cases.csv
│   │   ├── influnet_report/
│   │       ├── influnet-epi-2005-2006.pdf
│   ├── ...
│   │
│   │   
│   │
│   ├── 2012-2013/
│   │   ├── epidemiological_data/
│   │       ├── national_cases.csv
│   │       ├── regional_cases.csv
│   │   ├── influnet_report/
│   │       ├── influnet-epi/
│   │           ├── influnet-epi-2012-2013_1.pdf
│   │           ├── ...
│   │       ├── influnet-vir/
│   │           ├── influnet-vir-2012-2013_1.pdf
│   │           ├── ...
│   │   ├── virological_data/
│   │       ├── national_typing_subtyping_influenza_viruses.csv
├── data-aggregated/
│   ├── epidemiological_data/
│   │   ├── national_cases.csv
│   │   ├── regional_cases.csv
│   ├── virological_data/
│   │   ├── national_typing_subtyping_influenza_viruses.csv


```



## Epidemiological data schema

### National cases

| Column      | DataType | Description     |
| :---        |    :----:   |          ---: |
| flu_season      | String       | Flu season reference period  |
| year_week     |  String       | Bulletin reference week   |
| number_healthcare_workers   | Integer | Number of cases reported by the healthcare workers  |
| number_cases  | Integer | Weekly number of new confirmed cases  |
| population | Integer | Reference population |
| incidence | Double | 1000 x number_cases/population |
| pop_0-4 | Integer | 0-4 reference population  |
| cases_0-4 | Integer | 0-4 weekly new cases  |
| inc_0-4 | Double | 1000 x cases_age_0-4/pop_age_0-4  |
| pop_5-14 | Integer | 5-14 reference population  |
| cases_5-14 | Integer | 5-14 weekly new cases  |
| inc_5-14 | Double | 1000 x cases_age_5-14/pop_age_5-14  |
| pop_15-64 | Integer | 15-64 reference population  |
| cases_15-64 | Integer | 15-64 weekly new cases  |
| inc_15-64 | Double | 1000 x cases_age_15-64/pop_age_15-64  |
| pop_65+ | Integer | ≥65 reference population  |
| cases_65+ | Integer | ≥65 weekly new cases  |
| inc_65+ | Double | 1000 x cases_age_65-plus/pop_age_65-plus  |

### Regional cases

| Column      | DataType | Description     |
| :---        |    :----:   |          ---: |
| flu_season      | String       | Flu season reference period  |
| year_week     |  String       | Bulletin reference week   |
| region     |  String       | Region name   |
| number_healthcare_workers   | Integer | Number of cases reported by the healthcare workers  |
| number_cases  | Integer | Weekly number of new confirmed cases  |
| population | Integer | Reference population |
| incidence | Double | 1000 x number_cases/population |
| pop_0-4 | Integer | 0-4 reference population  |
| cases_0-4 | Integer | 0-4 weekly new cases  |
| inc_0-4 | Double | 1000 x cases_age_0-4/pop_age_0-4  |
| pop_5-14 | Integer | 5-14 reference population  |
| cases_5-14 | Integer | 5-14 weekly new cases  |
| inc_5-14 | Double | 1000 x cases_age_5-14/pop_age_5-14  |
| pop_15-64 | Integer | 15-64 reference population  |
| cases_15-64 | Integer | 15-64 weekly new cases  |
| inc_15-64 | Double | 1000 x cases_age_15-64/pop_age_15-64  |
| pop_65+ | Integer | ≥65 reference population  |
| cases_65+ | Integer | ≥65 weekly new cases  |
| inc_65+ | Double | 1000 x cases_age_65-plus/pop_age_65-plus  |

## Virological data schema

| Column      | DataType | Description     |
| :---        |    :----:   |          ---: |
| flu_season      | String       | Flu season reference period  |
| year_week     |  String       | Bulletin reference week   |
| influenza_viruses  |  String       |  Influenza virus name  |
| number_samples | Integer | Clinical samples received from the laboratories  |
| number_sequenced | Integer | Number of sequenced samples |
| number_detections_influenza_viruses | Integer | Number of detections reported of the virus |





## Getting the data

**Direct download (CSV)**: https://raw.githubusercontent.com/fbranda/influnet/main/data-aggregated/epidemiological_data/national_cases.csv

**Python** (requires `pandas`):
```python
import pandas as pd
df = pd.read_csv("https://raw.githubusercontent.com/fbranda/influnet/main/data-aggregated/epidemiological_data/national_cases.csv")
```

**R** (requires `httr`):
```r
library(httr)
df <- read.csv(text=content(GET("https://raw.githubusercontent.com/fbranda/influnet/main/data-aggregated/epidemiological_data/national_cases.csv")))
```

## Contributions
1) Automatic report at link https://fbranda.github.io/influnet/
2) Automatic bot at link https://mastodon.uno/@influbot@sociale.network


## License and attribution

This repository and data exports are published under the CC BY 4.0 license.


