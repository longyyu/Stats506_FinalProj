# Stats506_FinalProj

This repository contains the data, source code, and writeup for the final project of [Stats 506, F20](https://github.com/jbhender/Stats506_F20).  

## Proposed Question

> How does the choice of wall construction materials evolve over time? Are the trends different across census divisions?  

The analysis can be found in `report.html` [(preview)](https://raw.githack.com/longyyu/Stats506_FinalProj/main/report.html). 

## Data and Variables

* **Dataset:** the 2012 US Commercial Building Energy Consumption Survey (CBECS)  

| File | Link |
| :---- | :----: |
| Microdata | [link](https://www.eia.gov/consumption/commercial/data/2012/xls/2012_public_use_data_aug2016.csv) |
| Variable and response codebook | [link](https://www.eia.gov/consumption/commercial/data/2012/xls/2012microdata_codebook.xlsx) |


* **Variables:**  
  
| Variable Name | Description |
| :----: | :---- |
| `PUBID` | Building identifier |
| `FINALWT` | Final full sample building weight |
| `FINALWT1 – FINALWT197` | Final replicate weights |
| `REGION` | Census region |
| `YRCONC` | Year of construction (categorical) |
| `WLCNS` | Wall construction material |

## File structure

*  Data are downloaded from the EIA's website using [download_data.sh](https://github.com/longyyu/Stats506_FinalProj/blob/main/download_data.sh)
*  Downloaded data files can be found in [~/data/](https://github.com/longyyu/Stats506_FinalProj/tree/main/data)
*  Source code can be found in [final_project.R](https://github.com/longyyu/Stats506_FinalProj/blob/main/final_project.R)
    *  I mainly use R’s `data.table` package for computation
