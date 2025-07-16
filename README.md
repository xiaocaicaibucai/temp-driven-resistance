# Climate Warming and Insecticide Resistance Evolution in *Nilaparvata lugens*

**Author**: Tingwei Cai  
**Last Updated**: July 16, 2025  
**Affiliations**: Huazhong Agricultural University / University of British Columbia  
**Contact**: caitingwei@mail.hzau.edu.cn

---

## 🔍 Purpose

This repository contains the complete R script and datasets for the study:

**"Climate warming accelerates the evolution of insecticide resistance in *Nilaparvata lugens*"**

The script is designed to:
- Visualize spatial and temporal patterns of resistance monitoring
- Quantify trends in resistance ratio (RR) and LC50 values
- Evaluate the correlation between temperature and resistance evolution rate

---

## 📁 File Structure

| File / Folder | Description |
|---------------|-------------|
| `RR.csv` | Resistance Ratio (RR) data per year, pesticide, and population |
| `LC50.csv` | LC50 values by pesticide and year |
| `CWIRE.R` | Main R script for all analyses and figures |
| `README.md` | This documentation file |

---

## ⚙️ Dependencies

This script requires R (version ≥ 4.1) and the following R packages:

```r
data.table
readr
patchwork
ggpubr
ggrepel
ggplot2
ggsci
tidyverse
ggmap
sf
rnaturalearth
rnaturalearthdata
broom
dplyr
