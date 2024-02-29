# BB4FAIR: Implementation of a Digital Maturity Framework for Biobanking

## Introduction
Biobanks must ensure a fully engineered and digitalized process towards data FAIRification. To this aim, the first step is to assess the current digitalization status with quantitative metrics, particularly challenging given the multi-faceted regulatory and logistics aspects of biobanking.

BB4FAIR is a Biobanking digital maturity framework, implemented in the #NextGenerationEu "Strenghtening BBMRI.it" project. 

It comprises of:
1) A survey with 38 questions divided in three macro areas, namely IT infrastructure, personnel, and data annotation richness
2) An automated R system to analyze and produce data visualization based on the survey results.


![graph_abs-GA](https://github.com/bbdataeng/BB4FAIR/assets/51079644/a4f62d7a-222d-48b1-83b8-356edc262542)

### Requirements

#### R packages   
Ensure you have the following R packages installed:

```shell
conda install -y -c conda-forge r-dplyr r-ggplot2 r-rmarkdown r-readr r-tidyverse r-readxl r-formattable r-tidyr r-rcolorbrewer r-viridis r-fmsb r-stringr r-officer r-unikn
```


### Quantitative Tiering
`quantitative_tiering.Rmd` is designed to process the data from the survey responses and to generate an output Excel file `tiering.xlsx` containing the scores and the division of the biobanks into tiers, according to the ranges defined in the `quantitative.xlsx` file.

To compile the quantitative tiering report from the terminal, use the following command:

```shell
Rscript -e "rmarkdown::render('quantitative_tiering.Rmd')"
```


### Visualization
![Tier_Viz](https://github.com/bbdataeng/BB4FAIR/blob/main/visualization/Tier_Viz.png)


### Shiny App
Explore the BB4FAIR Shiny App [here](https://bbdataeng.shinyapps.io/bb4FAIR_app/).
