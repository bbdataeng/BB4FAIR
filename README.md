# BB4FAIR: Implementation of a Digital Maturity Framework for Biobanking

:bangbang: Check out the pre-print of this work available on ![Zenodo](https://doi.org/10.5281/zenodo.14012403).

## Introduction
Biobanks must ensure a fully engineered and digitalized process towards data FAIRification. To this aim, the first step is to assess the current digitalization status with quantitative metrics, particularly challenging given the multi-faceted regulatory and logistics aspects of biobanking.

BB4FAIR is a Biobanking digital maturity framework, implemented in the #NextGenerationEu “Strenghtening BBMRI.it” project. 
It comprises of:
1) a survey with 38 questions divided in three macro areas, namely IT infrastructure, personnel, and data annotation richness
2) an automated R system to analyze and produce data visualization based on the survey results.

![graph_abs-GA](https://github.com/bbdataeng/BB4FAIR/assets/51079644/a4f62d7a-222d-48b1-83b8-356edc262542)


### Requirements

#### R packages   
dplyr ggplot2 rmarkdown readr tidyverse readxl formattable tidyr RColorBrewer viridis fmsb stringr officer unikn    

#### how to install conda packages
```shell
conda install -y -c conda-forge r-dplyr r-ggplot2 r-rmarkdown r-readr r-tidyverse r-readxl r-formattable r-tidyr r-rcolorbrewer r-viridis r-fmsb r-stringr r-officer r-unikn
```

### Quantitative Tiering

#### how to compile from terminal 
```shell
Rscript -e "rmarkdown::render('quantitative_tiering.Rmd')"
```

### Visualization
![Tier_Viz](https://github.com/bbdataeng/BB4FAIR/blob/main/visualization/Tier_Viz.png)


### Shiny App
https://bbdataeng.shinyapps.io/bb4FAIR_app/

or using BB4FAIR docker image:
```shell
$ docker build -t bb4fair .
```

```shell
$ docker run -it --rm -p 3838:3838 bb4fair
```
use BB4FAIR shiny in a browser by typing http://localhost:3838
