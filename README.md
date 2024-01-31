
# R packages   
dplyr ggplot2 rmarkdown readr tidyverse readxl formattable tidyr RColorBrewer viridis fmsb stringr officer unikn    

# conda packages
conda install -y -c conda-forge r-dplyr r-ggplot2 r-rmarkdown r-readr r-tidyverse r-readxl r-formattable r-tidyr r-rcolorbrewer r-viridis r-fmsb r-stringr r-officer r-unikn

# howto compile from terminal 
Rscript -e "rmarkdown::render('quantitative_tiering.Rmd')"
 

