# Base R Shiny image
FROM rocker/shiny

# Install required R packages
RUN R -e "install.packages(c('shiny', 'readxl', 'dplyr', 'tidyverse', 'viridis', 'ggplot2', 'ggpubr', 'devtools', 'scales', 'tibble', 'hrbrthemes', 'grid', 'gridExtra', 'DT', 'RColorBrewer', 'shinydashboard', 'bs4Dash', 'ggh4x', 'waiter','shinyWidgets', 'spsComps', "flexdashboard"))"

RUN R -e "devtools::install_github('ricardo-bion/ggradar', dependencies = TRUE)"

# Copy the Shiny app files to the Docker image
COPY ./survey_shiny/bb4FAIR_app /app

# Set the working directory
WORKDIR /app

# Expose the port number the Shiny app runs on
EXPOSE 3838

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/app', port = 3838, host = '0.0.0.0')"]

