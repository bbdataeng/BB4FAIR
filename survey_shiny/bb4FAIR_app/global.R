## ---------------------------
##
## Script name: global.R
##
## Purpose of script: global script for shinyapp
##
## Author: BBDATAENG team 
##
## Date Created: 2025-01-22
##
## Copyright (c) Strengthening BBMRI.it
## Email:
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------



# Import libraries  ------------------------------------------------------------
library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(viridis)
library(ggplot2)
library(ggpubr)
library(ggradar)
library(tidyverse)
library(scales)
library(tibble)
library(hrbrthemes)
library(grid)
library(gridExtra)
library(DT)
library(RColorBrewer)
library(bs4Dash)
library(ggh4x)
library(waiter)
library(shinyWidgets)
library(spsComps)

# Utiliy Functions -------------------------------------------------------------

## Info Button on colnames
infoBtn <- function(id, title, content) {
  tag <- actionButton(
    id,
    label = "",
    icon = icon("info"),
    style = "info",
    size = "extra-small",
    class = 'btn action-button btn-info btn-xs shiny-bound-input',
  )
  return(
    bsPopover(
      tag = tag,
      title = title,
      content = content,
      placement = "top",
      trigger = "hover",
      html = TRUE
    )
  )
}


## Question Plots
source("./plot.R") # import plots

plots <- list(
  "Personnel dedicated to the Biobank" = function()
    chart_personnel,
  "Biobank personnel activities" = function()
    chart_personnel2,
  "Data annotation experience" = function()
    chart_personnel3,
  "Terminologies" = function()
    chart_term,
  "Personnel with experience in CDM" = function()
    chart_personnel4,
  "Common Data Models" = function()
    chart_cdm,
  "Personnel with experience in FHIR model" = function()
    chart_personnel5,
  "Biobank dedicated LIMS" = function()
    chart_lims,
  "BIMS in place" = function()
    chart_lims2,
  "Clinical data linkage" = function()
    chart_clindata,
  "Data storage without LIMS" = function()
    chart_nosist,
  "Informatic infrastructure for biobank" = function()
    chart_infrstr,
  "IT ifrstructure management" = function()
    chart_infrstr2,
  "Massive storage system" = function()
    chart_storage,
  "Federated Research" = function()
    chart_fed_search,
  "Resources for Federated Search services" = function()
    chart_fed_search2,
  "Data Warehouse" = function()
    chart_dwh,
  "Infrastructure for data storage" = function()
    chart_dwh2,
  "NGS laboratory" = function()
    chart_ngs,
  "NGS technology" = function()
    chart_ngs_tec,
  "'omics' platform" = function()
    chart_omics,
  "Laboratory accessibility" = function()
    chart_lab,
  "Laboratory specialization" = function()
    chart_lab2,
  "Data specialization" = function()
    chart_data,
  "Data type stored" = function()
    chart_data2,
  "Ontologies" = function()
    chart_onto,
  "Data sharing" = function()
    chart_network,
  # "Data network" = function()
  #   chart_network2,
  "Data crossing" = function()
    chart_data4,
  "System for cross-reference data" = function()
    chart_data5,
  "Informed consent" = function()
    chart_ic,
  "Informed consent model" = function()
    chart_ic_model
)
questions_labels <- names(plots)



## Assign questions to macroareas

info_area <- read_xlsx("./quantitative.xlsx", sheet = "razionale")
macroareas <- info_area[1:14, c("question", "area")]

# Load Data ---------------------------------------------------------------

## Scores Table 
scores <- read_xlsx("./tiering.xlsx")
colnames(scores)[1] <- "BB_ID"

## Add Tiers
scores$tier <- ifelse(
  scores$total_score > 20,
  "Mature",
  ifelse(scores$total_score < 11, "Starting", "Advanced")
)

scores <- scores %>% arrange(desc(total_score))
scores$BB_ID <- paste0("BB-", sample(100:999, size = nrow(scores), replace = FALSE)) # random name
                                    
# new tiering file
tier_BB <- read_xlsx("tiering6.xlsx")
scores$BB_ID <- tier_BB %>% arrange(desc(total_score)) %>% select(Biobank_ID)

# scores <- read_xlsx("tiering6.xlsx")

## Add info to colnames

# construct the title and convert to text for colnames info
text_1 <- tags$span("IT Head", infoBtn(
  'ffff',
  "IT head",
  paste(
    "If there is an IT infrastructure, who manages it?",
    "<br><i>Incomplete Response = 0",
    "Partial Response = 0",
    "Complete Response = 1</i>",
    sep = "<br>"
  )
), style = "white-space:nowrap") %>% as.character()
text_2 <- tags$span("Dedicated Personnel",
                    infoBtn(
                      'ffff',
                      "Dedicated Personnel",
                      paste(
                        "Does the organization where the Biobank is located as a service unit have dedicated personnel for biobank activities?",
                        "<br><i>Incomplete Response = 0",
                        "Partial Response = 1",
                        "Complete Response = 4</i>",
                        sep = "<br>"
                      )
                    ),
                    style = "white-space:nowrap") %>% as.character()
text_3 <- tags$span("Ontologies Richness",
                    infoBtn(
                      'Work',
                      "Ontologies Richness",
                      paste(
                        "Does the staff (dedicated or not) managing the Biobank have experience in data modeling and annotation? E.g. people that can associate clinical concepts and standard terminologies (SNOMED, LOINC,...) or relevant ontologies (OBIB).",
                        "<br><i>Incomplete Response = 0",
                        "Partial Response = 1",
                        "Complete Response = 3</i>",
                        sep = "<br>"
                      )
                    ),
                    style = "white-space:nowrap") %>% as.character()

text_4 <- tags$span("Common Data Models",
                    infoBtn(
                      'ffff',
                      "Common Data Models",
                      paste(
                        "Does the staff (dedicated or not) managing the Biobank have experience with Common Data Models and interoperable CDM formats like OMOP? By experience is meant the presence of datasets in these formats shared with national or international consortia.",
                        "<br><i>Incomplete Response = 0",
                        "Partial Response = 0",
                        "Complete Response = 1</i>",
                        sep = "<br>"
                      )
                    ),
                    style = "white-space:nowrap") %>% as.character()
text_5 <- tags$span(
  "BIMS",
  infoBtn(
    'ffff',
    "Biobanking Information Management System",
    paste(
      "Does the institution have an information system or LIMS-Database dedicated to the data associated with biological samples in the Biobank?",
      "<br><i>Incomplete Response = 0",
      "Partial Response = 1",
      "Complete Response = 8</i>",
      sep = "<br>"
    )
  ),
  style = "white-space:nowrap"
) %>% as.character()
text_6 <- tags$span("Data Management",
                    infoBtn(
                      'ffff',
                      "Data Management",
                      paste(
                        "If there is NO dedicated IT system, how are the data related to biological samples stored in the Biobank?",
                        "<br><i>Incomplete Response = 0",
                        "Partial Response = 0",
                        "Complete Response = 1</i>",
                        sep = "<br>"
                      )
                    ),
                    style = "white-space:nowrap") %>% as.character()
text_7 <- tags$span("Data Server",
                    infoBtn(
                      'ffff',
                      "Data Server",
                      paste(
                        "Does the institution have access to an IT infrastructure, physical or virtual, dedicated to the data or data processing associated with biological samples in the Biobank?",
                        "<br><i>Incomplete Response = 0",
                        "Partial Response = 1",
                        "Complete Response = 2</i>",
                        sep = "<br>"
                      )
                    ),
                    style = "white-space:nowrap") %>% as.character()
text_8 <- tags$span("Massive Storage",
                    infoBtn(
                      'ffff',
                      "Massive Storage",
                      paste(
                        "Does the Biobank have a massive storage system (capacity greater than 20TB), including backup or redundancy system (RAID), for research purposes?",
                        "<br><i>Incomplete Response = 0",
                        "Partial Response = 0",
                        "Complete Response = 1</i>",
                        sep = "<br>"
                      )
                    ),
                    style = "white-space:nowrap") %>% as.character()
text_9 <- tags$span("Service Resources",
                    infoBtn(
                      'ffff',
                      "Service Resources",
                      paste(
                        "Does the organization where the Biobank is located as a service unit have dedicated personnel for biobank activities?",
                        "<br><i>Incomplete Response = 0",
                        "Partial Response = 0",
                        "Complete Response = 1</i>",
                        sep = "<br>"
                      )
                    ),
                    style = "white-space:nowrap") %>% as.character()
text_10 <- tags$span("Data Warehouse",
                     infoBtn(
                       'ffff',
                       "Data Warehouse",
                       paste(
                         "Does the institution to which the Biobank belongs have a data collector (such as a Data Warehouse or Data Lake) and/or a Business Intelligence platform?",
                         "<br><i>Incomplete Response = 0",
                         "Partial Response = 0",
                         "Complete Response = 2</i>",
                         sep = "<br>"
                       )
                     ),
                     style = "white-space:nowrap") %>% as.character()
text_11 <- tags$span(
  "Clinical Data Availability",
  infoBtn(
    'ffff',
    "Clinical Data Availability",
    paste(
      "If there is a dedicated IT system, is it connected to the clinical data management system (e.g. clinical record systems or territorial health information systems)?",
      "<br><i>Incomplete Response = 0",
      "Partial Response = 0",
      "Complete Response = 1</i>",
      sep = "<br>"
    )
  ),
  style = "white-space:nowrap"
) %>% as.character()
text_12 <- tags$span("Annotations",
                     infoBtn(
                       'ffff',
                       "Annotations",
                       paste(
                         "What types of data are related to biobanked samples?",
                         "<br><i>Incomplete Response = 0",
                         "Partial Response = 1",
                         "Complete Response = 3</i>",
                         sep = "<br>"
                       )
                     ),
                     style = "white-space:nowrap") %>% as.character()
text_13 <- tags$span(
  "Registry Data Availability",
  infoBtn(
    'ffff',
    "Registry Data Availability",
    paste(
      "In compliance with ethical-legal requirements, is it possible to cross- reference sample data with data in the institution's or territorial systems?",
      "<br><i>Incomplete Response = 0",
      "Partial Response = 0",
      "Complete Response = 1</i>",
      sep = "<br>"
    )
  ),
  style = "white-space:nowrap"
) %>% as.character()
text_14 <- tags$span(
  "Informed Consent",
  infoBtn(
    'ffff',
    "Digitalized Informed Consent",
    paste(
      "Does the Biobank use a digital - electronic informed consent?",
      "<br><i>Incomplete Response = 0",
      "Partial Response = 0",
      "Complete Response = 3</i>",
      sep = "<br>"
    )
  ),
  style = "white-space:nowrap"
) %>% as.character()