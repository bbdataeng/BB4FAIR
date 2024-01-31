
# Import libraries  -------------------------------------------------------
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




# Load Data ---------------------------------------------------------------
# tier_BB <- read_xlsx("data/punteggi_tiering.xlsx", sheet = "abb")    	
scores <- read_xlsx("./punteggi_tiering.xlsx", sheet = "punteggi_totali")
info_area <- read_xlsx("./quantitativa.xlsx", sheet = "razionale")
# questions <- read_xlsx("./quantitativa.xlsx", sheet = 4)


## add tiers
scores$tier <- ifelse(scores$punteggio_totale > 20, "Advanced", 
                    ifelse(scores$punteggio_totale < 11, "Base", "Intermediate"))
macroareas <- info_area[1:14,c("domanda","area")]






# User Interface ----------------------------------------------------------


ui <- 
  ## navigation bar
  navbarPage(
    title = 'Digital Maturity Survey',
    header = "test",
    tabPanel('Scores',     
 
  fluidPage(
  ## new row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("bb_name",
                       "Biobank ID:",
                       c("All",
                         unique(as.character(scores$Abb))))
    ),
    column(4,
           selectInput("tier",
                       "Tier:",
                       c("All",
                         unique(as.character(scores$tier))))
    ),
    column(4,
           selectInput("macroarea",
                       "Macro-area:",
                       c("All",
                         unique(as.character(macroareas$area))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("scores")
)),
tabPanel('Questions', DT::dataTableOutput('questions')),
tabPanel('Visualization',  
       fluidPage(
        ## new row in the UI for selectInputs
        fluidRow(
           column(4,
                  plotOutput("heatmap")
         ),
         column(4,
                plotOutput("radars")
         ),
         column(4,
                plotOutput("legend")
         ),
         column(4,
                plotOutput("density")
         )))
))

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  # filter data based on selections
  output$scores <- DT::renderDataTable(DT::datatable({
    data <- scores
    if (input$bb_name != "All") {
      data <- data[data$Abb == input$bb_name,]
    }
    if (input$tier != "All") {
      data <- data[data$tier == input$tier,]
    }
    if (input$macroarea != "All") {
      selected_cols <- c(Abb,
                         macroareas$domanda[macroareas$area %in% c(input$macroarea)])
      data_filtered <- data[, colnames(data) %in%  selected_cols]
      data_filtered$punteggio_area <- apply(data_filtered[,-1], 1, sum)
      data_filtered$punteggio_totale <- data$punteggio_totale
      data <- data_filtered
    }
    data
  }))
  
  # output$questions <-DT::renderDataTable(DT::datatable({
  #   data <- questions}))
  




  tier_BB <- data.frame(cbind(data.frame(apply(scores[,1:15], 2, as.factor)), scores[,16]))
  
  tier_norm_BB <- tier_BB %>%
  			mutate(personale.dedicato = as.numeric(recode(personale.dedicato, "0"="0","1"="0.25","4"="1")),
  				 ontologie = as.numeric(recode(ontologie, "0"="0","1"="0.33","3"="1")),
  				 LIMS = as.numeric(recode(LIMS, "0"="0","1"="0.125","8"="1")),
  				 Infrastruttura.IT = as.numeric(recode(Infrastruttura.IT, "0"="0","1"="0.5", "2"="1")),
  				 Componenti.IT = as.numeric(recode(Componenti.IT, "0"="0","1"="0.5", "2"="1")), 
  				 DWH = as.numeric(recode(DWH, "0"="0", "2"="1")),
  				 annotazioni = as.numeric(recode(annotazioni, "0"="0","1"="0.5", "2"="1")),
  				 consenso.informato = as.numeric(recode(consenso.informato, "0"="0", "3"="1"))
  			)
  tier_norm_BB <- data.frame(apply(tier_norm_BB, 2, as.numeric))
  
  dati<-pivot_longer(tier_norm_BB, referente.IT:punteggio_totale,names_to="facilities",values_to="score")
  
  names(tier_norm_BB) <-c(
  "biobank",
  "IT_head",
  "dedicated_personnel",
  "ontologies_richness",
  "common_data_models",
  "BIMS",
  "data_management",
  "IT_infrastructures",
  "massive_storage",
  "IT_components",
  "data_warehouse",
  "registry_data_availability",
  "annotations",
  "clinical_data_availability",
  "informed_consent",
  "personnel",
  "infrastructure",
  "data",
  "punteggio_totale"
  )
  
  
  tier_norm_BB$punt_norm<-apply(tier_norm_BB[,c(2:15)],1,sum)
  
  tier_norm_BB$tier <- ifelse(tier_norm_BB$punteggio_totale < 11, "Base",
  					ifelse(tier_norm_BB$punteggio_totale < 21, "Intermediate", "Advance"))
  
  tier_norm_BB$tier_norm <- ifelse(tier_norm_BB$punt_norm < 4.62, "Base",
                         		   ifelse(tier_norm_BB$punt_norm < 9.24, "Intermediate", "Advance"))
  
  tier_norm_BB$tier_quart <- ifelse(tier_norm_BB$punt_norm < 3.5, "Low",
                         		   ifelse(tier_norm_BB$punt_norm < 7, "Base",
  							ifelse(tier_norm_BB$punt_norm < 10.5, "Intermediate", "Advance")))
  
  head(tier_norm_BB)
  
  dati<-pivot_longer(tier_norm_BB, IT_head:informed_consent,names_to="facilities",values_to="score")
  dati<-data.frame(dati,macro_areas=c(rep(c(rep("personnel",4),rep("infrastructure",6),rep("data",4)),nrow(tier_norm_BB))))
  dati$tier <- ifelse(dati$punteggio_totale < 11, "Base",
                         ifelse(dati$punteggio_totale < 21, "Intermediate", "Advance"))
  
  nuovi_nomi_variabili <- c(
  "IT head",
  "dedicated personnel",
  "ontologies richness",
  "common data models",
  "BIMS",
  "data management",
  "IT infrastructures",
  "massive storage",
  "IT components",
  "data warehouse",
  "registry data availability",
  "annotations",
  "clinical data availability",
  "informed consent")
  
  
  
  dati$nuova_variabile <- factor(dati$facilities, levels = unique(dati$facilities), labels = nuovi_nomi_variabili)
  
  # preprocessing radar plot
  lista_tier <- tier_norm_BB %>% group_split(tier)
  
  col_median1 <- apply(lista_tier[[1]][,c(2:15)], 2, median)
  col_median2 <- apply(lista_tier[[2]][,c(2:15)], 2, median)
  col_median3 <- apply(lista_tier[[3]][,c(2:15)], 2, median)
  col_summary <- data.frame(tier=c("Starting","Advanced","Mature"),t(data.frame(Base = col_median1, Medium = col_median3, High = col_median2)))
  
  
  # preprocessing legend
  etichette<-c("ITh","pers","onto","CDM","BIMS","DM","ITi","store","ITc","DWH","regis","anno","clin","IC")
  
  etichette <- paste0("    ", etichette)
  
  mytheme <- ttheme_default(
    core = list(
      fg_params = list(hjust = 0, x = 0.1, fontsize = 12),
      padding = unit(c(0.5, 0.5), "cm")
    ),
    colhead = list(
      fg_params = list(fontsize = 12, fontface="bold"),
      padding = unit(c(0.5, 0.5), "cm")
    )
  )
  
  tabella <- tableGrob(data.frame(etichette = as.character(etichette), nuova_variabile = as.character(unique(dati$nuova_variabile))), 
                       theme = mytheme, cols = c("", "record"))
  tabella$widths <- unit(tabella$widths + unit(0.5, "cm"), "cm")
  
  
  output$heatmap <- renderPlot({
   ggplot(dati, aes(x = biobank, y = nuova_variabile, fill = score)) +
     geom_tile(color = "black", linewidth = 0.7) +
     scale_fill_gradient(low="#244270", high="#EB5E00") +
     scale_y_discrete(position = "right",name="facilities") +
     theme(panel.background = element_blank(),
           axis.text.x = element_text(face = "bold", size = 9),  
           axis.text.y = element_text(face = "bold", size = 9, hjust = 1),
           axis.text.y.right = element_text(face = "bold", size = 9),
           # axis.ticks.y.right = element_line( margin = margin(l = -10)),
           axis.title.y.right = element_text(face = "bold", margin = margin(l = 10)),
           axis.title.x = element_text(face = "bold"),
           axis.title.y = element_text(face = "bold"),
           strip.text = element_text(face = "bold", size = 10),
           legend.text = element_text(face = "bold"),
           legend.title = element_text(face = "bold"),
           strip.placement = "outside") +
     facet_grid(rows = vars(macro_areas), switch = "y", scale = "free_y")}, width = 1056, height = 384)
     # width = 10560, height = 3840, res = 300)
  
  
  output$density <- renderPlot({ggplot(dati)+
      geom_density(aes(x = score), fill="#244270", alpha = 0.8) +
      facet_grid(macro_areas~., switch = "y")+
      scale_y_continuous(name = NULL, sec.axis = sec_axis(~., name = "density")) +
      theme(panel.background = element_blank(),
            axis.text.x = element_text(face = "bold",size = 10),  
            axis.text.y.right = element_text(face = "bold",size = 10, hjust = 1),
            axis.title.x = element_text(face = "bold"),
            axis.title.y.right = element_text(face = "bold", margin = margin(l = 15)),
            strip.text.y = element_text(face = "bold",size = 10),
            legend.text = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.position="none",
            strip.placement = "outside",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.line.y.right = element_line(color = "grey30"),
            axis.ticks.x = element_line(color = "grey30"),
            axis.line.x = element_blank(),
            axis.ticks.y.right = element_line(color = "grey30"),
            panel.spacing.x = unit(-3, "lines"))})
  
  
  
  


}

# Run Shiny App -----------------------------------------------------------
shinyApp(ui = ui, server = server)



## Istruzioni per il deployment -------------------------------------------
# library(rsconnect)
# 
# rsconnect::setAccountInfo(name='bbdataeng',
#                           token='1A6630C592D9A63FD1C8D65C0F08F390',
#                           secret='SLd4aCbiTMTzLbzh1QVn2/egA2b0H5JpLay6/iSG')
# 
# rsconnect::deployApp('bb4FAIR_app')
