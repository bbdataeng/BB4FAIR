
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
library(DT)





# Load Data ---------------------------------------------------------------
# tier_BB <- read_xlsx("data/punteggi_tiering.xlsx", sheet = "abb")    	
scores <- read_xlsx("./punteggi_tiering.xlsx", sheet = "punteggi_totali")
colnames(scores)[1] <- "BB_ID"
info_area <- read_xlsx("./quantitativa.xlsx", sheet = "razionale")
# questions <- read_xlsx("./quantitativa.xlsx", sheet = 4)


## add tiers
scores$tier <- ifelse(scores$punteggio_totale > 20, "Mature", 
                    ifelse(scores$punteggio_totale < 11, "Starting", "Advanced"))
macroareas <- info_area[1:14,c("domanda","area")]



# User Interface ----------------------------------------------------------
ui <- 
  ## navigation bar
  navbarPage(
    title = 'Digital Maturity Survey',
    tabPanel('Scores',     
 
  fluidPage(
  ## new row in the UI for selectInputs
  fluidRow(
	     column(4,selectInput("bb_name",
                       "Biobank ID:",
                       c("All",
                         unique(as.character(paste0("BB",scores$BB_ID)))))),
           column(4,selectInput("tier",
                       "Tier:",
                       c("All",
                         unique(as.character(scores$tier))))),
           column(4,selectizeInput("macroarea", 
                                   "Macro-areas:", 
                                   choices = c("All", unique(as.character(macroareas$area))),
                                multiple = TRUE)
		)
  ),
  
  DT::dataTableOutput("scores")

)),

tabPanel('Questions', DT::dataTableOutput('questions')),
tabPanel('Visualization',  
       fluidPage(
        fluidRow(
           column(3,plotOutput("density")),
           column(3,plotOutput("heatmap"))),
        fluidRow(
	     column(3,plotOutput("radar1")),
           column(3,plotOutput("radar2")),
           column(3,plotOutput("radar3")),
           column(3,plotOutput("legend"))
		    ))
))




# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  # filter data based on selections
  output$scores <- DT::renderDataTable(DT::datatable({
    data <- scores
    names(data)[names(data) == 'punteggio_totale'] <- 'total_score'
    data$BB_ID <- paste0("BB", data$BB_ID)
    
    if (input$bb_name != "All") {
      data <- data[data$BB_ID == input$bb_name,]
    }
    if (input$tier != "All") {
      data <- data[data$tier == input$tier,]
    }
 
    if (!"All" %in% input$macroarea & {!is.null(input$macroarea)}) {
      final_cols <- c("BB_ID")
      macro_scores <- c()
      
      for (selected_macroarea in input$macroarea) {
        colname <- paste0(selected_macroarea, "_score")
        macro_scores <- c(macro_scores, colname)
        # sum for the selected macroarea
        selected_cols <- macroareas$domanda[macroareas$area %in% selected_macroarea]
        final_cols <- c(final_cols, selected_cols)
        data[[colname]] <- rowSums(data[, selected_cols])}
      
    data <- data[, c(final_cols, "tier", macro_scores, "total_score")]
    }else{data <- data}

    data
    
  }))
  

  #preprocessing data long form for plots

  tier_norm_BB <- round(data.frame(scores[,1:16]) %>%
			mutate(personale.dedicato = personale.dedicato/max(personale.dedicato),
				 ontologie = ontologie/max(ontologie),
				 LIMS = LIMS/max(LIMS),
				 Infrastruttura.IT = Infrastruttura.IT/max(Infrastruttura.IT),
				 Componenti.IT = Componenti.IT/max(Componenti.IT), 
				 DWH = DWH/max(DWH),
				 annotazioni = annotazioni/max(annotazioni),
				 consenso.informato = consenso.informato/max(consenso.informato)
			),2)



  dimnames(tier_norm_BB)[[2]]<-c(
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
  "punteggio_totale"
  )
  


  tier_norm_BB$punt_norm<-apply(tier_norm_BB[,c(2:15)],1,sum)

  tier_norm_BB$tier <- ifelse(tier_norm_BB$punteggio_totale < 11, "Starting",
					ifelse(tier_norm_BB$punteggio_totale < 21, "Advanced", "Mature"))
  dati<-pivot_longer(tier_norm_BB, IT_head:informed_consent,names_to="facilities",values_to="score")
  dati<-data.frame(dati,
			 macro_areas=c(rep(c(rep("personnel",4),"infrastructure","data",rep("infrastructure",4),rep("data",4)),
			 nrow(tier_norm_BB))))
  dati$tier <- ifelse(dati$punteggio_totale < 11, "Starting",
                       ifelse(dati$punteggio_totale < 21, "Advanced", "Mature"))
  
  

  # preprocessing heatmap plot

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
  col_summary <- data.frame(tier=c("Starting","Advanced","Mature"),t(data.frame(Base = col_median3, Intermediate = col_median1, High = col_median2)))
  col_summary <- cbind(col_summary[,-c(7,13:15)],col_summary[,c(7,13:15)])
  
  # preprocessing legend

  etichette<-c("ITh","pers","onto","CDM","BIMS","ITi","store","ITc","DWH","regis","DM","anno","clin","IC")

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

  tabella <- tableGrob(data.frame(etichette = as.character(etichette), 
					    nuova_variabile = c(as.character(unique(dati$nuova_variabile)[1:5]),
									as.character(unique(dati$nuova_variabile)[7:11]),
									as.character(unique(dati$nuova_variabile)[c(6,12:14)]))),					
                       theme = mytheme, cols = c("", "record"))
  tabella$widths <- unit(tabella$widths + unit(0.5, "cm"), "cm")



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
            panel.spacing.x = unit(-3, "lines"))}, width = 384, height = 384)
 
 
  output$heatmap <- renderPlot({
   ggplot(dati, aes(x = biobank, y = nuova_variabile, fill = score)) +
     geom_tile(color = "black", linewidth = 0.7) +
     scale_fill_gradient(low="#244270", high="#EB5E00") +
     scale_y_discrete(position = "right",name="facilities") +
     theme(panel.background = element_blank(),
           axis.text.x = element_text(face = "bold", size = 9),  
           axis.text.y = element_text(face = "bold", size = 9, hjust = 1),
           axis.text.y.right = element_text(face = "bold", size = 9, margin = margin(l = -20)),
           axis.ticks.y.right = element_blank(),
           axis.title.y.right = element_text(face = "bold", margin = margin(l = 10)),
           axis.title.x = element_text(face = "bold"),
           axis.title.y = element_text(face = "bold"),
           strip.text = element_text(face = "bold", size = 10),
           legend.text = element_text(face = "bold"),
           legend.title = element_text(face = "bold"),
           strip.placement = "outside") +
     facet_grid(rows = vars(macro_areas), switch = "y", scale = "free_y")}, width = 1056, height = 384)
  
 output$radar1 <- renderPlot({
   ggradar(
    col_summary[1,],
    base.size = 8,
    font.radar = "Arial Black",
    values.radar = c("0%", "50%", "100%"),
    axis.labels = etichette,  
    plot.extent.x.sf = 1,
    plot.extent.y.sf = 1.2,
    label.centre.y = FALSE,
    grid.line.width = 0.3,
    grid.label.size = 3,
    label.gridline.min = TRUE,
    label.gridline.mid = TRUE,
    label.gridline.max = TRUE,
    axis.label.offset = 1.1,
    axis.label.size = 4,
    axis.line.colour = "#244270",
    group.line.width = 1.1,
    group.point.size = 2,
    group.colours = "#EC6707",
    background.circle.colour = "#244270",
    background.circle.transparency = 0.1,
    plot.legend = FALSE,
    legend.title = "",
    plot.title = paste(col_summary[1,1],"Tier" ),
    legend.text.size = 10,
    legend.position = "bottom",
    fill = TRUE,
    fill.alpha = 0.4) +
    theme_classic(base_family='Arial Black') +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
      axis.text = element_text(face = "bold"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
	legend.position = "none",
      plot.margin = margin(20, 20, 20, 20, "pt")) +
    labs(y = paste("Biobanks median score:"))}, width = 300, height = 300)
  
output$radar2 <- renderPlot({
   ggradar(
    col_summary[2,],
    base.size = 8,
    font.radar = "Arial Black",
    values.radar = c("0%", "50%", "100%"),
    axis.labels = etichette,  
    plot.extent.x.sf = 1,
    plot.extent.y.sf = 1.2,
    label.centre.y = FALSE,
    grid.line.width = 0.3,
    grid.label.size = 3,
    label.gridline.min = TRUE,
    label.gridline.mid = TRUE,
    label.gridline.max = TRUE,
    axis.label.offset = 1.1,
    axis.label.size = 4,
    axis.line.colour = "#244270",
    group.line.width = 1.1,
    group.point.size = 2,
    group.colours = "#EC6707",
    background.circle.colour = "#244270",
    background.circle.transparency = 0.1,
    plot.legend = FALSE,
    legend.title = "",
    plot.title = paste(col_summary[2,1],"Tier" ),
    legend.text.size = 10,
    legend.position = "bottom",
    fill = TRUE,
    fill.alpha = 0.4) +
    theme_classic(base_family='Arial Black') +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
      axis.text = element_text(face = "bold"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
	legend.position = "none",
      plot.margin = margin(20, 20, 20, 20, "pt")) +
    labs(y = paste("Biobanks median score:"))}, width = 300, height = 300)
  
output$radar3 <- renderPlot({
   ggradar(
    col_summary[3,],
    base.size = 8,
    font.radar = "Arial Black",
    values.radar = c("0%", "50%", "100%"),
    axis.labels = etichette,  
    plot.extent.x.sf = 1,
    plot.extent.y.sf = 1.2,
    label.centre.y = FALSE,
    grid.line.width = 0.3,
    grid.label.size = 3,
    label.gridline.min = TRUE,
    label.gridline.mid = TRUE,
    label.gridline.max = TRUE,
    axis.label.offset = 1.1,
    axis.label.size = 4,
    axis.line.colour = "#244270",
    group.line.width = 1.1,
    group.point.size = 2,
    group.colours = "#EC6707",
    background.circle.colour = "#244270",
    background.circle.transparency = 0.1,
    plot.legend = FALSE,
    legend.title = "",
    plot.title = paste(col_summary[3,1],"Tier" ),
    legend.text.size = 10,
    legend.position = "bottom",
    fill = TRUE,
    fill.alpha = 0.4) +
    theme_classic(base_family='Arial Black') +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold", size = 12),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
      axis.text = element_text(face = "bold"),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
	legend.position = "none",
      plot.margin = margin(20, 20, 20, 20, "pt")) +
    labs(y = paste("Biobanks median score:"))}, width = 300, height = 300)
  

output$legend <- renderPlot({
grid.arrange(tabella, nrow = 1)}, width = 300, height = 350)

}



# Run Shiny App -----------------------------------------------------------
shinyApp(ui = ui, server = server)


