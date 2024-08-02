
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
library(RColorBrewer)
# library(shinydashboard)
library(bs4Dash)
library(ggh4x)
library(waiter)
library(shinyWidgets)


# toast options
toastOpts <- list(
  autohide = TRUE,
  icon = "fas fa-home",
  close = FALSE,
  position = "bottomRight"
)

# Load Data ---------------------------------------------------------------
# tier_BB <- read_xlsx("/punteggi_tiering.xlsx", sheet = "abb")  

scores <- read_xlsx("./tiering.xlsx", sheet = "total_score")
# 
# scores <- read_xlsx("./punteggi_tiering.xlsx", sheet = "punteggi_totali")
colnames(scores)[1] <- "BB_ID"

info_area <- read_xlsx("./quantitative.xlsx", sheet = "razionale")
# questions <- read_xlsx("./quantitativa.xlsx", sheet = 4)


## add tiers
scores$tier <- ifelse(scores$total_score > 20, "Mature", 
                      ifelse(scores$total_score < 11, "Starting", "Advanced"))
macroareas <- info_area[1:14,c("question","area")]
macroareas$question <- str_replace(str_replace(macroareas$question, " ", "_"), "-", "_")

scores <- scores %>%arrange(desc(total_score))
scores$BB_ID <- paste0("BB", seq(1:dim(scores)[1]))

## question labels
source("./plot.R") # import plots 

plots <- list(
  "Personnel dedicated to the Biobank" = function() chart_personnel,
  "Biobank personnel activities" = function() chart_personnel2,
  "Data annotation experience" = function() chart_personnel3,
  "Terminologies" = function() chart_term,
  "Personnel with experience in CDM" = function() chart_personnel4,
  "Common Data Models" = function() chart_cdm,
  "Personnel with experience in FHIR model" = function() chart_personnel5,
  "Biobank dedicated LIMS" = function() chart_lims,
  "BIMS in place" = function() chart_lims2,
  "Clinical data linkage" = function() chart_clindata,
  "Data storage without LIMS" = function() chart_nosist,
  "Informatic infrastructure for biobank" = function() chart_infrstr,
  "IT ifrstructure management" = function() chart_infrstr2,
  "Massive storage system" = function() chart_storage,
  "Federated Research" = function() chart_fed_search,
  "Resources for Federated Search services" = function() chart_fed_search2,
  "Data Warehouse" = function() chart_dwh,
  "Infrastructure for data storage" = function() chart_dwh2,
  "NGS laboratory" = function() chart_ngs,
  "NGS technology" = function() chart_ngs_tec,
  "'omics' platform" = function() chart_omics,
  "Laboratory accessibility" = function() chart_lab,
  "Laboratory specialization" = function() chart_lab2,
  "Data specialization" = function() chart_data,
  "Data type stored" = function() chart_data2,
  "Ontologies" = function() chart_onto,
  "Data sharing" = function() chart_network,
  "Data network" = function() chart_network2,
  "Data crossing" = function() chart_data4,
  "System for cross-reference data" = function() chart_data5,
  "Informed consent" = function() chart_ic,
  "Informed consent model" = function() chart_ic_model
)
questions_labels <- names(plots)
# User Interface ----------------------------------------------------------
ui <- 
  dashboardPage(preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#001f3f"),
                dashboardHeader(skin = "navy",
                                title = div(
                                  style = "display: flex; align-items: center; justify-content: center; font-size: 18px; padding: 15px",
                                  "BB4FAIR"
                                ),
                                tags$li(
                                  class = "dropdown",
                                  style = "display: flex; align-items: center; width: 100%;",
                                  fluidRow(
                                    column(
                                      width = 6.5, 
                                      offset = 1,
                                      align = "center",
                                      span("Digital Maturity Survey for Biobanking", style = "font-size: 30px; color: white;")
                                    ),
                                    column(
                                      width = 2,
                                      # offset = 1,
                                      align = "center",
                                      img(src = 'Italy_grande_str_bbmri_bianco.png', style = "width: 70%;")
                                    )
                                  )
                                )
                ),
                
                sidebar = dashboardSidebar(
                  status = "navy", 
                  sidebarMenu(id = "tabs",
                              menuItem(text = "Scores", tabName = "Scores", icon = icon("table")),
                              menuItem(text = "Feature Analysis", tabName = "Answers", icon = icon("chart-simple")),
                              menuItem(text = "Visualization", tabName = "Visualization", icon = icon("chart-area")),
                              menuItem(text = "Tiering", tabName = "Tiering", icon = icon("star"))
                  )
                ),
                
                body = dashboardBody(
                  tabItems(
                    tabItem('Scores',     
                            titlePanel(""),
                            br(),
                            fluidRow(
                              column(4, selectInput("bb_name", "Biobank ID:", c("All", unique(as.character(scores$BB_ID))))),
                              column(4, selectInput("tier", "Tier:", c("All", unique(as.character(scores$tier))))),
                              column(4, selectizeInput("macroarea", "Macro-areas:", choices = c("All", unique(as.character(macroareas$area))), multiple = TRUE))
                            ),
                            DT::dataTableOutput("scores")
                    ),
                    tabItem('Answers', 
                            titlePanel(""),
                            fluidRow(
                              br(),
                              bs4Card(title= "Question", HTML("<p>Explore the answers to different questions by selecting from the list below.</p>"), selectizeInput(inputId = "question", label = NULL, choices = questions_labels, multiple = FALSE), collapsible = FALSE),
                              bs4Card(tags$style("#graph {margin-top: 50px;}"), plotOutput("graph", height = "60vh", width="100%"), collapsible = FALSE)
                            )
                    ),
                    tabItem('Visualization', fluidPage(
                      br(),
                      fluidRow(
                        column(3.5, plotOutput("density")),
                        column(4, plotOutput("heatmap"))
                      ),
                      br(),
                      fluidRow(
                        column(width = 3, class = "col-sm-3", plotOutput("radar1")),
                        column(width = 3, class = "col-sm-3", plotOutput("radar2")),
                        column(width = 3, class = "col-sm-3", plotOutput("radar3")),
                        column(width = 3, class = "col-sm-3", plotOutput("legend"))
                      )
                    )),
                    tabItem('Tiering',
                            titlePanel("Evaluation of your Biobank"),
                            br(),
                            fluidRow(
                              column(4,
                                     bs4Card(title = "Select your Biobank ID", selectInput(inputId = "bb_name_plot", label = NULL, choices = unique(as.character(scores$BB_ID))), width = 10, headerBorder = FALSE, collapsible = FALSE),
                                     valueBoxOutput("tiering_valueBox", width = 10),
                                     bs4Card(title = "Ranking Position", label = NULL, uiOutput("slider"), width = 10, collapsible = FALSE)
                              ),
                              column(8,
                                     bs4Card(plotOutput("eval_plot"), class = "plot-box", width = 12, headerBorder = FALSE, collapsible = FALSE)
                              )
                            ),
                            fluidRow(
                              column(12,
                                     bs4Card(title = "Suggestions", " ", width = 12, headerBorder = FALSE, collapsed = TRUE)
                              )
                            )
                    )
                  )
                ),
                
                controlbar = dashboardControlbar(title = "Digital Maturity Survey"),
                footer = bs4DashFooter(left = "bbdataeng-team", right = "Strengthening BBMRI.it", fixed = FALSE)
  )




# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  # filter data based on selections
  output$scores <- DT::renderDataTable(DT::datatable({
    data <- scores
    colnames(data)[2:15] <- macroareas$question
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
        selected_cols <- macroareas$question[macroareas$area %in% selected_macroarea]
        final_cols <- c(final_cols, selected_cols)
        data[[colname]] <- rowSums(data[, selected_cols])}
      
      data <- data[, c(final_cols, "tier", macro_scores, "total_score")]
    }else{data <- data}
    
    data
  }, options = list(
    scrollX = TRUE,
    autoWidth = TRUE,
    # columnDefs = list(list(width = '2px', targets = "_all")),
    extensions = c('Buttons', 'Scroller'),
    buttons = list('excel'),
    dom = 'lBfrtip'
  )))
  
  #preprocessing data long form for plots
  scores_forplot <- scores%>%arrange(desc(total_score))
  # scores <- scores%>%arrange(desc(total_score))
  tier_norm_BB <- round(scores_forplot[,2:16] %>%
                          mutate(dedicated_personnel = dedicated_personnel/max(dedicated_personnel),
                                 ontologies_richness = ontologies_richness/max(ontologies_richness),
                                 BIMS = BIMS/max(BIMS),
                                 IT_infrastructures = IT_infrastructures/max(IT_infrastructures),
                                 IT_components = IT_components/max(IT_components), 
                                 data_warehouse = data_warehouse/max(data_warehouse),
                                 annotations = annotations/max(annotations),
                                 informed_consent = informed_consent/max(informed_consent)
                          ),2)
  tier_norm_BB$biobank <- seq(1,47)
  
  tier_norm_BB$tier <- ifelse(tier_norm_BB$total_score < 11, "Starting",
                              ifelse(tier_norm_BB$total_score < 21, "Advanced", "Mature"))
  
  
  dati<-pivot_longer(tier_norm_BB, IT_head:informed_consent,names_to="facilities",values_to="score")
  dati<-data.frame(dati,
                   macro_areas=c(rep(c(rep("personnel",4),rep("infrastructure",5),rep("data",5)),
                                     nrow(tier_norm_BB))))
  

  
  
  
  
  # Heatmap -----------------------------------------------------------
  
  # Preprocessing data
  
  new_names <- c(
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
    "clinical data availability",
    "annotations",
    "registry data availability",
    "informed consent")
  
  dati$nuova_variabile <- factor(dati$facilities, levels = unique(dati$facilities), labels = new_names)
  
  # preprocessing radar plot
  lista_tier <- tier_norm_BB %>% group_split(tier)
  
  col_median1 <- apply(lista_tier[[1]][,c(1:14)], 2, median)
  col_median2 <- apply(lista_tier[[2]][,c(1:14)], 2, median)
  col_median3 <- apply(lista_tier[[3]][,c(1:14)], 2, median)
  col_summary <- data.frame(tier=c("Starting","Advanced","Mature"),t(data.frame(Base = col_median3, Intermediate = col_median1, High = col_median2)))
  # col_summary <- cbind(col_summary[,-c(7,13:15)],col_summary[,c(7,13:15)])
  
  # preprocessing legend
  
  # etichette <-c("ITh","pers","onto","CDM","BIMS","ITi","store","ITc","DWH",
  #               "clin","DM","anno","regis","IC")
  etichette <-c("ITh","pers","onto","CDM","BIMS","DM","ITi","store","ITc","DWH",
                "clin","anno","regis","IC")
  
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
                                  nuova_variabile = c(as.character(unique(dati$nuova_variabile)))),					
                       theme = mytheme, cols = c("", "record"))
  tabella$widths <- unit(tabella$widths + unit(0.5, "cm"), "cm")
  
  
# LINE PLOT
# Data processing  
  
  dati1 <- dati
  dati1$tier <- factor(dati1$tier, levels = unique(dati1$tier))
  dati1$facilities <- factor(dati1$facilities, levels = unique(dati1$facilities))
  dati1$macro_areas <- factor(dati1$macro_areas, levels = unique(dati1$macro_areas)) 
  
  BB_data <- vector(mode='list', length=length(unique(dati1$biobank)))
  
  for(i in 1:length(unique(dati1$biobank))){
    BB_data[[i]] <- dati1[which(dati1$biobank == i),]
  }
  
  BB_median <- dati1 %>%
    group_by(macro_areas,nuova_variabile) %>%
    summarise(score = median(score))
  BB_median$Italian_median_trend <- rep("Italian median trend", dim(BB_median)[1])
  
  
  for(i in 1:length(BB_data)){
    BB_data[[i]]$score_M <- BB_median$score
    BB_data[[i]]$diff <- BB_data[[i]]$score - BB_data[[i]]$score_M
    names(BB_data)[i] <- paste0("BB",BB_data[[i]]$biobank[1])
  }
  
  BB_data_fill <- BB_data
  
  for(i in 1:length(BB_data)){
    BB_data_fill[[i]]$nuova_variabile <- seq(1:14)
  }
  
  for(i in 1:length(BB_data)){
    BB_data_fill[[i]]$BB_ID <- paste0("BB",BB_data_fill[[i]]$biobank)
    names(BB_data_fill)[i] <- BB_data_fill[[i]]$BB_ID[1]
    }

  
  

  output$density <- renderPlot({ggplot(dati)+
                                        geom_density(aes(x = score), fill="#244270", alpha = 0.8) +
                                        facet_grid(macro_areas~., switch = "y")+
                                        scale_y_continuous(name = NULL, sec.axis = sec_axis(~., name = "density")) +
                                        theme(panel.background = element_blank(),
                                              axis.text.x = element_text(color = "black", face = "bold",size = 10),  
                                              axis.text.y.right = element_text(color = "black", face = "bold",size = 10, hjust = 1),
                                              axis.title.x = element_text(face = "bold"),
                                              axis.title.y.right = element_text(face = "bold", margin = margin(l = 15)),
                                              strip.text.y = element_text(face = "bold",size = 13),
                                              legend.text = element_text(face = "bold"),
                                              legend.title = element_text(face = "bold"),
                                              legend.position="none",
                                              strip.placement = "outside",
                                              axis.text.y = element_blank(),
                                              axis.ticks.y = element_blank(),
                                              axis.title.y = element_blank(),
                                              axis.line.y.right = element_line(color = "black"),
                                              axis.ticks.x = element_line(color = "black"),
                                              axis.line.x = element_line(color = "black"),
                                              axis.ticks.y.right = element_line(color = "black"),
                                              panel.spacing.x = unit(-1, "lines"))+
                                        scale_x_continuous(expand = c(0.01, 0.01), 
                                                           labels = c("0%", "25%", "50%", "75%", "100%"))}, 
                               width = 345.6, height = 345.6, execOnResize = FALSE)
                                    
  
  output$heatmap <- renderPlot({ggplot(dati, aes(x = biobank, y = nuova_variabile, fill = score)) +
                                  geom_tile(color = "black", linewidth = 0.7) +
                                  scale_fill_gradient(low = "#244270", high = "#EB5E00") +
                                  scale_y_discrete(position = "right", name = "Facilities") +
                                  labs(x = "Biobank name") +  
                                  theme(panel.background = element_blank(),
                                        axis.text.x = element_text(color = "black", face = "bold", size = 10),
                                        axis.text.y = element_text(color = "black", face = "bold", size = 10, hjust = 1),
                                        axis.text.y.right = element_text(face = "bold", size = 11),
                                        axis.title.y.right = element_text(face = "bold", margin = margin(l = 10)),
                                        axis.ticks.y.right = element_blank(),
                                        axis.ticks.x.bottom = element_blank(),
                                        axis.title.x = element_text(face = "bold",margin = margin(t = 10)),
                                        axis.title.y = element_text(face = "bold"),
                                        strip.text = element_text(face = "bold", size = 13),
                                        legend.text = element_text(face = "bold", size = 11),
                                        legend.title = element_text(face = "bold", , size = 11, margin = margin(b = 10)),
                                        strip.placement = "inside") +
                                  facet_grid(rows = vars(macro_areas), switch = "y", scale = "free") +
                                  scale_x_continuous(expand = c(0.01, 0.01), breaks = seq(min(dati$biobank), max(dati$biobank), by = 1))}, 
                                width = 950.4, height = 345.6, execOnResize = FALSE)

  
  
  output$radar1 <- renderPlot({ggradar(
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
                                plot.title = paste0("Tier 1: ",col_summary[3,1]),
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
                                labs(y = paste("Biobanks median score:"))}, width = 300, height = 300, execOnResize = FALSE)
                            
  
  output$radar2 <- renderPlot({ggradar(
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
                                  plot.title = paste0("Tier 2: ",col_summary[2,1]),
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
                                  labs(y = paste("Biobanks median score:"))}, width = 300, height = 300, execOnResize = FALSE)
  
  
  output$radar3 <- renderPlot({ggradar(
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
                                    plot.title = paste0("Tier 3: ",col_summary[1,1]),
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
                                    labs(y = paste("Biobanks median score:"))}, width = 300, height = 300, execOnResize = FALSE)
  
  
  output$legend <- renderPlot({grid.arrange(tabella, nrow = 1)}, width = 300, height = 350, execOnResize = FALSE)
  
  
  
  
names(BB_data_fill)
  
  ## aggiungere plot singole biobanche


    output$eval_plot <- renderPlot({ggplot(BB_data_fill[[input$bb_name_plot]], aes(x = nuova_variabile, y = score, group = biobank)) +
                                          ggtitle(paste0("Italian Biobanks Digital Maturity Model: Biobank number ", input$bb_name_plot)) + 
                                            xlab("facilities") +
                                            ylab("score") +
                                            stat_difference(aes(ymin = score, ymax = score_M),
                                                            levels = c("-", "+"),
                                                            alpha  = 1,
                                                            show.legend = FALSE) +
                                            scale_fill_manual(values = c("+" = "darkgreen", "-" = "darkred")) +
                                            geom_line(aes(y = score, color = "navy"), linewidth = 4) +
                                            geom_point(fill = "navy", shape=21, color = "black", size = 5) +
                                            geom_line(aes(y= score_M, group = 1, color = "lightblue4"), linewidth = 1.5, linetype = "11") +
                                            scale_color_manual(values = c("navy" = "navy", "lightblue4" = "lightblue4"),
                                                               labels = c("navy" = paste0("Biobank ", str_sub(input$bb_name_plot, start = 3)," Score"), "lightblue4" = "BBMRI.it trend"),
                                                               name = "") +
                                            theme_classic() + 
                                            theme(plot.title = element_text(color = "black", face = "bold", size = 13.5, margin = margin(0,0,20,0)),
                                                  axis.line.x = element_line(linewidth = 1.3),
                                                  axis.line.y = element_line(linewidth = 1.3),
                                                  axis.text.x = element_text(color = "black", face = "bold", 
                                                                             size = 10, hjust = 0.5, vjust = 0.3, margin = margin(t = 7)),
                                                  axis.text.y.right = element_text(margin = margin(l = 10)),
                                                  axis.ticks.y.right = element_line(color = "black",linewidth = 1),
                                                  axis.ticks.length.y.right = (unit(0.12, "cm")),
                                                  axis.text.y = element_text(color = "black", face = "bold", size = 10),
                                                  axis.title.x = element_text(color = "black", face = "bold", size = 11),
                                                  axis.title.y = element_text(color = "black", face = "bold", size = 11),
                                                  axis.ticks.x = element_line(color = "black",linewidth = 1),
                                                  axis.ticks.length.x = (unit(0.12, "cm")),
                                                  strip.text = element_text(face = "bold", size = 11),
                                                  strip.background = element_rect(linewidth = 2),
                                                  strip.placement = "inside",
                                                  legend.position = "bottom",
                                                  legend.direction = "horizontal",
                                                  legend.margin = margin(-5,10,0,0),
                                                  legend.text = element_text(color = "black", face = "bold", size = 10),
                                                  legend.title = element_text(color = "black", face = "bold", size = 10)) +
                                            scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) +
                                            scale_x_continuous(position = "top", breaks = seq(1,length(BB_data_fill[[input$bb_name_plot]]$nuova_variabile),1), 
                                                               labels = as.vector(BB_data[[input$bb_name_plot]]$nuova_variabile)) +
                                            coord_flip()}, width = 600, height = 400)
  
  ###############
  
   source("plot.R") # import plots 
  
  
  
  output$graph <- renderPlot({
    question <- input$question
    if (question %in% names(plots)) {
      plots[[question]]()
    } else {
      NULL
    }
  })
  
  tiering_text <- reactive({
    validate(need(input$bb_name_plot, "Please select a Biobank ID"))
    scores$tier[scores$BB_ID == input$bb_name_plot]
  })
  
  tiering_color <- reactive({
    ifelse(tiering_text() == "Mature", "success", ifelse(tiering_text() == "Advanced", "warning", "danger"))}
   )
  
  output$tiering_valueBox <- renderValueBox({
    valueBox(
      value = tiering_text(),
      subtitle = "Tier",
      color = tiering_color(),
      icon = icon("chart-simple"),
      width = 6
    )
  })

  output$slider <- renderUI({
    slider_col <- reactive({
      ifelse(tiering_text() == "Mature", "#28a745",
             ifelse(tiering_text() == "Advanced", "#ffc107", "#dc3545"))
    })
    
    tags$div(
      id = "custom_slider",
      sliderInput(inputId = "inslider", label = NULL,
                  min = 1,
                  max = 47,
                  value = which(scores$BB_ID == input$bb_name_plot),
                  ticks = FALSE),
      tags$style(HTML(paste0(
        sprintf("#custom_slider .irs-bar { background: %s; }", slider_col()),
        sprintf("#custom_slider .irs-bar-edge { background: %s; }", slider_col()),
        sprintf("#custom_slider .irs-line { background: %s; }", slider_col()),
        sprintf("#custom_slider .irs-single { background: %s; }", slider_col()),
        sprintf("#custom_slider .irs-bar-edge { background: %s; }", slider_col())
        )
        )
    ))
  })
  
  

  #   tags$script(HTML('
  #   $(document).on("shiny:value", function(event) {
  #     if (event.name === "inslider") {
  #       var tiering_color = $("#tiering_valueBox").css("background-color");
  #       $("#custom_slider .irs-bar").css("background", tiering_color);
  #       $("#custom_slider .irs-bar-edge").css("border-color", tiering_color);
  #       $("#custom_slider .irs-single").css("background", tiering_color);
  #     }
  #   });
  # '))
  # })

}


# Run Shiny App -----------------------------------------------------------
shinyApp(ui = ui, server = server)


