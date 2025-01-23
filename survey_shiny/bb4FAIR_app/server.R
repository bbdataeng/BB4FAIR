## ---------------------------
##
## Script name: server.R
##
## Purpose of script: server script for shinyapp
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

# Server -----------------------------------------------------------------------


server <- function(input, output) {

    map_names <- data.frame(old = colnames(scores), 
                new = c(
    "Biobank ID",
    "IT Head" ,
    "Dedicated Personnel",
    "Ontologies Richness",
    "Common Data Models",
    "BIMS",
    "Data Management",
    "Data Server",
    "Massive Storage",
    "Service Resources",
    "Data Warehouse",
    "Clinical Data Availability",
    "Annotations",
    "Registry Data Availability",
    "Informed Consent",
    "Total Score",
    "Tier"
  ))

  ## Scores Panel --------------------------------------------------------------
  output$scores <- DT::renderDataTable({
    
    colnames(scores) <- dplyr::recode(
      colnames(scores),
      !!!setNames(as.character(map_names$new), map_names$old)
    )
    DT::datatable({
      data <- scores
     
      
      ## filter by biobank name
      if (input$bb_name != "All") {
        data <- data[data$"Biobank ID" == input$bb_name, ]
      }
      ## filter by tier
      if (input$tier != "All") {
        data <- data[data$Tier == input$tier, ]
      }
      ## filter by macroarea
      if (!"All" %in% input$macroarea &
          {
            !is.null(input$macroarea)
          }) {
        final_cols <- c("Biobank ID")
        
        macro_scores <- c()
        
        for (selected_macroarea in input$macroarea) {
          colname <- paste0(selected_macroarea, " Score")
          macro_scores <- c(macro_scores, colname)
          # sum for the selected macro_area
          selected_cols <- macroareas$question[macroareas$area %in% selected_macroarea]
          final_cols <- c(final_cols, selected_cols)
          data[[colname]] <- rowSums(data[, selected_cols])
        }
        
       
        data <- data[, c(final_cols, "Tier", macro_scores, "Total Score")]
        
        
      } else{
        data <- data
      }
      
      
      map <- map_names[map_names$new %in% colnames(data),]
   
      colnames(data) <- dplyr::recode(
        colnames(data),
        !!!setNames(as.character(map$new), map$old)
      )

      rename_map <- c(
        "IT Head" = text_1,
        "Dedicated Personnel" = text_2,
        "Ontologies Richness" = text_3,
        "Common Data Models" = text_4,
        "BIMS" = text_5,
        "Data Management" = text_6,
        "Data Server" = text_7,
        "Massive Storage" = text_8,
        "Service Resources" = text_9,
        "Data Warehouse" = text_10,
        "Clinical Data Availability" = text_11,
        "Annotations" = text_12,
        "Registry Data Availability" = text_13,
        "Informed Consent" = text_14
      )
      
      # fitler only existing selected columns
      existing_rename_map <- rename_map[names(rename_map) %in% colnames(data)]
      
      data %>% rename(!!!setNames(names(existing_rename_map), existing_rename_map))
      # data %>% rename(
      #   !!text_1 := "IT Head" ,!!text_2 := "Dedicated Personnel",!!text_3 := "Ontologies Richness",!!text_4 :=
      #     "Common Data Models",!!text_5 := "BIMS",!!text_6 := "Data Management",!!text_7 :=
      #     "Data Server",!!text_8 := "Massive Storage",!!text_9 := "Service Resources",!!text_10 :=
      #     "Data Warehouse",!!text_11 := "Clinical Data Availability",!!text_12 := "Annotations",!!text_13 :=
      #     "Registry Data Availability",!!text_14 := "Informed Consent"
      # )
      
    }, rownames = TRUE, selection = 'none', extensions = c('Buttons', 'Scroller'), escape =
      FALSE, options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(
          width = 'dt-center', targets = "_all"
        )),
        
        buttons = list('excel')
      ))
  })
 
  
  
  
  ## Answers Plot Panel --------------------------------------------------------
  
  # output$graph <- 
  # networks errore su sharing_net!!!
  output$graph <- renderPlot({
    question <- input$question
    if (question %in% names(plots)) {
      plots[[question]]()
    } else {
      NULL
    }
  })
  
  ## Visualization Panel -------------------------------------------------------
  
  tier_BB <- read_xlsx("tiering6.xlsx")

  tier_norm_BB <- round(tier_BB[,-c(1:2)] %>%
                          mutate(dedicated_personnel = dedicated_personnel/max(dedicated_personnel),
                                 ontologies_richness = ontologies_richness/max(ontologies_richness),
                                 BIMS = BIMS/max(BIMS), 
                                 data_server = data_server/max(data_server),
                                 massive_storage = massive_storage/max(massive_storage),
                                 service_resources = service_resources/max(service_resources), 
                                 data_warehouse = data_warehouse/max(data_warehouse),
                                 clinical_data_availability = clinical_data_availability/max(clinical_data_availability),
                                 annotations = annotations/max(annotations),
                                 registry_data_availability = registry_data_availability/max(registry_data_availability),
                                 informed_consent = informed_consent/max(informed_consent)),
                        2)
  tier_norm_BB<-cbind(tier_BB[,1:2],tier_norm_BB)
  
  tier_norm_BB$tier <- ifelse(tier_norm_BB$total_score < 11, "Starting",
                              ifelse(tier_norm_BB$total_score < 21, "Advanced", "Mature"))
  
  
  dati<-pivot_longer(tier_norm_BB, IT_head:informed_consent,names_to="facilities",values_to="score")
  dati<-data.frame(dati,
                   macro_areas=c(rep(c(rep("personnel",4),rep("IT infrastructure",4),rep("data",5)),
                                     nrow(tier_norm_BB))))
  
  
  ### Preprocessing for plots --------------------------------------------------
  # Preprocessing data
  
  # Preprocessing data
  
  new_names <- c(
    "IT head",
    "dedicated personnel",
    "ontologies richness",
    "common data models",
    "BIMS in place",
    "server for data processing",
    "massive storage in place",
    "computational resources for FS services",
    "data warehouse in place",
    "clinical data system connection",
    "annotations",
    "data cross-referencing with other registries",
    "digital informed consent")
  
  dati$nuova_variabile <- factor(dati$facilities, levels = unique(dati$facilities), labels = new_names)
  dati$tier <- factor(dati$tier, levels = unique(dati$tier))
  # 
  output$heatmap <- renderPlot({ggplot(dati, aes(x = Biobank_ID, y = nuova_variabile, fill = score)) +
      geom_tile(color = "black", linewidth = 0.7) +
      scale_fill_gradient(low = "#244270", high = "#EB5E00") +
      scale_y_discrete(position = "right", name = "Facilities") +
      labs(x = "Biobank ID") +  
      theme(panel.background = element_blank(),
            axis.text.x = element_text(color = "black", face = "bold", size = 10, vjust = 1.1, hjust = 1.1, angle = 55),
            axis.text.y = element_text(color = "black", face = "bold", size = 10, hjust = 1),
            axis.text.y.right = element_text(face = "bold", size = 11),
            axis.title.y.right = element_text(face = "bold", margin = margin(l = 10)),
            axis.ticks.y.right = element_blank(),
            axis.ticks.x.bottom = element_blank(),
            axis.title.x = element_text(face = "bold",margin = margin(t = 10)),
            axis.title.y = element_text(face = "bold"),
            strip.text = element_text(face = "bold", size = 10),
            legend.text = element_text(face = "bold", size = 11),
            legend.title = element_text(face = "bold", , size = 11, margin = margin(b = 10)),
            strip.placement = "inside") +
      facet_grid(rows = vars(macro_areas), switch = "y", scale = "free") 
}, width = 1000, height = 400, execOnResize = FALSE)
  

dati$macro_aree <- recode(dati$macro_areas, personnel = "personale", "IT infrastructure" = "infrastrutture IT", data = "dati")
# strutture <- rep(ita_etiquettes,length(unique(dati$Biobank_ID)))

  output$density <- renderPlot({
    ggplot(dati)+
      geom_density(aes(x = score, y = ..density..), fill="#244270", alpha = 0.8) +
      facet_grid(macro_areas~., switch = "y")+
      scale_y_continuous(name = NULL, sec.axis = sec_axis(~., name = "density")) +
      theme(panel.background = element_blank(),
            axis.text.x = element_text(color = "black", face = "bold",size = 10),  
            axis.text.y.right = element_text(color = "black", face = "bold",size = 10, hjust = 1),
            axis.title.x = element_text(face = "bold"),
            axis.title.y.right = element_text(face = "bold", margin = margin(l = 15)),
            strip.text.y = element_text(face = "bold", size = 11),
            legend.text = element_text(face = "bold"),
            legend.title = element_text(face = "bold"),
            legend.position="none",
            strip.placement = "outside",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.line.y.right = element_line(),
            axis.ticks.x = element_line(color = "black"),
            axis.line.x = element_line(),
            axis.ticks.y.right = element_line(color = "black"),
            panel.spacing.x = unit(-1, "lines")) +
      scale_x_continuous(expand = c(0.01, 0.01), labels = c("0", "0.25", "0.50", "0.75", "1"))
    
  }, width = 320, height = 380, execOnResize = FALSE)

  
  # Radarplots --------------------------------------------------------
  
  # Preprocess data
  lista_tier <- tier_norm_BB %>% group_split(tier)
  
  col_median1 <- apply(lista_tier[[3]][,c(3:15)], 2, mean)
  col_median2 <- apply(lista_tier[[1]][,c(3:15)], 2, mean)
  col_median3 <- apply(lista_tier[[2]][,c(3:15)], 2, mean)
  col_summary <- data.frame(tier=c("starting","advanced","mature"),
                            t(data.frame(Base = col_median1, Intermediate = col_median2, High = col_median3)))
  
  etichette <-c("ITh","pers","onto","CDM","BIMS","Dserv","store","Cres","DWH","clin","anno","Dreg","DIC")
  
  # # Loop for radarplots and saving files
  # p4 <- list(c(NULL),c(NULL),c(NULL))
  # 
  # for(i in 1:nrow(col_summary)) {
  #   p4[[i]] <- ggradar(
  #     col_summary[i,],
  #     base.size = 8,
  #     font.radar = "Arial Black",
  #     values.radar = c("0%", "50%", "100%"),
  #     axis.labels = etichette,  
  #     plot.extent.x.sf = 1,
  #     plot.extent.y.sf = 1.2,
  #     label.centre.y = FALSE,
  #     grid.line.width = 0.3,
  #     grid.label.size = 3,
  #     label.gridline.min = TRUE,
  #     label.gridline.mid = TRUE,
  #     label.gridline.max = TRUE,
  #     axis.label.offset = 1.1,
  #     axis.label.size = 4,
  #     axis.line.colour = "#244270",
  #     group.line.width = 1.1,
  #     group.point.size = 2,
  #     group.colours = "#EC6707",
  #     background.circle.colour = "#244270",
  #     background.circle.transparency = 0.1,
  #     plot.legend = FALSE,
  #     legend.title = "",
  #     plot.title = paste0("Tier ",4-i,": ",col_summary[i,1]),
  #     legend.text.size = 10,
  #     legend.position = "bottom",
  #     fill = TRUE,
  #     fill.alpha = 0.4) +
  #     theme_classic(base_family='Arial Black') +
  #     theme(
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.title.x = element_text(face = "bold"),
  #       axis.title.y = element_text(face = "bold", size = 12),
  #       strip.text = element_text(face = "bold"),
  #       plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
  #       axis.text = element_text(face = "bold"),
  #       axis.ticks.y = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       axis.line.x = element_blank(),
  #       axis.line.y = element_blank(),
  #       legend.position = "none",plot.margin = margin(20, 20, 20, 20, "pt")) +
  #     labs(y = paste("Biobanks mean score:"))

  output$radar1 <- renderPlot({
    ggradar(
      col_summary[3, ],
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
      plot.title = paste0("Tier 1: ", col_summary[3, 1]),
      legend.text.size = 10,
      legend.position = "bottom",
      fill = TRUE,
      fill.alpha = 0.4
    ) +
      theme_classic(base_family = 'Arial Black') +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", size = 12),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5
        ),
        axis.text = element_text(face = "bold"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(20, 20, 20, 20, "pt")
      ) +
      labs(y = paste("Biobanks median score:"))
  }, width = 300, height = 300, execOnResize = FALSE)


  output$radar2 <- renderPlot({
    ggradar(
      col_summary[2, ],
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
      plot.title = paste0("Tier 2: ", col_summary[2, 1]),
      legend.text.size = 10,
      legend.position = "bottom",
      fill = TRUE,
      fill.alpha = 0.4
    ) +
      theme_classic(base_family = 'Arial Black') +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", size = 12),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5
        ),
        axis.text = element_text(face = "bold"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(20, 20, 20, 20, "pt")
      ) +
      labs(y = paste("Biobanks median score:"))
  }, width = 300, height = 300, execOnResize = FALSE)


  output$radar3 <- renderPlot({
    ggradar(
      col_summary[1, ],
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
      plot.title = paste0("Tier 3: ", col_summary[1, 1]),
      legend.text.size = 10,
      legend.position = "bottom",
      fill = TRUE,
      fill.alpha = 0.4
    ) +
      theme_classic(base_family = 'Arial Black') +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", size = 12),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(
          size = 14,
          face = "bold",
          hjust = 0.5
        ),
        axis.text = element_text(face = "bold"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(20, 20, 20, 20, "pt")
      ) +
      labs(y = paste("Biobanks median score:"))
  }, width = 300, height = 300, execOnResize = FALSE)


    
# Radarplots legend -------------------------------------------------

etichette1 <- paste0("  ", etichette)
mytheme <- ttheme_default(
  core = list(
    fg_params = list(hjust = 0, x = 0.1, fontsize = 11),
    padding = unit(c(0.5, 0.5), "cm")
  ),
  colhead = list(
    fg_params = list(fontsize = 11, fontface="bold"),
    padding = unit(c(0.5, 0.5), "cm")
  )
)
tabella <- tableGrob(rows = NULL, data.frame(etichette1 = as.character(etichette1), 
                                nuova_variabile = c(as.character(unique(dati$nuova_variabile)))),					
                     theme = mytheme, cols = c("short", "extended"))
tabella$widths <- unit(tabella$widths + unit(0.5, "cm"), "cm")


output$legend <- renderPlot({
  grid.arrange(tabella)
}, width = 360, height = 320, execOnResize = FALSE)

 
  
  
# Tiering Panel --------------------------------------------------------------
  
dati1 <- dati
dati1$tier <- factor(dati1$tier, levels = unique(dati1$tier))
dati1$facilities <- factor(dati1$facilities, levels = unique(dati1$facilities))
dati1$macro_areas <- factor(dati1$macro_areas, levels = unique(dati1$macro_areas)) 

BB_mean <- dati1 %>%
  group_by(macro_areas,nuova_variabile, macro_aree) %>%
  summarise(score = mean(score))
BB_mean$Italian_mean_trend <- rep("Italian mean trend", dim(BB_mean)[1])


dati1$BBMRI_mean <- rep(BB_mean$score,37)


BB_data <- vector(mode='list', length=length(unique(dati1$Biobank_ID)))

for(i in unique(dati1$Biobank_ID)){
  BB_data[[i]] <- dati1[which(dati1$Biobank_ID == i),] 
}

BB_data <- BB_data[38:74]
colnames(BB_data[[1]])




tier_BB$tier <- ifelse(
  tier_BB$total_score > 20,
  "Mature",
  ifelse(tier_BB$total_score < 11, "Starting", "Advanced")
)



  output$eval_plot <- renderPlot({
    as.data.frame(BB_data[[input$bb_name_plot]]) %>%
    ggplot(aes(x = score, y = nuova_variabile)) +
    geom_segment(aes(xend = BBMRI_mean, yend = nuova_variabile, color = "BBMRI.it mean level"), lwd = 1.5) +
    geom_segment(aes(xend = 0, yend = nuova_variabile, color = "Biobank level"), lwd = 2.5) +
    geom_point(aes(color = "Biobank level"), size = 5) +
    geom_point(aes(x = BBMRI_mean, color = "BBMRI.it mean level"), size = 4, pch = 18) +
    theme_bw() +
    ylab("Facilities") +
    xlim(0,1) +
    ggtitle(paste0("Digital Maturity Model")) + #level: ", as.data.frame(BB_data[[1]])$tier[1])) +
    scale_color_manual(name = "",
                       values = c("Biobank level" = "#244270", "BBMRI.it mean level" = "#EC6707")) +
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = "black", face = "bold", size = 12),
          axis.text.y = element_text(color = "black", face = "bold", size = 12, hjust = 1),
          axis.text.y.right = element_text(face = "bold", size = 12),
          axis.title.y.right = element_text(face = "bold", margin = margin(l = 10)),
          axis.ticks.y.right = element_blank(),
          axis.ticks.x.bottom = element_blank(),
          title = element_text(color = "black", face = "bold", size = 15),
          axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
          axis.title.y = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.text = element_text(color = "black", face = "bold", size = 12),
          legend.title = element_text(color = "black", face = "bold", size = 10))}, width = 650, height = 450
    )

  
  # Ranking 
  tiering_rank <- reactive({
    validate(need(input$bb_name_plot, "Please select Biobank ID"))
    sorted_df <- tier_BB %>% arrange(desc(total_score))
    which(sorted_df$Biobank_ID == input$bb_name_plot)
  })
  tiering_tier <- reactive({
    validate(need(input$bb_name_plot, "Please select Biobank ID"))
    tier_BB$tier[tier_BB$Biobank_ID == input$bb_name_plot]
  })
  tiering_color <- reactive({
    ifelse(
      tiering_tier() == "Mature",
      "success",
      ifelse(tiering_tier() == "Advanced", "warning", "danger")
    )
  })

  output$tiering_valueBox <- bs4Dash::renderValueBox({
    print(tiering_color())
    print(tiering_tier())
    print(tiering_rank())
    bs4Dash::valueBox(
      value = tiering_tier(),
      subtitle = "Tier",
      color = tiering_color(),
      icon = icon("chart-simple"),
      width = 6
    )
  })
# 
# 
# 
  output$gauge <- flexdashboard::renderGauge({
    flexdashboard::gauge(value = tiering_rank(), min = 0, max = 37, 
                         sectors = flexdashboard::gaugeSectors(success = c(1, 10), warning = c(11,29), danger = c(30,37))
                         )})
  
  # tiering_text <- reactive({
  #   validate(need(input$bb_name_plot, "Please select Biobank ID"))
  #   print(tier_BB)
  #   tier_BB$tier[tier_BB$Biobank_ID == input$bb_name_plot]
  # })
  # 
  # tiering_color <- reactive({
  #   ifelse(tiering_text() == "Mature", "success", ifelse(tiering_text() == "Advanced", "warning", "danger"))}
  # )
  # 
  # output$tiering_valueBox <- renderValueBox({
  #   valueBox(
  #     value = tiering_text(),
  #     subtitle = "Tier",
  #     color = tiering_color(),
  #     icon = icon("chart-simple"),
  #     width = 6
  #   )
  # })
  # 
  # output$gauge <- renderUI({
  #   tier_BB <- tier_BB %>% arrange(desc(total_score))
  #   slider_col <- reactive({
  #     ifelse(tiering_text() == "Mature", "#28a745",
  #            ifelse(tiering_text() == "Advanced", "#ffc107", "#dc3545"))
  #   })
  #   
  #   tags$div(
  #     id = "custom_slider",
  #     sliderInput(inputId = "inslider", label = NULL,
  #                 min = 1,
  #                 max = 37,
  #                 value = which(tier_BB$Biobank_ID == input$bb_name_plot),
  #                 ticks = FALSE),
  #     tags$style(HTML(paste0(
  #       sprintf("#custom_slider .irs-bar { background: %s; }", slider_col()),
  #       sprintf("#custom_slider .irs-bar-edge { background: %s; }", slider_col()),
  #       sprintf("#custom_slider .irs-line { background: %s; }", slider_col()),
  #       sprintf("#custom_slider .irs-single { background: %s; }", slider_col()),
  #       sprintf("#custom_slider .irs-bar-edge { background: %s; }", slider_col())
  #     )
  #     )
  #     ))
  # })

  
  }