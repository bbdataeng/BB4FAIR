library(tidyverse)
library(tidyr)
library(dplyr)
library(viridis)
library(ggplot2)
library(ggpubr)
library(readxl)
library(ggradar)
library(tidyverse)
library(scales)
library(tibble)
library(hrbrthemes)
library(grid)
library(gridExtra)





tier_BB <- data.frame(read_excel("./data/punteggi_tiering.xlsx",1))

tier_norm_BB <- round(tier_BB %>%
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

tier_norm_BB$tier_norm <- ifelse(tier_norm_BB$punt_norm < 4.62, "Starting",
                       		   ifelse(tier_norm_BB$punt_norm < 9.24, "Advanced", "Mature"))

tier_norm_BB$tier_quart <- ifelse(tier_norm_BB$punt_norm < 3.5, "Low",
                       		   ifelse(tier_norm_BB$punt_norm < 7, "Starting",
							ifelse(tier_norm_BB$punt_norm < 10.5, "Advanced", "Mature")))

head(tier_norm_BB)

dati<-pivot_longer(tier_norm_BB, IT_head:informed_consent,names_to="facilities",values_to="score")
dati<-data.frame(dati,macro_areas=c(rep(c(rep("personnel",4),rep("infrastructure",6),rep("data",4)),nrow(tier_norm_BB))))
dati$tier <- ifelse(dati$punteggio_totale < 11, "Starting",
                       ifelse(dati$punteggio_totale < 21, "Advanced", "Mature"))
dati






####################################################################################
###################################### PLOTS  ######################################
####################################################################################



#######HEATMAP

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
head(dati,40)

p2<-ggplot(dati, aes(x = biobank, y = nuova_variabile, fill = score)) +
   geom_tile(color = "black", linewidth = 0.7) +
   scale_fill_gradient(low="#244270", high="#EB5E00") +
   scale_y_discrete(position = "right",name="facilities") +
   theme(panel.background = element_blank(),
         axis.text.x = element_text(face = "bold", size = 9),  
         axis.text.y = element_text(face = "bold", size = 9, hjust = 1),
         axis.text.y.right = element_text(face = "bold", size = 9, margin = margin(l = -20)),
         axis.title.y.right = element_text(face = "bold", margin = margin(l = 10)),
	   axis.ticks.y.right = element_blank(),
         axis.title.x = element_text(face = "bold"),
         axis.title.y = element_text(face = "bold"),
         strip.text = element_text(face = "bold", size = 10),
         legend.text = element_text(face = "bold"),
         legend.title = element_text(face = "bold"),
         strip.placement = "outside") +
   facet_grid(rows = vars(macro_areas), switch = "y", scale = "free_y")

ggsave(filename = "heatmap.png", 
	 plot = p2 , 
	 device = "png", 
	 dpi = 300, 
	 height = 4,
	 width = 11)





##DENSITYPLOT

p1<-ggplot(dati)+
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
		 panel.spacing.x = unit(-3, "lines"))


ggsave(filename = "density.png", 
	 plot = p1 , 
	 device = "png", 
	 dpi = 300, 
	 height = 4,
	 width = 5)






			

lista_tier <- tier_norm_BB %>% group_split(tier)

col_median1 <- apply(lista_tier[[1]][,c(2:15)], 2, median)
col_median2 <- apply(lista_tier[[2]][,c(2:15)], 2, median)
col_median3 <- apply(lista_tier[[3]][,c(2:15)], 2, median)
col_summary <- data.frame(tier=c("Starting","Advanced","Mature"),t(data.frame(Base = col_median1, Medium = col_median3, High = col_median2)))



##RADARPLOT

for(i in 1:nrow(col_summary)) {
  p4 <- ggradar(
    col_summary[i,],
    base.size = 8,
    font.radar = "Arial Black",
    values.radar = c("0%", "50%", "100%"),
    axis.labels = c("ITh","pers","onto","CDM","BIMS","DM","ITi","store","ITc","DWH","regis","anno","clin","IC"),  
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
    plot.title = paste(col_summary[i,1],"Tier" ),
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
    labs(y = paste("Biobanks median score:"))  

ggsave(filename = paste("radar_tier",i,".png"), 
	 plot = p4 , 
	 device = "png", 
	 dpi = 300, 
	 height = 3,
	 width = 3.9)
}








##LEGENDA RADAR

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

p5<-grid.arrange(tabella, nrow = 1)

ggsave("arranged_plot.png", p5, dpi=300, device = "png",height = 5,width = 4)

