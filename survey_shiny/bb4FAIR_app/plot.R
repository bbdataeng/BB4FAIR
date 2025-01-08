# Import libraries  -------------------------------------------------------
library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(viridis)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(scales)
library(tibble)
library(hrbrthemes)
library(grid)
library(gridExtra)
library(DT)


#survey_BB
# setwd("/Users/federica/Documents/GitHub/BB4FAIR/survey_shiny/bb4FAIR_app")
survey <- read_xlsx("./survey_risposte_completo.xlsx")
survey <- as.data.frame(survey[,c(7:44)])


# function
move2last <- function(df, r) {
  n <- nrow(df)
  if(r >= n) df else df[c(seq_len(n)[-r],r),,drop = FALSE]
}



# color palette 
nb.cols <- 18
mycolors <- c("#8dd3c7", "#fdb462", "#bebada", "#fb8072", "#80b1d3", "#dfc27d", "#b3de69", "#fccde5", "#ccebc5", "#a18080", "#bc80bd", "#cdea51", "#da5f0f", "#1499bf", "#906a85", "#2aad8a", "#f4a582", "#f9f68a")



# dedicated personnel  ----------------------------------------------------

bb_personnel <- as.data.frame(table(survey[[5]]), stringsAsFactors = FALSE)

bb_personnel$Var1 <- ifelse(grepl("Si", ignore.case = T, bb_personnel$Var1), "Yes", bb_personnel$Var1)

## chart

bb_personnel$fraction = bb_personnel$Freq / sum(bb_personnel$Freq)

bb_personnel$ymax = cumsum(bb_personnel$fraction)
bb_personnel$ymin = c(0, head(bb_personnel$ymax, n=-1))

bb_personnel$labelPosition <- (bb_personnel$ymax + bb_personnel$ymin) / 2

# donut plot

chart_personnel <- ggplot(bb_personnel, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y") +
  xlim(c(2, 4)) +
  theme_void() +
  labs(title = "Presence of dedicated personnel") +
  guides(fill = guide_legend(title = "Answers")) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    text = element_text(family = "Arial"))+
   scale_fill_manual(values = mycolors)


# staff activities-------------------------------

activ <- survey[[6]] %>%
  na.omit() %>%
  strsplit(";") %>%
  unlist() %>%
  table() %>%
  as.data.frame() 


activ$. <- str_replace(activ$.,"Gestione dei campioni", "Samples handling")
activ$. <- str_replace(activ$.,"Gestione dei dati associati ai campioni", "Specimen associated data management")
activ$. <- str_replace(activ$.,"Gestione della qualità", "Quality management")
activ$. <- ifelse(grepl("Compliance", ignore.case = T, activ$.), "Ethical-legal-social compliance (Informed consents, data protection, feedback results, access)", activ$.)

activ <-activ[order(activ$Freq, decreasing = T),]
activ$. <- factor(activ$., levels = activ$.)

# Barplot

chart_personnel2 <- ggplot(activ, aes(x = ., y = Freq, fill =.)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal()+
  geom_text(aes(label = Freq), vjust = -0.5) +
  scale_x_discrete(labels = c("", "", "", "", "")) +
  labs(title = "Biobank dedicated staff activities", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))
  
# data annotation personnel --------------------------

annot_pers <- as.data.frame(table(survey[[8]]))

annot_pers$Var1 <- str_replace(annot_pers$Var1, "Si", "Yes")
annot_pers$Var1 <- str_replace(annot_pers$Var1, "Non so", "Unsure")

# donut setting
annot_pers$fraction = annot_pers$Freq / sum(annot_pers$Freq)

annot_pers$ymax = cumsum(annot_pers$fraction)
annot_pers$ymin = c(0, head(annot_pers$ymax, n=-1))

annot_pers$labelPosition <- (annot_pers$ymax + annot_pers$ymin) / 2

# donut

chart_personnel3 <- ggplot(annot_pers, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= Var1)) +
  geom_rect() +
  coord_polar("y", start=0)+
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")),size = 5, fontface = "bold") +
  theme_void() +
  guides(fill = guide_legend(title = "Answers")) +
  labs(title = "Staff with experience in data annotation \nand data modelling") +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    text = element_text(family = "Arial")
  ) +
  scale_fill_manual(values = mycolors)


# terminologies ---------------------------------------------------

term <- survey[[9]] %>%
  na.omit() %>%
  strsplit(";|,") %>%
  unlist() %>%
  as.data.frame()

ans9 <- c("SNOMED CT","ICD-9","ICD-10","LOINC","OBIB","ATC")
    
other_term <- c()

term$. <- str_remove_all(term$., "Terminologie: ")
term$. <- str_remove_all(term$., "Ontologie: ")
term$. <- ifelse(grepl("atc", ignore.case = T, term$.), "ATC", term$.)


for (r in term$.) {
  risp <- tolower(r)  
  if (!(risp %in% tolower(ans9))) {
    other_term <- c(other_term, r)
  }
}

term <- as.data.frame(table(term), stringsAsFactors = FALSE)
 

for (a in 1:length(term$.)) {
  
  ifelse(term$Freq[a] <= 1, term$.[a] <-"Some other", term$.)
  
} 

term <- term %>%
  group_by(term$.) %>%
  summarize(Somma_Frequenza = sum(Freq))

term <- term[order(term$Somma_Frequenza, decreasing = T),]
term <- move2last(term,5)
term$`term$.` <- factor(term$`term$.`, levels = term$`term$.`)

## barplot terminologie

chart_term <- ggplot(term, aes(x = `term$.`, y = Somma_Frequenza, fill = `term$.`)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal()+
  geom_text(aes(label = Somma_Frequenza), vjust = -0.5) +
  labs(title = "Terminologies or ontologies commonly used", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -30, hjust = 0.15, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
  guides(fill = guide_legend(title = "")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# common data model - personnel -------------------------------------

cdm_pers <- as.data.frame(table(survey[[10]]))

cdm_pers$Var1 <- str_replace(cdm_pers$Var1, "Si", "Yes")
cdm_pers$Var1 <- str_replace(cdm_pers$Var1, "Non so", "Unsure")

# donut setting
cdm_pers$fraction = cdm_pers$Freq / sum(cdm_pers$Freq)

cdm_pers$ymax = cumsum(cdm_pers$fraction)
cdm_pers$ymin = c(0, head(cdm_pers$ymax, n=-1))

cdm_pers$labelPosition <- (cdm_pers$ymax + cdm_pers$ymin) / 2
 
# donut

chart_personnel4 <- ggplot(cdm_pers,  aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  coord_polar("y", start=0)+
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  theme_void()+
  labs(title = "Do the Biobank staff have expertise in Common Data Models?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
  guides(fill = guide_legend(title = "Answers")) + 
  scale_fill_manual(values = mycolors)


# Common Data Models --------------------------------

cdm <- survey[[11]] %>%
  na.omit %>%
  strsplit(";|,|-| e ") %>%
  unlist() %>%
  as.data.frame()

cdm$. <- ifelse(grepl("omop", cdm$., ignore.case = T), "OMOP", cdm$. )
cdm$. <- ifelse(grepl("hir", cdm$.,  ignore.case = T), "FHIR", cdm$. )
  
other_CDM <- c()

for (cdmodel in 1:length(cdm$.)) {
  if(cdm$.[cdmodel] %in% c("OMOP", "FHIR"))  {
  cdmodel <- cdmodel 
  } else {
  other_CDM <- c(other_CDM, cdm$.[cdmodel])
  cdm$.[cdmodel] <- "Some other"
  }
}  

cdm <- as.data.frame(table(cdm))

cdm$.<- factor(cdm$., levels = cdm$.)

# barplot 

chart_cdm <-ggplot(cdm, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -30, hjust = 0.15, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "Adopted Common Data Models", x = "Answers", y = "N Biobanks") +
  guides(fill = guide_legend(title = "CDM"))+
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# HL7 FHIR - personnel ----------------------------------------

fhir <- as.data.frame(table(survey[[12]]))

# donut setting
fhir$fraction = fhir$Freq / sum(fhir$Freq)

fhir$ymax = cumsum(fhir$fraction)
fhir$ymin = c(0, head(fhir$ymax, n=-1))

fhir$labelPosition <- (fhir$ymax + fhir$ymin) / 2
 
# donut 

chart_personnel5 <- ggplot(fhir, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Does the staff have experience with HL7 FHIR?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
  guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# database ----------------------------------------

lim <- as.data.frame(table(survey[[13]]))

lim$Var1 <- str_replace(lim$Var1, "Si", "Yes")

# donut setting
lim$fraction = lim$Freq / sum(lim$Freq)

lim$ymax = cumsum(lim$fraction)
lim$ymin = c(0, head(lim$ymax, n=-1))

lim$labelPosition <- (lim$ymax + lim$ymin) / 2
 
# piechart 

chart_lims <- ggplot(lim, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Does the institution have a computer system or LIMS-Database dedicated 
  to the data associated with the biological samples in the Biobank?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
  guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# bims2 -------------------------------------------

sist <- survey[[14]] %>%
  na.omit() %>%
  unlist() %>%
  as.data.frame()


lims <- c()
NOlims <- c()

for(n in sist$.) {

  if (startsWith(n, "S")) {
    NOlims <- append(NOlims, n)
  } else {
    lims <- append(lims, n)
  }

}

sist <- as.data.frame(table(sist), stringsAsFactors = F)


sist <- sist %>%
  group_by(sist$.) %>%
  summarize(Freq = sum(Freq)) %>%
  arrange(desc(Freq)) %>%
  move2last(1)

sist$`sist$.` <- factor(sist$`sist$.`, levels = sist$`sist$.`)


# bims 3 ----------------------------------------------

lims <- as.data.frame(table(lims), stringsAsFactors = F)
lims <- lims[order(lims$Freq, decreasing = T),]
# lims$lims <- paste("BIMS_", seq(1, length(lims$lims)), sep = "")
lims$lims <- factor(lims$lims, levels = lims$lims)

# barplot 

chart_lims2 <- ggplot(lims, aes(x = lims, y = Freq, fill = lims)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "Used LIMS or BIMS software", x = "BIMS", y= "count") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -30, hjust = 0.15, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect()) +
  guides(fill = guide_legend(title = "")) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# clinical data - it system -----------------------------------

link <- survey[[15]] %>%
  tolower() %>%
  na.omit() %>%
  as.data.frame()

link$. <- ifelse(grepl("si\\b|si |sì|si:|si,|collegato|linked", link$.), "yes", link$.)
link$. <- ifelse(grepl("parzialmente", link$.), "partly", link$.)
link$. <- ifelse(grepl("previs|svilup", link$.), "planned", link$.)
link$. <- ifelse(grepl("na|no,|no", link$.), "no", link$.)
link <- as.data.frame(table(link)) 
link <- link[order(link$Freq, decreasing = T),]
link$. <- factor(link$., levels = link$.)

# barplot 

chart_clindata <- ggplot(link, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  geom_text(aes(label = Freq),vjust = -0.5) +
  scale_x_discrete(limits = levels(link$.)) +
  labs(title = "Is the IT system linked to clinical data?", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))



# data storage without db --------------------------------------

nosist <- survey[[16]] %>%
  na.omit %>%
  as.data.frame()

risp16 <- c("Tabelle Excel o simili compilate manualmente", "Stesso Database-LIMS di altro dipartimento (e.g. Anatomia Patologica)")

nosist$. <- ifelse(!(nosist$. %in% risp16), "Some other", nosist$.) 

nosist$. <- str_replace(nosist$., "Tabelle Excel o simili compilate manualmente", "Excel sheet or similar, manually filled")
nosist[2,1] <- "Database or LIMS from another departement (e.g. anatomical pathology)"

nosist <-as.data.frame(table(nosist))

# donut setting
nosist$fraction = nosist$Freq / sum(nosist$Freq)

nosist$ymax = cumsum(nosist$fraction)
nosist$ymin = c(0, head(nosist$ymax, n=-1))

nosist$labelPosition <- (nosist$ymax + nosist$ymin) / 2

# donutchart 

chart_nosist <- ggplot(nosist, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=.)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "How are the data stored without 
       a dedicated IT system?", x = "", y = "", ) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
  guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)

# infrastructure - data -------------------------------------------

inf <- as.data.frame(table(survey[[17]]))

inf$Var1 <- str_replace(inf$Var1, "Si", "Yes")
inf$Var1 <- str_replace(inf$Var1, "Non so", "Unsure")

# donut setting
inf$fraction = inf$Freq / sum(inf$Freq)

inf$ymax = cumsum(inf$fraction)
inf$ymin = c(0, head(inf$ymax, n=-1))

inf$labelPosition <- (inf$ymax + inf$ymin) / 2

# donut

chart_infrstr <- ggplot(inf, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Does the institution have access to an IT infrastructure \nfor data processing associated with the biobanked sample?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# IT infrastructure management -------------------------------

risp18 <- c("dal personale della biobanca","da un gruppo dedicato afferente all'istituto (e.g., il Centro Elaborazione Dati)", "fornitore esterno, non-commerciale (e.g., CINECA, INFN)",  "fornitore esterno, commerciale (e.g., AWS, Azure, etc.)")

infr_manag <- survey[[18]] %>%
  na.omit 
infr_manag<- ifelse(!(infr_manag %in% risp18), "Some other", infr_manag) 

infr_manag <- as.data.frame(table(infr_manag))

infr_manag$infr_manag <- str_replace(infr_manag$infr_manag, "dal personale della biobanca", "By Biobank staff")
infr_manag$infr_manag <- ifelse(grepl("gruppo dedicato afferente", infr_manag$infr_manag), "By dedicated staff relating to the institute (e.g. Data Processing Center)", infr_manag$infr_manag)
infr_manag$infr_manag <- ifelse(grepl("fornitore esterno, non-commerciale", infr_manag$infr_manag), "Non-bussiness third-party provider (e.g., CINECA, INFN)", infr_manag$infr_manag)
infr_manag$infr_manag <- ifelse(grepl("fornitore esterno, commerciale ", infr_manag$infr_manag), "Bussiness third-party provider (e.g., AWS, Azure, etc.)", infr_manag$infr_manag)

infr_manag$infr_manag <- factor(infr_manag$infr_manag, levels = infr_manag$infr_manag)

# barplot 

chart_infrstr2 <- ggplot(infr_manag, aes(x = infr_manag, y = Freq, fill =infr_manag)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "IT infrastructure management", x = "Answers", y = "N Biobanks" ) +
  scale_x_discrete(labels = c("","","","","")) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
  guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))

# storage ----------------------------------------------

storage <- as.data.frame(table(survey[[19]]))

storage$Var1 <- str_replace(storage$Var1, "Si", "Yes")
storage$Var1 <- str_replace(storage$Var1, "Non so", "Unsure")

# donut setting
storage$fraction = storage$Freq / sum(storage$Freq)

storage$ymax = cumsum(storage$fraction)
storage$ymin = c(0, head(storage$ymax, n=-1))

storage$labelPosition <- (storage$ymax + storage$ymin) / 2
 
# donut

chart_storage <- ggplot(storage, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Availability of a massive storage system", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# federated search ------------------------------------

fed_s <- as.data.frame(table(survey[[20]]))

fed_s$Var1 <- str_replace(fed_s$Var1, "Si", "Yes")

# donut setting
fed_s$fraction = fed_s$Freq / sum(fed_s$Freq)

fed_s$ymax = cumsum(fed_s$fraction)
fed_s$ymin = c(0, head(fed_s$ymax, n=-1))

fed_s$labelPosition <- (fed_s$ymax + fed_s$ymin) / 2
 
# donut

chart_fed_search <- ggplot(fed_s, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Can the Biobank, indipendently or by the IT departement, 
       allocate computational resources for Federated Search services?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
    guides(fill = guide_legend(title = "Answers", reverse = T)) +
   scale_fill_manual(values = mycolors)


# federated search resoures ----------------------------------

fed_res <- survey[[21]] %>%
  na.omit() %>%
  strsplit(";") %>%
  unlist() %>%
  table() %>%
  as.data.frame( stringsAsFactors = F)

risp21 <- c("macchina virtuale","software containers","computer fisico dedicato")

for (a in 1:length(fed_res$.)) {
  
  ifelse(!(fed_res$.[a] %in% risp21),fed_res$.[a] <-"Some other", fed_res$.)
  
}

fed_res <- fed_res %>%
  group_by(fed_res$.) %>%
  summarize(Freq = sum(Freq)) %>%
  move2last(1) 

fed_res <- fed_res[order(fed_res$Freq, decreasing = T),]
fed_res$`fed_res$.` <- str_replace(fed_res$`fed_res$.`, "macchina virtuale", "Virtual machine")
fed_res$`fed_res$.` <- str_replace(fed_res$`fed_res$.`, "computer fisico dedicato", "Physical dedicated computer")
fed_res$`fed_res$.` <-  factor(fed_res$`fed_res$.` , levels = fed_res$`fed_res$.`)

# barplot 

chart_fed_search2 <- ggplot(fed_res, aes(x = `fed_res$.`, y = Freq, fill = `fed_res$.`)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal() +
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "Resources employed for Federated Search services", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -20, hjust = 0.1, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect()) +
  guides(fill = guide_legend(title = "")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# dwh ----------------------------------------------

dwh <- as.data.frame(table(survey[[22]]))

dwh$Var1 <- str_replace(dwh$Var1, "Si", "Yes")
dwh$Var1 <- str_replace(dwh$Var1, "Non so", "Unsure")

# donut setting
dwh$fraction = dwh$Freq / sum(dwh$Freq)

dwh$ymax = cumsum(dwh$fraction)
dwh$ymin = c(0, head(dwh$ymax, n=-1))

dwh$labelPosition <- (dwh$ymax + dwh$ymin) / 2
 
# donut

chart_dwh <- ggplot(dwh, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Data Warehouse or Data Lake presence", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# data storage infrastructure ------------------------------------------

str_dwh <- survey[[23]] %>%
  na.omit() %>%
  table() %>%
  as.data.frame()

str_dwh$. <- str_replace(str_dwh$., "in sistemi fisici dell'istituto", "Institute's physical system")
str_dwh$. <- str_replace(str_dwh$., "su cloud", "Cloud")

str_dwh <- str_dwh[order(str_dwh$Freq, decreasing = T),]
str_dwh$.<- factor(str_dwh$., levels = str_dwh$.)

# barplot

chart_dwh2 <- ggplot(str_dwh, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "Where is the infrastructure for data storage located?", x = "Answer", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -20, hjust = 0.1, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
  guides(fill = guide_legend(title = "")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))

 
# ngs lab -------------------------------
# data storage infrastructure ------------------------------------------

str_dwh <- survey[[23]] %>%
  na.omit() %>%
  table() %>%
  as.data.frame()

str_dwh$. <- str_replace(str_dwh$., "in sistemi fisici dell'istituto", "Institute's physical system")
str_dwh$. <- str_replace(str_dwh$., "su cloud", "Cloud")

str_dwh <- str_dwh[order(str_dwh$Freq, decreasing = T),]
str_dwh$.<- factor(str_dwh$., levels = str_dwh$.)

# barplot

chart_dwh2 <- ggplot(str_dwh, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "Where is the infrastructure for data storage located?", x = "Answer", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -20, hjust = 0.1, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
  guides(fill = guide_legend(title = "")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# ngs tech --------------------------------------
# ngs technology -----------------------------------------------

ngs_tech <- survey[[25]] %>%
  na.omit() %>%
  strsplit(";") %>%
  unlist() 

risp25 <- c ("Illumina","Thermo Fisher","Oxford Nanopore")
ngs_tech <- ifelse(!(ngs_tech %in% risp25), "Other technologies", ngs_tech)

ngs_tech <- ngs_tech %>% 
  table() %>%
  as.data.frame() %>%
  move2last(1)

ngs_tech <- ngs_tech[order(ngs_tech$Freq, decreasing = T), ]
ngs_tech$. <- factor(ngs_tech$., levels = ngs_tech$.)


# barplot

chart_ngs_tec <- ggplot(ngs_tech, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "NGS technology", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -20, hjust = 0.4, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# omics platform ---------------------------------------------

omics <- survey[[26]] %>%
  na.omit() %>%
  strsplit(";|,| e ") %>%
  unlist() %>%
  as.data.frame()

omics <- omics %>%
  filter(!if_any(everything(), ~ . %in% c("no", "No", "NA") ))

omics$. <-  ifelse(grepl("Proteomica", omics$., ignore.case = T), "Proteomics", omics$.)
omics$. <-  ifelse(grepl("Metabolomica", omics$., ignore.case = T), "Metabolomics", omics$.)
omics$. <-  ifelse(grepl("Genomica", omics$., ignore.case = T), "Genomics", omics$.)
omics$. <-  ifelse(grepl("multiplex spatial", omics$., ignore.case = T), "Multiplex Spatial Profiling", omics$.)
omics$. <-  ifelse(grepl("radiomica", omics$., ignore.case = T), "Radiomics", omics$.)
omics$. <-  ifelse(grepl("trascrittomica", omics$., ignore.case = T), "Transcriptomics", omics$.)

for (om in 1:length(omics$.)) {
  if(omics$.[om] %in% c("Proteomics","Metabolomics","Genomics", "Transcriptomics", "Multiplex Spatial Profiling", "Transcriptomics", "Radiomics"))  {
  om <- om 
  } else {
  omics$.[om] <- "Some other"
  }
}  


omics <- table(omics) %>%
  as.data.frame()

omics <- omics[order(omics$Freq, decreasing = T),]
omics <- omics %>% move2last(4)
omics$. <-  factor(omics$., levels = omics$.)

# barplot 

chart_omics <- ggplot(omics, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "'omics' characterization platform", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -20, hjust = 0.1, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# lab ----------------------------------------

lab_spec <- as.data.frame(table(survey[[27]]))

lab_spec$Var1 <- str_replace(lab_spec$Var1, "Si", "Yes")
lab_spec$Var1 <- str_replace(lab_spec$Var1, "Non so", "Unsure")

# donut setting
lab_spec$fraction = lab_spec$Freq / sum(lab_spec$Freq)

lab_spec$ymax = cumsum(lab_spec$fraction)
lab_spec$ymin = c(0, head(lab_spec$ymax, n=-1))

lab_spec$labelPosition <- (lab_spec$ymax + lab_spec$ymin) / 2
 
# piechart

chart_lab <- ggplot(lab_spec, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Accesibility to other specialized laboratory", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# lab specialization --------------------------------------

lab <- survey[[28]] %>%
  na.omit() %>%
  as.data.frame()

lab$. <- ifelse(grepl("patologica", lab$., ignore.case = T), "Pathological anatomy", lab$.)
lab$. <- ifelse(grepl("pathology", lab$., ignore.case = T), "Digital Pathology", lab$.)
lab <- as.data.frame(table(lab),stringsAsFactors = F)

other_lab <- c()

for (labspec in 1:length(lab$.)) {
  if(lab$.[labspec] %in% c("Pathological anatomy","Digital Pathology"))  {
  labspec <- labspec
  } else {
  other_lab <- c(other_lab, lab$.[labspec])
  lab$.[labspec] <- "Some other"
  }
}  

lab <- lab %>%
  group_by(lab$.) %>%
  summarize(Freq = sum(Freq))

lab <- lab[order(lab$`lab$.`, decreasing = T),]
lab <- move2last(lab, 1)
lab$`lab$.` <- factor(lab$`lab$.`, levels = lab$`lab$.`)

# barplot

chart_lab2 <- ggplot(lab, aes(x = `lab$.`, y = Freq, fill = `lab$.`)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal() +
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "Specialized laboratory", x = "", y = "") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -15, hjust = 0.4, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# data specialization ------------------------------------------

data_sp <- survey[[29]] %>%
  strsplit(";|,") %>%
  unlist() %>%
  as.data.frame() 

risp29 <- c("malattie rare","oncologia","metagenomica batterica","metagenomica virale")
  
for (a in 1:length(data_sp$.)) {
  
  ifelse(!(data_sp$.[a] %in% risp29),data_sp$.[a] <-"Some other", data_sp$.)
  
}

data_sp$. <- str_replace(data_sp$., "malattie rare", "Rare disease")
data_sp$. <- str_replace(data_sp$., "oncologia", "Oncology")
data_sp$. <- str_replace(data_sp$., "metagenomica batterica", "Bacterial metagenomics")
data_sp$. <- str_replace(data_sp$., "metagenomica virale", "Viral metagenomics")

data_sp <- as.data.frame(table(data_sp))

# donut setting
data_sp$fraction = data_sp$Freq / sum(data_sp$Freq)

data_sp$ymax = cumsum(data_sp$fraction)
data_sp$ymin = c(0, head(data_sp$ymax, n=-1))

data_sp$labelPosition <- (data_sp$ymax + data_sp$ymin) / 2
 
# piechart

chart_data <- ggplot(data_sp, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=.)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 4.3, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 3.1, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void() +
  labs(title = "Biobanked data specialization", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
    guides(fill = guide_legend(title = "Answers", reverse = T)) +
   scale_fill_manual(values = mycolors)


# correlated data ---------------------------------------------

data_type <- survey[[30]] %>%
  strsplit(";") %>%
  unlist()

risp30 <- c("Lifestyle dataset", "Environmental dataset", "Physiological dataset", "Biochemical dataset", "Clinical dataset", "Psychological dataset", "Genomic dataset (sequences)", "Genomic dataset (variants)", "Proteomic dataset", "Metabolomic dataset", "Body (Radiological) image", "Whole slide image", "Photo image", "Genealogical records")


for (a in 1:length(data_type)) {
  
  ifelse(!(data_type[a] %in% risp30),data_type[a] <-"Some other", data_type)
  
}

data_type<- as.data.frame(table(data_type)) 

data_type <- data_type[order(data_type$Freq, decreasing = T),]
data_type <- move2last(data_type,9)
data_type$data_type <- factor(data_type$data_type, levels = data_type$data_type)

# barplot 

chart_data2 <- ggplot(data_type, aes(x = data_type, y = Freq, fill = data_type)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "Data type correlated to Biobanks", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -45, hjust = 0),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        plot.margin = margin(0,1,0,0, "cm"),
        text = element_text(family = "Arial")) + 
  guides(fill = guide_legend(title = "")) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# ontologies ---------------------------------------------

ont <- survey[[31]] %>%
  strsplit(";|-|, ") %>%
  unlist()

ont <- ifelse(grepl("no |na|nessun| no|No$", ont, ignore.case = T), "Not", ont)
ont <- ifelse(grepl("icd", ont, ignore.case = T), "ICD", ont)
ont <- ifelse(grepl("non", ont, ignore.case = T), "Unsure", ont)
ont <- ifelse(grepl("meddra", ont, ignore.case = T), "MedDRA", ont)
ont <- ont[ont != "etc."]

risp31 <- c("FMA","NCIt","ORDO","OMIM","HPO","MIABIS","MedDRA","OBIB","ICD","Not","Unsure")

other_ont <- c()

for (ontologie in 1:length(ont)) {
  if(ont[ontologie] %in% risp31)  {
  ontologie <- ontologie 
  } else {
  other_ont <- c(other_ont, ont[ontologie])
  ont[ontologie] <- "Some other"
  }
}  

ont <- as.data.frame(table(ont)) 

ont <- ont[order(ont$Freq, decreasing = T), ]
ont <- move2last(ont, 3) %>%
  move2last(7) %>%
  move2last(1)

ont$ont <- factor(ont$ont, levels = ont$ont)

# barplot 

chart_onto <- ggplot(ont, aes(x = ont, y = Freq, fill = ont)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "Ontologies reference associated to biobanked sample correlated data", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -45, hjust = 0),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")
        ) + 
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# data sharing - specialiazed network -----------------

data_share <- as.data.frame(table(survey[[32]]))

data_share$Var1 <- str_replace(data_share$Var1, "Sì", "Yes")

# donut setting
data_share$fraction = data_share$Freq / sum(data_share$Freq)

data_share$ymax = cumsum(data_share$fraction)
data_share$ymin = c(0, head(data_share$ymax, n=-1))

data_share$labelPosition <- (data_share$ymax + data_share$ymin) / 2
 
# piechart 

chart_network <- ggplot(data_share, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Are there subsets of data shared in specialized networks?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# network ----------------------------

sharing_net <- survey[[33]] %>%
  na.omit() %>% 
  strsplit(";|,|; |, ") %>%
  unlist() %>%
  table() %>%
  as.data.frame( stringsAsFactors = F)

sharing_net <- sharing_net[-1,]

for (a in 1:length(sharing_net$.)) {
  
  ifelse(sharing_net$Freq[a] <= 1, sharing_net$.[a] <-"Some other", sharing_net$.)
  
}

sharing_net <-sharing_net %>%
  group_by(sharing_net$.) %>%
  summarize(Freq = sum(Freq)) %>%
  move2last(2)
 

# donut setting
sharing_net$fraction = sharing_net$Freq / sum(sharing_net$Freq)

sharing_net$ymax = cumsum(sharing_net$fraction)
sharing_net$ymin = c(0, head(sharing_net$ymax, n=-1))

sharing_net$labelPosition <- (sharing_net$ymax + sharing_net$ymin) / 2

sharing_net$`sharing_net$.` <- factor(sharing_net$`sharing_net$.`, levels = sharing_net$`sharing_net$.`)

# donut

chart_network2 <- ggplot(sharing_net, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=`sharing_net$.`)) +
  geom_rect() +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) + 
  theme_void() +
  labs(title = "Networks in which data are shared") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# data cross-ref ----------------------------------

cross <- as.data.frame(table(survey[[34]]))

cross$Var1 <- str_replace(cross$Var1, "Si", "Yes")
cross$Var1 <- str_replace(cross$Var1, "Non so", "Unsure")

# donut setting
cross$fraction = cross$Freq / sum(cross$Freq)

cross$ymax = cumsum(cross$fraction)
cross$ymin = c(0, head(cross$ymax, n=-1))

cross$labelPosition <- (cross$ymax + cross$ymin) / 2

# piechart

chart_data4 <- ggplot(cross, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Is it possible to cross-reference data with ones on another \ninstitution's or terrritorial system?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        text = element_text(family = "Arial")
        ) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# cross data 2 ---------------------------------------
# how data are cross-referenced ----------------------------------------------------

sist_cross <- survey[[35]] %>%
  na.omit() %>%
  strsplit(";") %>%
  unlist() %>%
  table() %>%
  as.data.frame( stringsAsFactors = F) 

sist_cross <- sist_cross[order(sist_cross$Freq, decreasing = T),]

sist_cross <- sist_cross[1:3,]

sist_cross$. <- str_replace(sist_cross$., "è possìbile incrociare i dati con il sìstema informativo clinico dell'infrastruttura a cui afferisce la biobanca", "It is possible to cross-reference data with clnical information system of the infrastructure affiliated to the Biobank")
sist_cross$. <- str_replace(sist_cross$.,"è possìbile incrociare i dati con sìstemi informativi clinici territoriali a cui è collegata l'istituzione a cui afferisce la biobanca","It is possible to cross-reference data with territorial clinical information system connected to  the Biobank affiliated institution" )
sist_cross$. <- str_replace(sist_cross$.,"è possibile incrcociare ma non in modo automatico", "it is possible but not in automated way")

sist_cross$.<- factor(sist_cross$., levels = sist_cross$.)

# barplot 

chart_data5 <- ggplot(sist_cross, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.1) +
  labs(title = "How can the data be cross-referenced?", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")
        ) +
  guides(fill = guide_legend(title = "Answers",)) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# digital informe consent -----------------------------------

inf_cons <- as.data.frame(table(survey[[36]]))

inf_cons$Var1 <- str_replace(inf_cons$Var1, "Si", "Yes")

# donut setting
inf_cons$fraction = inf_cons$Freq / sum(inf_cons$Freq)

inf_cons$ymax = cumsum(inf_cons$fraction)
inf_cons$ymin = c(0, head(inf_cons$ymax, n=-1))

inf_cons$labelPosition <- (inf_cons$ymax + inf_cons$ymin) / 2
 
# piechart 

chart_ic <- ggplot(inf_cons,aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  coord_polar("y", start=0)+
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  theme_void()+
  labs(title = "Usage of digital-electronic informed consent", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
        ) +
    guides(fill = guide_legend(title = "Answers")) +
   scale_fill_manual(values = mycolors)


# iformed consent model -----------------------------

ic_type <- survey[[37]] %>%
  na.omit()

ic_type <- ifelse(grepl("dinamic", ic_type, ignore.case = T), "Dynamic", ic_type)
ic_type <- as.data.frame(table(ic_type))

# barplot

chart_ic_model <- ggplot(ic_type, aes(x = ic_type, y =Freq, fill = ic_type)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal() +
  geom_text(aes(label = Freq),vjust = -0.3) +
  labs(title = "Informed consent model", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(),
        text = element_text(family = "Arial")
        ) +
   scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))

