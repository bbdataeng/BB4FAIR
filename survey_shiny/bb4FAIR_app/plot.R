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
# setwd("C:/Users/Antonella/Desktop/BBMRI_repos/bb4FAIR/survey_shiny/bb4FAIR_app")
survey <- read_xlsx("./survey_risposte_240123.xlsx")
survey <- as.data.frame(survey[,c(7:44)])


# function
move2last <- function(df, r) {
  n <- nrow(df)
  if(r >= n) df else df[c(seq_len(n)[-r],r),,drop = FALSE]
}



# color palette 
nb.cols <- 18
mycolors <- c("#8dd3c7","#fdb462","#bebada","#fb8072","#80b1d3","#dfc27d","#b3de69","#fccde5","#ccebc5","#a18080","#bc80bd","#cdea51","#da5f0f","#1499bf","#906a85","#2aad8a","#f4a582","#f9f68a")



## dedicated personnel ----------------------

p_dedicato <- as.data.frame(table(survey[5], dnn = c("ans", "Freq")), stringsAsFactors = FALSE)

p_dedicato$ans.1 <- ifelse(grepl("Si", ignore.case = T, p_dedicato$ans.1), "Yes", p_dedicato$ans.1)

#frazione delle variabili nel donut
p_dedicato$fraction = p_dedicato$Freq / sum(p_dedicato$Freq)

# top e bottom di ogni rettangolo per il donut
p_dedicato$ymax = cumsum(p_dedicato$fraction)
p_dedicato$ymin = c(0, head(p_dedicato$ymax, n=-1))

# Compute label position
p_dedicato$labelPosition <- (p_dedicato$ymax + p_dedicato$ymin) / 2

# donut plot

chart_personnel1 <- ggplot(p_dedicato, 
                           aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=ans.1)) +
                                         geom_rect() +
                                         geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), fontface = "bold") +
                                         coord_polar("y") +
                                         xlim(c(2, 4)) +
                                         theme_void() +
                                         labs(title = "Presence of dedicated personnel") +
                                         guides(fill = guide_legend(title = "dedicates personnel")) +
                                         theme(
                                           plot.title = element_text(face = "bold", size = 20, hjust = 0.5) )+
                                         scale_fill_manual(values = mycolors)

print(chart_personnel1)
## personnel activities ---------------------------

attiv <- survey[[6]] %>%
  na.omit() %>%
  strsplit(";") %>%
  unlist() %>%
  table() %>%
  as.data.frame()

attiv <- attiv[-1,]
colnames(attiv) <- c("attività", "risp")

attiv$attività <- str_replace(attiv$attività,"Gestione dei campioni", "Samples handling")
attiv$attività <- str_replace(attiv$attività,"Gestione dei dati associati ai campioni", "Specimen associated data management")
attiv$attività <- str_replace(attiv$attività,"Gestione della qualità", "Quality management")
attiv$attività <- ifelse(grepl("Compliance", ignore.case = T, attiv$attività), "Ethical-legal-social compliance (Informed consents, data protection, feedback results, access)", attiv$attività)

attiv <-attiv[order(attiv$risp, decreasing = T),]
attiv$attività <- factor(attiv$attività, levels = attiv$attività)

# Barplot

personnel2 <- ggplot(attiv, aes(x = as.factor(attività), y = as.factor(risp), fill =attività)) +
                                   geom_bar(stat = "identity", width = 0.5) +
                                   geom_rect(
                                     aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                                     fill = NA,
                                     color = "lightgrey") +
                                   theme_minimal()+
                                   geom_text(aes(label = risp), vjust = -0.5) +
                                   scale_x_discrete(labels = c("", "", "", "", "")) +
                                   labs(title = "Biobank dedicated staff activities", x = "", y = "") +
                                   theme(legend.position = "bottom",
                                         legend.direction = "vertical",
                                         plot.title = element_text(face = "bold", size = 20, hjust = 0.5))+
                                   guides(fill = guide_legend(title = "Activities:")) +
                                   scale_fill_manual(values = mycolors)


## data annotation personnel ------------------------------

pers_annotazione <- as.data.frame(table(survey[8], dnn = c("ans", "Freq")))

pers_annotazione$ans.1 <- str_replace(pers_annotazione$ans.1, "Si", "Yes")
pers_annotazione$ans.1 <- str_replace(pers_annotazione$ans.1, "Non so", "Unsure")

#frazione delle variabili nel donut
pers_annotazione$fraction = pers_annotazione$Freq / sum(pers_annotazione$Freq)

# top e bottom di ogni rettangolo per il donut
pers_annotazione$ymax = cumsum(pers_annotazione$fraction)
pers_annotazione$ymin = c(0, head(pers_annotazione$ymax, n=-1))

# Compute label position
pers_annotazione$labelPosition <- (pers_annotazione$ymax + pers_annotazione$ymin) / 2

## donutplot

personnel3 <-ggplot(pers_annotazione, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= ans.1)) +
                                   geom_rect() +
                                   coord_polar("y", start=0)+
                                   xlim(c(2, 4)) +
                                   geom_text(x= 4.3, 
                                             aes(y=labelPosition, 
                                                 label = paste0("N = ", Freq, ",\n", 
                                                                round(fraction*100, digit = 1), "%")),
                                             size = 3.5, fontface = "bold") +
                                   theme_void() +
                                   guides(fill = guide_legend(title = "")) +
                                   labs(title = "Staff with experience in data annotation \nand data modelling") +
                                   theme(
                                     plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
                                   ) +
                                   scale_fill_manual(values = mycolors)


# staff composition -----------------------------------

pers_annotazione <- as.data.frame(table(survey[8], dnn = c("ans", "Freq") ))

pers_annotazione$ans.1 <- str_replace(pers_annotazione$ans.1, "Si", "Yes")
pers_annotazione$ans.1 <- str_replace(pers_annotazione$ans.1, "Non so", "Unsure")

#frazione delle variabili nel donut
pers_annotazione$fraction = pers_annotazione$Freq / sum(pers_annotazione$Freq)

# top e bottom di ogni rettangolo per il donut
pers_annotazione$ymax = cumsum(pers_annotazione$fraction)
pers_annotazione$ymin = c(0, head(pers_annotazione$ymax, n=-1))

# Compute label position
pers_annotazione$labelPosition <- (pers_annotazione$ymax + pers_annotazione$ymin) / 2

# donut

personnel4 <- ggplot(pers_annotazione, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= ans.1)) +
                                   geom_rect() +
                                   coord_polar("y", start=0)+
                                   xlim(c(2, 4)) +
                                   geom_text(x= 4.3, 
                                             aes(y=labelPosition, 
                                                 label = paste0("N = ", Freq, ",\n", 
                                                                round(fraction*100, digit = 1), "%")),
                                             size = 3.5, fontface = "bold") +
                                   theme_void() +
                                   guides(fill = guide_legend(title = "")) +
                                   labs(title = "Staff with experience in data annotation \nand data modelling") +
                                   theme(
                                     plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
                                   ) +
                                   scale_fill_manual(values = mycolors)


# terminologies ---------------------------------

term <- survey[[9]] %>%
  na.omit() %>%
  strsplit(";|,") %>%
  unlist() %>%
  as.data.frame()

ans9 <- c("SNOMED CT","ICD-9","ICD-10","LOINC","OBIB","ATC")

altre_term <- c()

term$. <- str_remove_all(term$., "Terminologie: ")
term$. <- str_remove_all(term$., "Ontologie: ")
term$. <- ifelse(grepl("atc", ignore.case = T, term$.), "ATC", term$.)


for (r in term$.) {
  risp <- tolower(r)  
  if (!(risp %in% tolower(ans9))) {
    altre_term <- c(altre_term, r)
  }
}

term<- as.data.frame(table(term, dnn = c("ans", "Freq")), stringsAsFactors = FALSE)

for (a in 1:length(term$ans.1)) {
  
  ifelse(term$Freq[a] <= 1, term$ans.1[a] <-"Some other", term$ans.1)
  
}

term <- term %>%
  group_by(term$ans.1) %>%
  summarize(Somma_Frequenza = sum(Freq))

term <- term[order(term$Somma_Frequenza, decreasing = T),]
term <- move2last(term,5)
term$`term$ans.1` <- factor(term$`term$ans.1`, levels = term$`term$ans.1`)

## barplot terminologie

terminologies <- ggplot(term, aes(x = `term$ans.1`, y = as.factor(Somma_Frequenza), fill = `term$ans.1`)) +
                                      geom_bar(stat = "identity", width = 0.5) +
                                      theme_minimal()+
                                      geom_rect(
                                        aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                                        fill = NA,
                                        color = "lightgrey"
                                      ) +
                                      geom_text(aes(label = Somma_Frequenza), vjust = -0.5) +
                                      labs(title = "Terminologies or ontologies commonly used ", x = "", y = "", ) +
                                      theme(legend.position = "right",
                                            legend.direction = "vertical",
                                            plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
                                      ) +
                                      guides(fill = guide_legend(title = "")) +
                                      scale_fill_manual(values = mycolors)



# personale con esperienza in CDM ----------------------------------

esp_cdm <- as.data.frame(table(survey[10], dnn = c("ans", "Freq")))

esp_cdm$ans.1 <- str_replace(esp_cdm$ans.1, "Si", "Yes")
esp_cdm$ans.1 <- str_replace(esp_cdm$ans.1, "Non so", "Unsure")

#frazione delle variabili nel donut
esp_cdm$fraction = esp_cdm$Freq / sum(esp_cdm$Freq)

# top e bottom di ogni rettangolo per il donut
esp_cdm$ymax = cumsum(esp_cdm$fraction)
esp_cdm$ymin = c(0, head(esp_cdm$ymax, n=-1))

# Compute label position
esp_cdm$labelPosition <- (esp_cdm$ymax + esp_cdm$ymin) / 2


## donut

personnel5 <- ggplot(esp_cdm,  aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=ans.1)) +
                                   geom_rect() +
                                   xlim(c(2, 4)) +
                                   coord_polar("y", start=0)+
                                   geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), fontface = "bold") +
                                   theme_void()+
                                   labs(title = "Presence of personnel proficient in Common Data Models", x = "", y = "", ) +
                                   theme(legend.position = "right",
                                         legend.direction = "vertical",
                                         plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
                                   ) +
                                   guides(fill = guide_legend(title = "")) + 
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

cdm <- as.data.frame(table(cdm, dnn = c("ans", "Freq")))

cdm$ans.1<- factor(cdm$ans.1, levels = cdm$ans.1)

## barplot cdm usati

cdm <- ggplot(cdm, aes(x = ans.1, y = Freq, fill =ans.1)) +
                            geom_bar(stat = "identity", width = 0.5) +
                            theme_minimal()+
                            geom_rect(
                              aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                              fill = NA,
                              color = "lightgrey") +
                            scale_y_discrete(limits = as.factor(c(1:14))) +
                            geom_text(aes(label = Freq),vjust = -0.5) +
                            labs(title = "Adopted Common Data Models", x = "", y = "", ) +
                            theme(legend.position = "right",
                                  legend.direction = "vertical",
                                  plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
                            guides(fill = guide_legend(title = "CDM"))+
                            scale_fill_manual(values = mycolors)



# personnel FHIR expert -----------------------------------

fhir <- as.data.frame(table(survey[12], dnn = c("ans", "Freq")))

#frazione delle variabili nel donut
fhir$fraction = fhir$Freq / sum(fhir$Freq)

# top e bottom di ogni rettangolo per il donut
fhir$ymax = cumsum(fhir$fraction)
fhir$ymin = c(0, head(fhir$ymax, n=-1))

# Compute label position
fhir$labelPosition <- (fhir$ymax + fhir$ymin) / 2


fhir <- ggplot(fhir, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = ans.1)) +
                             geom_rect() +
                             xlim(c(2, 4)) +
                             geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), fontface = "bold") +
                             coord_polar("y", start=0)+
                             theme_void()+
                             labs(title = "Experience with HL7 FHIR", x = "", y = "", ) +
                             theme(legend.position = "right",
                                   legend.direction = "vertical",
                                   plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
                             ) +
                             guides(fill = guide_legend(title = "proficient personnel")) +
                             scale_fill_manual(values = mycolors)


# bims

lim <- as.data.frame(table(survey[13], dnn =c("ans", "Freq")))

#frazione delle variabili nel donut
lim$fraction = lim$Freq / sum(lim$Freq)

# top e bottom di ogni rettangolo per il donut
lim$ymax = cumsum(lim$fraction)
lim$ymin = c(0, head(lim$ymax, n=-1))

# Compute label position
lim$labelPosition <- (lim$ymax + lim$ymin) / 2

#donut plot

bims <- ggplot(lim, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=ans.1)) +
                             geom_rect() +
                             xlim(c(2, 4)) +
                             geom_text(x= 3.5, 
                                       aes(y=labelPosition, 
                                           label = paste0("N = ", Freq, ",\n", 
                                                          round(fraction*100, digit = 1), "%")), 
                                       fontface = "bold") +
                             coord_polar("y", start=0)+
                             theme_void()+
                             labs(title = "The institution has a computer system o LIMS-Database dedicated to the data associated 
       with the biological samples in the Biobank", x = "", y = "", ) +
                             theme(legend.position = "right",
                                   legend.direction = "vertical",
                                   plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
                             ) +
                             guides(fill = guide_legend(title = "")) +
                             scale_fill_manual(values = mycolors)


# bims2

sist <- survey[[14]] %>%
  na.omit() %>%
  strsplit(";|- ") %>%
  unlist() %>%
  as.data.frame()

sist$. <- ifelse(grepl("easy", sist$., ignore.case = TRUE), "EASYTRACK", sist$.)
sist$. <- ifelse(grepl("bio manag", sist$., ignore.case = TRUE), "Bio Management",sist$.)
sist$. <- ifelse(grepl("red", sist$., ignore.case = TRUE), "REDcap",sist$.)
sist$. <- ifelse(grepl("td", sist$., ignore.case = TRUE), "TDBioBank",sist$.)
sist$. <- ifelse(grepl("home|house", sist$., ignore.case = TRUE), "auto sviluppato",sist$.)
sist$. <- ifelse(grepl("wins", sist$., ignore.case = TRUE), "WINSAP",sist$.)
sist$. <- ifelse(grepl("crios", sist$., ignore.case = TRUE), "CryoSMART",sist$.)

sist$. <- ifelse(grepl("FREEZERWORKS", sist$., ignore.case = TRUE), "FREEZERWORKS",sist$.)
sist$. <- ifelse(grepl("labvantage", sist$., ignore.case = TRUE), "LabVantage",sist$.)
sist$. <- ifelse(grepl("tngb", sist$., ignore.case = TRUE), "TNGB",sist$.)
sist$. <- ifelse(grepl("olohealth", sist$., ignore.case = TRUE), "Olohealth",sist$.)
sist$. <- ifelse(grepl("NAUTILUS", sist$., ignore.case = TRUE), "NAUTILUS",sist$.)
sist$. <- ifelse(grepl("smartlab", sist$., ignore.case = TRUE), "SmartLab",sist$.)
sist$. <- ifelse(grepl("MBioLIMS", sist$., ignore.case = TRUE), "MBioLIMS",sist$.)
sist$. <- ifelse(grepl("SmartyBioB", sist$., ignore.case = TRUE), "SmartyBioB",sist$.)

sist$. <- str_remove_all(sist$., "\\(.*$")
sist$. <- str_remove_all(sist$., "\\,.*$")
sist$. <- str_remove_all(sist$., " $")
sist$. <- str_remove_all(sist$., "^ ")

sist_lims <- c("EASYTRACK", "TDBioBank", "LabVantage","REDcap", "Bio Management","NAUTILUS", "MBioLIMS","FREEZERWORKS","TNGB","Olohealth","SmartLab","SmartyBioB","Noraybank")

lims <- c()
NOlims <- c()

for(n in sist$.) {
  
  if (n %in% sist_lims) {
    lims <- append(lims, n)
  } else { 
    NOlims <- append(NOlims, n)
  }
  
}

sist <- as.data.frame(table(sist), stringsAsFactors = F)

for (a in 1:length(sist$.)) {
  
  ifelse((sist$Freq[a] <= 1) && !(sist$.[a] %in% sist_lims), sist$.[a] <-"altro", sist$.)
  
}

sist <- sist %>%
  group_by(sist$.) %>%
  summarize(Freq = sum(Freq)) %>%
  arrange(desc(Freq)) %>%
  move2last(1)

sist$`sist$.` <- factor(sist$`sist$.`, levels = sist$`sist$.`)


# bims 3 ----------------------------------------------

lims <- as.data.frame(table(lims), stringsAsFactors = F)
lims <- lims[order(lims$Freq, decreasing = T),]
lims$lims <- factor(lims$lims, levels = lims$lims)

lims$lims <- paste("BIMS_", seq(1, length(lims$lims)), sep = "")
lims$lims <- factor(lims$lims, levels = lims$lims)

# donut

bims3 <- ggplot(lims, aes(x = lims, y = Freq, fill = lims)) +
                              geom_bar(stat = "identity", width = 0.5) +
                              theme_minimal() +
                              geom_text(aes(label = Freq),vjust = -0.5) +
                              geom_rect(
                                aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                                fill = NA,
                                color = "lightgrey"
                              ) +
                              scale_x_discrete(limits = levels(lims$lims)) +
                              labs(title = "Used LIMS or BIMS software", x = "", y= "") +
                              theme(legend.position = "right",
                                    axis.text.x = element_text(angle = -30, vjust = 0, hjust = 0.2),
                                    legend.direction = "vertical",
                                    plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
                              guides(fill = guide_legend(title = "")) +
                              scale_fill_manual(values = mycolors)



# clinical data ----------------------------------------

link <- survey[[15]] %>%
  tolower() %>%
  na.omit() %>%
  as.data.frame()

link$. <- ifelse(grepl("si\\b|si |sì|si:|si,|collegato|linked", link$.), "si", link$.)
link$. <- ifelse(grepl("previs|svilup", link$.), "in previsione", link$.)
link$. <- ifelse(grepl("na|no,|no", link$.), "no", link$.)
link <- as.data.frame(table(link)) 
link <- link[order(link$Freq, decreasing = T),]
link$. <- factor(link$., levels = link$.)

# barplot 

clindata <- ggplot(link, aes(x = ., y = Freq, fill = .)) +
                                 geom_bar(stat = "identity", width = 0.5) +
                                 theme_minimal() +
                                 geom_text(aes(label = Freq),vjust = -0.5) +
                                 geom_rect(
                                   aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                                   fill = NA,
                                   color = "lightgrey") +
                                 scale_x_discrete(limits = levels(link$.)) +
                                 labs(title = "the IT system is linked to clinical data", x = "", y= "") +
                                 theme(legend.position = "right",
                                       legend.direction = "vertical",
                                       plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
                                 guides(fill = guide_legend(title = "")) +
                                 scale_fill_manual(values = mycolors)



# bb data infrastructure ---------------------------------------------------

inf <- as.data.frame(table(survey[[17]]))

inf$Var1 <- str_replace(inf$Var1, "Si", "Yes")
inf$Var1 <- str_replace(inf$Var1, "Non so", "Unsure")

#frazione delle variabili nel donut
inf$fraction = inf$Freq / sum(inf$Freq)

# top e bottom di ogni rettangolo per il donut
inf$ymax = cumsum(inf$fraction)
inf$ymin = c(0, head(inf$ymax, n=-1))

# Compute label position
inf$labelPosition <- (inf$ymax + inf$ymin) / 2

# donut

infradata <- ggplot(inf, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
                                  geom_rect() +
                                  xlim(c(2, 4)) +
                                  geom_text(x= 3.5, 
                                            aes(y=labelPosition, 
                                                label = paste0("N = ", Freq, ",\n", 
                                                               round(fraction*100, digit = 1), "%")), 
                                            fontface = "bold") +
                                  coord_polar("y", start=0)+
                                  theme_void()+
                                  labs(title = "The institution has access to an informatic infrastructure dedicated to the data 
           and/or data processing associated with the biobanked biological sample", x = "", y = "", ) +
                                  theme(legend.position = "right",
                                        legend.direction = "vertical",
                                        plot.title = element_text(face = "bold", size = 12, hjust = 0.5)) +
                                  guides(fill = guide_legend(title = "")) +
                                  scale_fill_manual(values = mycolors)


# storage ----------------------------------------------

storage <- as.data.frame(table(survey[[19]]))

storage$Var1 <- str_replace(storage$Var1, "Si", "Yes")
storage$Var1 <- str_replace(storage$Var1, "Non so", "Unsure")

# donut
storage$fraction = storage$Freq / sum(storage$Freq)
storage$ymax = cumsum(storage$fraction)
storage$ymin = c(0, head(storage$ymax, n=-1))
storage$labelPosition <- (storage$ymax + storage$ymin) / 2

storage <- ggplot(storage, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
                                geom_rect() +
                                xlim(c(2, 4)) +
                                geom_text(x= 3.5, 
                                          aes(y=labelPosition, 
                                              label = paste0("N = ", Freq, ",\n", 
                                                             round(fraction*100, digit = 1), "%")), 
                                          fontface = "bold") +
                                coord_polar("y", start=0)+
                                theme_void()+
                                labs(title = "Availability of a massive storage system", x = "", y = "", ) +
                                theme(legend.position = "right",
                                      legend.direction = "vertical",
                                      plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
                                guides(fill = guide_legend(title = "")) +
                                scale_fill_manual(values = mycolors)


# federal research ------------------------------------

ric_fed <- as.data.frame(table(survey[[20]]))

# donut

ric_fed$Var1 <- str_replace(ric_fed$Var1, "Si", "Yes")

ric_fed$fraction = ric_fed$Freq / sum(ric_fed$Freq)
ric_fed$ymax = cumsum(ric_fed$fraction)
ric_fed$ymin = c(0, head(ric_fed$ymax, n=-1))
ric_fed$labelPosition <- (ric_fed$ymax + ric_fed$ymin) / 2

fedres <- ggplot(ric_fed, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
                               geom_rect() +
                               xlim(c(2, 4)) +
                               geom_text(x= 3.5, 
                                         aes(y=labelPosition, 
                                             label = paste0("N = ", Freq, ",\n", 
                                                            round(fraction*100, digit = 1), "%")), 
                                         fontface = "bold") +
                               coord_polar("y", start=0)+
                               theme_void()+
                               labs(title = "The Biobank can allocate computational resources indipendently 
       or by the IT departement for Federated Search service",
                                    x = "", y = "", ) +
                               theme(legend.position = "right",
                                     legend.direction = "vertical",
                                     plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
                                     plot.subtitle = element_text(size = 12, hjust = 0.5)) +
                               guides(fill = guide_legend(title = "")) +
                               scale_fill_manual(values = mycolors)


# dwh ----------------------------------------------

dwh <- as.data.frame(table(survey[[22]]))

# donut

dwh$Var1 <- str_replace(dwh$Var1, "Si", "Yes")
dwh$Var1 <- str_replace(dwh$Var1, "Non so", "Unsure")
dwh$fraction = dwh$Freq / sum(dwh$Freq)
dwh$ymax = cumsum(dwh$fraction)
dwh$ymin = c(0, head(dwh$ymax, n=-1))
dwh$labelPosition <- (dwh$ymax + dwh$ymin) / 2

dwh <- ggplot(dwh, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
                            geom_rect() +
                            xlim(c(2, 4)) +
                            geom_text(x= 3.5, 
                                      aes(y=labelPosition, 
                                          label = paste0("N = ", Freq, ",\n", 
                                                         round(fraction*100, digit = 1), "%")), 
                                      fontface = "bold") +
                            coord_polar("y", start=0)+
                            theme_void()+
                            labs(title = "Usage of Data Warehouse or Data Lake", x = "", y = "", ) +
                            theme(legend.position = "right",
                                  legend.direction = "vertical",
                                  plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
                            ) +
                            guides(fill = guide_legend(title = "")) +
                            scale_fill_manual(values = mycolors)


# omics --------------------------------------

omiche <- survey[[26]] %>%
  na.omit() %>%
  strsplit(";|,") %>%
  unlist() %>%
  as.data.frame()

altre_omiche <- c()

for (omica in 1:length(omiche$.)) {
  if(omiche$.[omica] %in% c("Proteomica","Metabolomica","Genomica"))  {
    omica <- omica 
  } else {
    altre_omiche <- c(altre_omiche, omiche$.[omica])
    omiche$.[omica] <- "Some other"
  }
}  

omiche <- table(omiche) %>%
  as.data.frame() %>%
  move2last(1)

omiche <- omiche[order(omiche$Freq, decreasing = T),]

omiche$. <- str_replace(omiche$., "Proteomica", "Proteomicsc")
omiche$. <- str_replace(omiche$., "Metabolomica", "Metabolomics")
omiche$. <- str_replace(omiche$., "Genomica", "Genomics")

omiche$. <-  factor(omiche$., levels = omiche$.)

# barplot 

omics <- ggplot(omiche, aes(x = ., y = Freq, fill = .)) +
                              geom_bar(stat = "identity", width = 0.5, color = "transparent") +
                              theme_minimal()+
                              geom_rect(
                                aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                                fill = NA,
                                color = "lightgrey"
                              ) +
                              geom_text(aes(label = Freq),vjust = -0.5) +
                              labs(title = "'omics' characterization platform", x = "", y = "") +
                              theme(legend.position = "right",
                                    legend.direction = "vertical",
                                    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
                              guides(fill = guide_legend(title = "")) +
                              scale_fill_manual(values = mycolors)


# data -----------------------------------------------

dati <- survey[[29]] %>%
  strsplit(";|,") %>%
  unlist() %>%
  as.data.frame() 

risp29 <- c("malattie rare","oncologia","metagenomica batterica","metagenomica virale")

for (a in 1:length(dati$.)) {
  
  ifelse(!(dati$.[a] %in% risp29),dati$.[a] <-"Some other", dati$.)
  
}

dati <- as.data.frame(table(dati))

dati$. <- str_replace(dati$., "malattie rare", "Rare disease")
dati$. <- str_replace(dati$., "oncologia", "Oncology")
dati$. <- str_replace(dati$., "metagenomica batterica", "Bacterial metagenomics")
dati$. <- str_replace(dati$., "metagenomica virale", "Viral metagenomics")

dati <- dati[order(dati$Freq),]
dati$. <- factor(dati$., levels =dati$.)

# donut
dati$fraction = dati$Freq / sum(dati$Freq)
dati$ymax = cumsum(dati$fraction)
dati$ymin = c(0, head(dati$ymax, n=-1))
dati$labelPosition <- (dati$ymax + dati$ymin) / 2

data <- ggplot(dati, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=.)) +
                             geom_rect() +
                             xlim(c(2, 4)) +
                             geom_text(x= 4.2, 
                                       aes(y=labelPosition, 
                                           label = paste0("N = ", Freq, ",\n", 
                                                          round(fraction*100, digit = 1), "%")), 
                                       size = 3, fontface = "bold") +
                             coord_polar("y", start=0)+
                             theme_void() +
                             labs(title = "Biobanked data specialization", x = "", y = "", ) +
                             theme(legend.position = "right",
                                   legend.direction = "vertical",
                                   plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
                             guides(fill = guide_legend(title = "Data belonging to:")) +
                             scale_fill_manual(values = mycolors)


# data type

tip_dati <- survey[[30]] %>%
  strsplit(";") %>%
  unlist()

risp30 <- c("Lifestyle dataset", "Environmental dataset", "Physiological dataset", "Biochemical dataset", "Clinical dataset", "Psychological dataset", "Genomic dataset (sequences)", "Genomic dataset (variants)", "Proteomic dataset", "Metabolomic dataset", "Body (Radiological) image", "Whole slide image", "Photo image", "Genealogical records")

for (a in 1:length(tip_dati)) {
  ifelse(!(tip_dati[a] %in% risp30),tip_dati[a] <-"Some other", tip_dati)
}

tip_dati<- as.data.frame(table(tip_dati)) %>%
  move2last(1)

tip_dati <- tip_dati[order(tip_dati$Freq, decreasing = T),]
tip_dati$tip_dati <- factor(tip_dati$tip_dati, levels = tip_dati$tip_dati)

# barplot

data2 <-  ggplot(tip_dati, aes(x = tip_dati, y = Freq, fill = tip_dati)) +
                              geom_bar(stat = "identity", width = 0.5, color = "transparent", show.legend = F) +
                              theme_minimal()+
                              geom_rect(
                                aes(xmin = -Inf, xmax = 17, ymin = -Inf, ymax = 39),
                                fill = NA,
                                color = "lightgrey"
                              ) +
                              geom_text(aes(label = Freq),vjust = -0.5) +
                              labs(title = "Data type correlated to Biobanks", x = "", y = "") +
                              theme(legend.position = "right",
                                    axis.text.x = element_text(angle = -45, hjust = 0),
                                    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
                              guides(fill = guide_legend(title = "")) +
                              scale_fill_manual(values = mycolors)


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

altre_ont <- c()

for (ontologie in 1:length(ont)) {
  if(ont[ontologie] %in% risp31)  {
    ontologie <- ontologie 
  } else {
    altre_ont <- c(altre_ont, ont[ontologie])
    ont[ontologie] <- "Some other"
  }
}  

ont <- as.data.frame(table(ont)) 

ont <- ont[order(ont$Freq, decreasing = T), ]
ont <- move2last(ont, 8) %>%
  move2last(3) %>%
  move2last(1)

ont$ont <- factor(ont$ont, levels = ont$ont)

# barplot ontologie

onto <- ggplot(ont, aes(x = ont, y = Freq, fill = ont)) +
                             geom_bar(stat = "identity", width = 0.5, color = "transparent") +
                             theme_minimal()+
                             geom_rect(
                               aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                               fill = NA,
                               color = "lightgrey"
                             ) +
                             geom_text(aes(label = Freq),vjust = -0.5) +
                             labs(title = "Ontologies reference associated to data", x = "", y = "") +
                             theme(legend.position = "right",
                                   legend.direction = "vertical",
                                   plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
                             guides(fill = guide_legend(title = "")) +
                             scale_fill_manual(values = mycolors)


# data cross-ref ----------------------------------

cross <- as.data.frame(table(survey[[34]]))

cross$Var1 <- str_replace(cross$Var1, "Si", "Yes")
cross$Var1 <- str_replace(cross$Var1, "Non so", "Unsure")

# donut
cross$fraction = cross$Freq / sum(cross$Freq)
cross$ymax = cumsum(cross$fraction)
cross$ymin = c(0, head(cross$ymax, n=-1))
cross$labelPosition <- (cross$ymax + cross$ymin) / 2

data3 <- ggplot(cross, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
                              geom_rect() +
                              xlim(c(2, 4)) +
                              geom_text(x= 3.5, aes(y=labelPosition, 
                                                    label = paste0("N = ", Freq, ",\n", 
                                                                   round(fraction*100, digit = 1), "%")), 
                                        fontface = "bold") +
                              coord_polar("y", start=0)+
                              theme_void()+
                              labs(title = "It is possible to cross-reference data with ones on another 
           institution's or terrritorial system", x = "", y = "", ) +
                              theme(legend.position = "right",
                                    legend.direction = "vertical",
                                    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
                              ) +
                              guides(fill = guide_legend(title = "")) +
                              scale_fill_manual(values = mycolors)


# cross data 2 ---------------------------------------

sist_cross <- survey[[35]] %>%
  na.omit() %>%
  strsplit(";") %>%
  unlist() %>%
  table() %>%
  as.data.frame( stringsAsFactors = F)

sist_cross <- sist_cross[order(sist_cross$Freq, decreasing = T),]
sist_cross <- sist_cross[1:3,]
sist_cross$. <- str_replace(sist_cross$., "è possìbile incrociare i dati con il sìstema informativo clinico dell'infrastruttura a cui afferisce la biobanca", 
                            "It is possible to cross-reference data with clnical information system of the infrastructure affiliated to the Biobank")
sist_cross$. <- str_replace(sist_cross$.,"è possìbile incrociare i dati con sìstemi informativi clinici territoriali a cui è collegata l'istituzione a cui afferisce la biobanca",
                            "It is possible to cross-reference data with territorial clinical information system connected to  the Biobank affiliated institution" )
sist_cross$. <- str_replace(sist_cross$.,"è possibile incrcociare ma non in modo automatico", 
                            "it is possible but not in automated way")
sist_cross$.<- factor(sist_cross$., levels = sist_cross$.)

# barplot quali sistemi

data4 <- ggplot(sist_cross, aes(x = ., y = Freq, fill = .)) +
                              geom_bar(stat = "identity", width = 0.5, color = "transparent") +
                              theme_minimal()+
                              geom_rect(
                                aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
                                fill = NA,
                                color = "lightgrey"
                              ) +
                              geom_text(aes(label = Freq),vjust = -0.1) +
                              labs(title = "How the data are cross-referenced", x = "", y = "") +
                              scale_x_discrete(labels = "", "", "") +
                              theme(legend.position = "bottom",
                                    legend.direction = "vertical",
                                    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
                              guides(fill = guide_legend(title = "",)) +
                              scale_fill_manual(values = mycolors)



# digital informe consent -----------------------------------

cons_inf <- as.data.frame(table(survey[[36]]))
cons_inf$Var1 <- str_replace(cons_inf$Var1, "Si", "Yes")

# donut
cons_inf$fraction = cons_inf$Freq / sum(cons_inf$Freq)
cons_inf$ymax = cumsum(cons_inf$fraction)
cons_inf$ymin = c(0, head(cons_inf$ymax, n=-1))
cons_inf$labelPosition <- (cons_inf$ymax + cons_inf$ymin) / 2


ic <- ggplot(cons_inf,aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
                           geom_rect() +
                           xlim(c(2, 4)) +
                           coord_polar("y", start=0)+
                           geom_text(x= 3.5, 
                                     aes(y=labelPosition, 
                                         label = paste0("N = ", Freq, ",\n", 
                                                        round(fraction*100, digit = 1), "%")), 
                                     fontface = "bold") +
                           theme_void()+
                           labs(title = "Usage of digital-electronic informed consent", x = "", y = "", ) +
                           theme(legend.position = "right",
                                 legend.direction = "vertical",
                                 plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
                           guides(fill = guide_legend(title = "")) +
                           scale_fill_manual(values = mycolors)



