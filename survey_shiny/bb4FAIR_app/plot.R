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



# dedicated personnel ----------------------

p_dedicato <- as.data.frame(table(survey[[5]]), stringsAsFactors = FALSE)

p_dedicato$Var1 <- ifelse(grepl("Si", ignore.case = T, p_dedicato$Var1), "Yes", p_dedicato$Var1)

#frazione delle variabili nel donut
p_dedicato$fraction = p_dedicato$Freq / sum(p_dedicato$Freq)

# top e bottom di ogni rettangolo per il donut
p_dedicato$ymax = cumsum(p_dedicato$fraction)
p_dedicato$ymin = c(0, head(p_dedicato$ymax, n=-1))

# Compute label position
p_dedicato$labelPosition <- (p_dedicato$ymax + p_dedicato$ymin) / 2

# donut plot

chart_personnel <- ggplot(p_dedicato, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y") +
  xlim(c(2, 4)) +
  theme_void() +
  labs(title = "Presence of dedicated personnel") +
  guides(fill = guide_legend(title = "Answers")) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5) )+
  scale_fill_manual(values = mycolors)


# personnel activities ---------------------------

attiv <- survey[[6]] %>%
  na.omit() %>%
  strsplit(";") %>%
  unlist() %>%
  table() %>%
  as.data.frame() 


attiv$. <- str_replace(attiv$.,"Gestione dei campioni", "Samples handling")
attiv$. <- str_replace(attiv$.,"Gestione dei dati associati ai campioni", "Specimen associated data management")
attiv$. <- str_replace(attiv$.,"Gestione della qualità", "Quality management")
attiv$. <- ifelse(grepl("Compliance", ignore.case = T, attiv$.), "Ethical-legal-social compliance (Informed consents, data protection, feedback results, access)", attiv$.)

attiv <-attiv[order(attiv$Freq, decreasing = T),]
attiv$. <- factor(attiv$., levels = attiv$.)

# Barplot attività del personale

chart_personnel2 <- ggplot(attiv, aes(x = ., y = Freq, fill =.)) +
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
        panel.background = element_rect()) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))

# data annotation personnel ------------------------------

pers_annotazione <- as.data.frame(table(survey[[8]]))

pers_annotazione$Var1 <- str_replace(pers_annotazione$Var1, "Si", "Yes")
pers_annotazione$Var1 <- str_replace(pers_annotazione$Var1, "Non so", "Unsure")


#frazione delle variabili nel donut
pers_annotazione$fraction = pers_annotazione$Freq / sum(pers_annotazione$Freq)

# top e bottom di ogni rettangolo per il donut
pers_annotazione$ymax = cumsum(pers_annotazione$fraction)
pers_annotazione$ymin = c(0, head(pers_annotazione$ymax, n=-1))

# Compute label position
pers_annotazione$labelPosition <- (pers_annotazione$ymax + pers_annotazione$ymin) / 2


# donut

chart_personnel3 <- ggplot(pers_annotazione, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= pers_annotazione$Var1)) +
  geom_rect() +
  coord_polar("y", start=0)+
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")),size = 5, fontface = "bold") +
  theme_void() +
  guides(fill = guide_legend(title = "Answers")) +
  labs(title = "Staff with experience in data annotation \nand data modelling") +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  ) +
  scale_fill_manual(values = mycolors)


# staff composition -----------------------------------

pers_annotazione <- as.data.frame(table(survey[[8]]))

pers_annotazione$Var1 <- str_replace(pers_annotazione$Var1, "Si", "Yes")
pers_annotazione$Var1 <- str_replace(pers_annotazione$Var1, "Non so", "Unsure")


#frazione delle variabili nel donut
pers_annotazione$fraction = pers_annotazione$Freq / sum(pers_annotazione$Freq)

# top e bottom di ogni rettangolo per il donut
pers_annotazione$ymax = cumsum(pers_annotazione$fraction)
pers_annotazione$ymin = c(0, head(pers_annotazione$ymax, n=-1))

# Compute label position
pers_annotazione$labelPosition <- (pers_annotazione$ymax + pers_annotazione$ymin) / 2


# donut

chart_personnel3 <- ggplot(pers_annotazione, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill= pers_annotazione$Var1)) +
  geom_rect() +
  coord_polar("y", start=0)+
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")),size = 5, fontface = "bold") +
  theme_void() +
  guides(fill = guide_legend(title = "Answers")) +
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

term<- as.data.frame(table(term), stringsAsFactors = FALSE)


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
        panel.background = element_rect()) +
  guides(fill = guide_legend(title = "")) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# personale con esperienza in CDM ----------------------------------

esp_cdm <- as.data.frame(table(survey[[10]]))

esp_cdm$Var1 <- str_replace(esp_cdm$Var1, "Si", "Yes")
esp_cdm$Var1 <- str_replace(esp_cdm$Var1, "Non so", "Unsure")

#frazione delle variabili nel donut
esp_cdm$fraction = esp_cdm$Freq / sum(esp_cdm$Freq)

# top e bottom di ogni rettangolo per il donut
esp_cdm$ymax = cumsum(esp_cdm$fraction)
esp_cdm$ymin = c(0, head(esp_cdm$ymax, n=-1))

# Compute label position
esp_cdm$labelPosition <- (esp_cdm$ymax + esp_cdm$ymin) / 2


# donut

chart_personnel4 <- ggplot(esp_cdm,  aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=esp_cdm[[1]])) +
  geom_rect() +
  xlim(c(2, 4)) +
  coord_polar("y", start=0)+
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  theme_void()+
  labs(title = "Is the Biobank staff proficient in Common Data Models?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
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

# barplot cdm usati

chart_cdm <-ggplot(cdm, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal()+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -30, hjust = 0.15, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect()) +
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "Adopted Common Data Models", x = "Answers", y = "N Biobanks") +
  guides(fill = guide_legend(title = "CDM"))+
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# personnel FHIR expert -----------------------------------

fhir <- as.data.frame(table(survey[[12]]))

#frazione delle variabili nel donut
fhir$fraction = fhir$Freq / sum(fhir$Freq)

# top e bottom di ogni rettangolo per il donut
fhir$ymax = cumsum(fhir$fraction)
fhir$ymin = c(0, head(fhir$ymax, n=-1))

# Compute label position
fhir$labelPosition <- (fhir$ymax + fhir$ymin) / 2


# donut 

chart_personnel5 <- ggplot(fhir, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = fhir[[1]])) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Does the staff have experience with HL7 FHIR?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors)

# bims --------------------------------------

lim <- as.data.frame(table(survey[[13]]))

lim$Var1 <- str_replace(lim$Var1, "Si", "Yes")

#frazione delle variabili nel donut
lim$fraction = lim$Freq / sum(lim$Freq)

# top e bottom di ogni rettangolo per il donut
lim$ymax = cumsum(lim$fraction)
lim$ymin = c(0, head(lim$ymax, n=-1))

# Compute label position
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
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors)


# bims2 -------------------------------------------

sist <- survey[[14]] %>%
  na.omit() %>%
  strsplit(";|- ") %>%
  unlist() %>%
  as.data.frame()


lims <- c()
NOlims <- c()

for(n in sist$.) {

  if (n == "NOBIMS") {
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

# clinical data ----------------------------------------

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
        panel.background = element_rect()) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))



# storage without lims ----------------------------

nosist <- survey[[16]] %>%
  na.omit %>%
  as.data.frame()

risp16 <- c("Tabelle Excel o simili compilate manualmente", "Stesso Database-LIMS di altro dipartimento (e.g. Anatomia Patologica)")

nosist$. <- ifelse(!(nosist$. %in% risp16), "Some other", nosist$.) 

nosist$. <- str_replace(nosist$., "Tabelle Excel o simili compilate manualmente", "Excel sheet or similar, manually filled")
nosist[2,1] <- "Database or LIMS from another departement (e.g. anatomical pathology)"

nosist <-as.data.frame(table(nosist))

#frazione delle variabili nel donut
nosist$fraction = nosist$Freq / sum(nosist$Freq)

# top e bottom di ogni rettangolo per il donut
nosist$ymax = cumsum(nosist$fraction)
nosist$ymin = c(0, head(nosist$ymax, n=-1))

# Compute label position
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
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers")) +
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

# donut se presente infrastruttura

chart_infrstr <- ggplot(inf, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Does the institution have access to an IT infrastructure dedicated to the data \nand/or data processing associated with the biobanked biological sample?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors)

# IT infrastructure management -------------------------------

risp18 <- c("dal personale della biobanca","da un gruppo dedicato afferente all'istituto (e.g., il Centro Elaborazione Dati)", "fornitore esterno, non-commerciale (e.g., CINECA, INFN)",  "fornitore esterno, commerciale (e.g., AWS, Azure, etc.)")

gestione <- survey[[18]] %>%
  na.omit 
gestione<- ifelse(!(gestione %in% risp18), "Some other", gestione) 

gestione <- as.data.frame(table(gestione))

gestione$gestione <- str_replace(gestione$gestione, "dal personale della biobanca", "By Biobank staff")
gestione$gestione <- ifelse(grepl("gruppo dedicato afferente", gestione$gestione), "By dedicated staff relating to the institute (e.g. Data Processing Center)", gestione$gestione)
gestione$gestione <- ifelse(grepl("fornitore esterno, non-commerciale", gestione$gestione), "Non-bussiness third-party provider (e.g., CINECA, INFN)", gestione$gestione)
gestione$gestione <- ifelse(grepl("fornitore esterno, commerciale ", gestione$gestione), "Bussiness third-party provider (e.g., AWS, Azure, etc.)", gestione$gestione)

gestione$gestione <- factor(gestione$gestione, levels = gestione$gestione)


# barplot 

chart_infrstr2 <- ggplot(gestione, aes(x = gestione, y = Freq, fill =gestione)) +
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
        panel.background = element_rect()) +
  guides(fill = guide_legend(title = "Ansewrs")) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))

# storage ----------------------------------------------

storage <- as.data.frame(table(survey[[19]]))

storage$Var1 <- str_replace(storage$Var1, "Si", "Yes")
storage$Var1 <- str_replace(storage$Var1, "Non so", "Unsure")

#frazione delle variabili nel donut
storage$fraction = storage$Freq / sum(storage$Freq)

# top e bottom di ogni rettangolo per il donut
storage$ymax = cumsum(storage$fraction)
storage$ymin = c(0, head(storage$ymax, n=-1))

# Compute label position
storage$labelPosition <- (storage$ymax + storage$ymin) / 2

# donut sotrage massivo

chart_storage <- ggplot(storage, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Availability of a massive storage system", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors)


# federated search ------------------------------------

ric_fed <- as.data.frame(table(survey[[20]]))

ric_fed$Var1 <- str_replace(ric_fed$Var1, "Si", "Yes")

#frazione delle variabili nel donut
ric_fed$fraction = ric_fed$Freq / sum(ric_fed$Freq)

# top e bottom di ogni rettangolo per il donut
ric_fed$ymax = cumsum(ric_fed$fraction)
ric_fed$ymin = c(0, head(ric_fed$ymax, n=-1))

# Compute label position
ric_fed$labelPosition <- (ric_fed$ymax + ric_fed$ymin) / 2

# donut

chart_fed_search <- ggplot(ric_fed, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Can the Biobank, indipendently or by the IT departement, 
       allocate computational resources for Federated Search service?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers", reverse = T)) +
  scale_fill_manual(values = mycolors)


# federated search resoures ----------------------------------

risorse_fed <- survey[[21]] %>%
  na.omit() %>%
  strsplit(";") %>%
  unlist() %>%
  table() %>%
  as.data.frame( stringsAsFactors = F)

risp21 <- c("macchina virtuale","software containers","computer fisico dedicato")


for (a in 1:length(risorse_fed$.)) {
  
  ifelse(!(risorse_fed$.[a] %in% risp21),risorse_fed$.[a] <-"Some other", risorse_fed$.)
  
}

risorse_fed <- risorse_fed %>%
  group_by(risorse_fed$.) %>%
  summarize(Freq = sum(Freq)) %>%
  move2last(1) 

risorse_fed <- risorse_fed[order(risorse_fed$Freq, decreasing = T),]
risorse_fed$`risorse_fed$.` <- str_replace(risorse_fed$`risorse_fed$.`, "macchina virtuale", "Virtual machine")
risorse_fed$`risorse_fed$.` <- str_replace(risorse_fed$`risorse_fed$.`, "computer fisico dedicato", "Physical dedicated computer")
risorse_fed$`risorse_fed$.` <-  factor(risorse_fed$`risorse_fed$.` , levels = risorse_fed$`risorse_fed$.`)

# barplot risorse per allocazione dati

chart_fed_search2 <- ggplot(risorse_fed, aes(x = `risorse_fed$.`, y = Freq, fill = `risorse_fed$.`)) +
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

#frazione delle variabili nel donut
dwh$fraction = dwh$Freq / sum(dwh$Freq)

# top e bottom di ogni rettangolo per il donut
dwh$ymax = cumsum(dwh$fraction)
dwh$ymin = c(0, head(dwh$ymax, n=-1))

# Compute label position
dwh$labelPosition <- (dwh$ymax + dwh$ymin) / 2

# donut collettore dati

chart_dwh <- ggplot(dwh, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Data Warehouse or Data Lake presence", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors)


# dwh location ------------------------------------

str_dwh <- survey[[23]] %>%
  na.omit() %>%
  table() %>%
  as.data.frame()

str_dwh$. <- str_replace(str_dwh$., "in sistemi fisici dell'istituto", "Institute's physical system")
str_dwh$. <- str_replace(str_dwh$., "su cloud", "Cloud")

str_dwh <- str_dwh[order(str_dwh$Freq, decreasing = T),]
str_dwh$.<- factor(str_dwh$., levels = str_dwh$.)

# barplot struttura

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
        panel.background = element_rect()) +
  guides(fill = guide_legend(title = "")) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# 
# ngs lab -------------------------------

ngs <- as.data.frame(table(survey[[24]]))

ngs$Var1 <- str_replace(ngs$Var1, "Si", "Yes")
ngs$Var1 <- str_replace(ngs$Var1, "Non so", "Unsure")

#frazione delle variabili nel donut
ngs$fraction = ngs$Freq / sum(ngs$Freq)

# top e bottom di ogni rettangolo per il donut
ngs$ymax = cumsum(ngs$fraction)
ngs$ymin = c(0, head(ngs$ymax, n=-1))

# Compute label position
ngs$labelPosition <- (ngs$ymax + ngs$ymin) / 2

# donut accesso a ngs

chart_ngs <- ggplot(ngs, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Accessibility to NGS laboratory", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors)


# ngs tech --------------------------------------

tipo_ngs <- survey[[25]] %>%
  na.omit() %>%
  strsplit(";") %>%
  unlist() 

risp25 <- c ("Illumina","Thermo Fisher","Oxford Nanopore")
tipo_ngs <- ifelse(!(tipo_ngs %in% risp25), "Other technologies", tipo_ngs)

tipo_ngs <- tipo_ngs %>% 
  table() %>%
  as.data.frame() %>%
  move2last(1)

tipo_ngs <- tipo_ngs[order(tipo_ngs$Freq, decreasing = T), ]
tipo_ngs$. <- factor(tipo_ngs$., levels = tipo_ngs$.)


# barplot tecnologie

chart_ngs_tec <- ggplot(tipo_ngs, aes(x = ., y = Freq, fill = .)) +
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
        panel.background = element_rect()) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


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

omiche$. <- str_replace(omiche$., "Proteomica", "Proteomics")
omiche$. <- str_replace(omiche$., "Metabolomica", "Metabolomics")
omiche$. <- str_replace(omiche$., "Genomica", "Genomics")
omiche <- omiche[order(omiche$Freq, decreasing = T),]

omiche$. <-  factor(omiche$., levels = omiche$.)

# barplot omiche

chart_omics <- ggplot(omiche, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.5) +
  labs(title = "'omics' characterization platform", x = "Answers", y = "N Biobanks") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -20, hjust = 0.4, vjust = 0.4),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect()) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# lab ----------------------------------------

lab_spec <- as.data.frame(table(survey[[27]]))

lab_spec$Var1 <- str_replace(lab_spec$Var1, "Si", "Yes")
lab_spec$Var1 <- str_replace(lab_spec$Var1, "Non so", "Unsure")

#frazione delle variabili nel donut
lab_spec$fraction = lab_spec$Freq / sum(lab_spec$Freq)

# top e bottom di ogni rettangolo per il donut
lab_spec$ymax = cumsum(lab_spec$fraction)
lab_spec$ymin = c(0, head(lab_spec$ymax, n=-1))

# Compute label position
lab_spec$labelPosition <- (lab_spec$ymax + lab_spec$ymin) / 2

# piechart altri lab specializzati

chart_lab <- ggplot(lab_spec, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Accesibility to other specialized laboratory", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
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

altri_lab <- c()

for (labspec in 1:length(lab$.)) {
  if(lab$.[labspec] %in% c("Pathological anatomy","Digital Pathology"))  {
    labspec <- labspec
  } else {
    altri_lab <- c(altri_lab, lab$.[labspec])
    lab$.[labspec] <- "Some other"
  }
}  

lab <- lab %>%
  group_by(lab$.) %>%
  summarize(Freq = sum(Freq))

lab <- lab[order(lab$`lab$.`, decreasing = T),]
lab$`lab$.` <- factor(lab$`lab$.`, levels = lab$`lab$.`)

# barplot altri laboratori specializzati 

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
        panel.background = element_rect()) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# data specialization ------------------------------------------

dati <- survey[[29]] %>%
  strsplit(";|,") %>%
  unlist() %>%
  as.data.frame() 

risp29 <- c("malattie rare","oncologia","metagenomica batterica","metagenomica virale")

for (a in 1:length(dati$.)) {
  
  ifelse(!(dati$.[a] %in% risp29),dati$.[a] <-"Some other", dati$.)
  
}

dati$. <- str_replace(dati$., "malattie rare", "Rare diseases")
dati$. <- str_replace(dati$., "oncologia", "Oncology")
dati$. <- str_replace(dati$., "metagenomica batterica", "Bacterial metagenomics")
dati$. <- str_replace(dati$., "metagenomica virale", "Viral metagenomics")

dati <- as.data.frame(table(dati))

#frazione delle variabili nel donut
dati$fraction = dati$Freq / sum(dati$Freq)

# top e bottom di ogni rettangolo per il donut
dati$ymax = cumsum(dati$fraction)
dati$ymin = c(0, head(dati$ymax, n=-1))

# Compute label position
dati$labelPosition <- (dati$ymax + dati$ymin) / 2

# piechart specializzazione dati BB

chart_data <- ggplot(dati, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=.)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 4.3, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 2.8, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void() +
  labs(title = "Biobanked data specialization", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers", reverse = T)) +
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


# data type -------------------------------------------

tip_dati <- survey[[30]] %>%
  strsplit(";") %>%
  unlist()

risp30 <- c("Lifestyle dataset", "Environmental dataset", "Physiological dataset", "Biochemical dataset", "Clinical dataset", "Psychological dataset", "Genomic dataset (sequences)", "Genomic dataset (variants)", "Proteomic dataset", "Metabolomic dataset", "Body (Radiological) image", "Whole slide image", "Photo image", "Genealogical records")


for (a in 1:length(tip_dati)) {
  
  ifelse(!(tip_dati[a] %in% risp30),tip_dati[a] <-"Some other", tip_dati)
  
}

tip_dati<- as.data.frame(table(tip_dati)) 

tip_dati <- tip_dati[order(tip_dati$Freq, decreasing = T),]
tip_dati <- move2last(tip_dati,9)
tip_dati$tip_dati <- factor(tip_dati$tip_dati, levels = tip_dati$tip_dati)

# barplot tipi di dati della BB

chart_data2 <- ggplot(tip_dati, aes(x = tip_dati, y = Freq, fill = tip_dati)) +
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
        plot.margin = margin(0,1,0,0, "cm")) + 
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
ont <- move2last(ont, 3) %>%
  move2last(7) %>%
  move2last(1)

ont$ont <- factor(ont$ont, levels = ont$ont)

# barplot ontologie

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
        panel.background = element_rect()
  ) + 
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))

# data subset for network -----------------------------

cond <- as.data.frame(table(survey[[32]]))

cond$Var1 <- str_replace(cond$Var1, "Sì", "Yes")

#frazione delle variabili nel donut
cond$fraction = cond$Freq / sum(cond$Freq)

# top e bottom di ogni rettangolo per il donut
cond$ymax = cumsum(cond$fraction)
cond$ymin = c(0, head(cond$ymax, n=-1))

# Compute label position
cond$labelPosition <- (cond$ymax + cond$ymin) / 2

# piechart sottoinsieme di dati presenti

chart_network <- ggplot(cond, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Are there subsets of data shared in specialized networks?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors)


# network ----------------------------

reti_cond <- survey[[33]] %>%
  na.omit() %>% 
  strsplit(";|,|; |, ") %>%
  unlist() %>%
  table() %>%
  as.data.frame( stringsAsFactors = F)

reti_cond <- reti_cond[-1,]

for (a in 1:length(reti_cond$.)) {
  
  ifelse(reti_cond$Freq[a] <= 1, reti_cond$.[a] <-"Some other", reti_cond$.)
  
}

reti_cond <-reti_cond %>%
  group_by(reti_cond$.) %>%
  summarize(Freq = sum(Freq))


#frazione delle variabili nel donut
reti_cond$fraction = reti_cond$Freq / sum(reti_cond$Freq)

# top e bottom di ogni rettangolo per il donut
reti_cond$ymax = cumsum(reti_cond$fraction)
reti_cond$ymin = c(0, head(reti_cond$ymax, n=-1))
reti_cond$labelPosition <- (reti_cond$ymax + reti_cond$ymin) / 2

reti_cond$`reti_cond$.` <- factor(reti_cond$`reti_cond$.`, levels = reti_cond$`reti_cond$.`)

# donut plot

chart_network2 <- ggplot(reti_cond, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=`reti_cond$.`)) +
  geom_rect() +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) + 
  theme_void() +
  labs(title = "Networks in which data are shared") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
  guides(fill = guide_legend(title = "Answers")) +
  scale_fill_manual(values = mycolors)

# data cross-ref ----------------------------------

cross <- as.data.frame(table(survey[[34]]))

cross$Var1 <- str_replace(cross$Var1, "Si", "Yes")
cross$Var1 <- str_replace(cross$Var1, "Non so", "Unsure")

#frazione delle variabili nel donut
cross$fraction = cross$Freq / sum(cross$Freq)

# top e bottom di ogni rettangolo per il donut
cross$ymax = cumsum(cross$fraction)
cross$ymin = c(0, head(cross$ymax, n=-1))

# Compute label position
cross$labelPosition <- (cross$ymax + cross$ymin) / 2

# piechart possibilità di incrociare dati

chart_data4 <- ggplot(cross, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  xlim(c(2, 4)) +
  geom_text(x= 3.5, aes(y=labelPosition, label = paste0("N = ", Freq, ",\n", round(fraction*100, digit = 1), "%")), size = 5, fontface = "bold") +
  coord_polar("y", start=0)+
  theme_void()+
  labs(title = "Is it possible to cross-reference data with ones on another \ninstitution's or terrritorial system?", x = "", y = "", ) +
  theme(legend.position = "right",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  ) +
  guides(fill = guide_legend(title = "Answers")) +
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

sist_cross$. <- str_replace(sist_cross$., "è possìbile incrociare i dati con il sìstema informativo clinico dell'infrastruttura a cui afferisce la biobanca", "It is possible to cross-reference data with clnical information system of the infrastructure affiliated to the Biobank")
sist_cross$. <- str_replace(sist_cross$.,"è possìbile incrociare i dati con sìstemi informativi clinici territoriali a cui è collegata l'istituzione a cui afferisce la biobanca","It is possible to cross-reference data with territorial clinical information system connected to  the Biobank affiliated institution" )
sist_cross$. <- str_replace(sist_cross$.,"è possibile incrcociare ma non in modo automatico", "it is possible but not in automated way")

sist_cross$.<- factor(sist_cross$., levels = sist_cross$.)

#barplot quali sistemi

chart_data5 <- ggplot(sist_cross, aes(x = ., y = Freq, fill = .)) +
  geom_bar(stat = "identity", width = 0.5, color = "transparent") +
  theme_minimal()+
  geom_text(aes(label = Freq),vjust = -0.1) +
  labs(title = "How can the data be cross-referenced?", x = "", y = "") +
  scale_x_discrete(labels = "", "", "") +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = -45, hjust = 0),
        axis.text.y = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect()
  ) +
  guides(fill = guide_legend(title = "Answers",)) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))


# digital informe consent -----------------------------------

cons_inf <- as.data.frame(table(survey[[36]]))

cons_inf$Var1 <- str_replace(cons_inf$Var1, "Si", "Yes")

#frazione delle variabili nel donut
cons_inf$fraction = cons_inf$Freq / sum(cons_inf$Freq)

# top e bottom di ogni rettangolo per il donut
cons_inf$ymax = cumsum(cons_inf$fraction)
cons_inf$ymin = c(0, head(cons_inf$ymax, n=-1))

# Compute label position
cons_inf$labelPosition <- (cons_inf$ymax + cons_inf$ymin) / 2

# piechart consenso inforato digitale/elettronico

chart_ic <- ggplot(cons_inf,aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
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
tipo_cons <- survey[[37]] %>%
  na.omit()

tipo_cons <- ifelse(grepl("dinamic", tipo_cons, ignore.case = T), "Dynamic", tipo_cons)
tipo_cons <- as.data.frame(table(tipo_cons))

# barplot

chart_ic_model <- ggplot(tipo_cons, aes(x = tipo_cons, y =Freq, fill = tipo_cons)) +
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
        panel.background = element_rect()
  ) +
  scale_fill_manual(values = mycolors) + 
  scale_y_continuous(expand = expansion( mult = c(0, 0.1)))





