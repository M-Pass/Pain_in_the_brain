#### WORKSPACE SETTING ####

rm(list=ls())

library(dplyr)
library(diceR)
library(effectsize)
library(ggplot2)
library(ggthemes)
library(gplots)
library(mclust)
library(mice)
library(patchwork)
library(pheatmap)
library(psych)
library(tidyr)
library(xlsx)


#### FIBROMYALGIA DATA LOADING & SCORING ####

d <- read.xlsx("Data/Data_Fibro.xlsx", 1)

# Selecting only women
d <- d[!is.na(d$Sesso) & d$Sesso == 0,]

# Computing age
d[,5] <- 2021-as.numeric(d[,5])
colnames(d)[5] <- "Age"

# Attributing colnames
colnames(d)[8:16] <- c("TitoloStudio", "Occupazione", "Professione", "Medici_anno", "Anni_diagnosi", "Psicoterapeuta_Disorder", 
                                 "Psicoterapeuta_Generico", "Farmaci_Binario", "Farmaci_Numero")

# TAQ #

# Reverse scoring
d[,c(17, 22, 23, 24, 26, 29, 32, 33, 34, 35, 39, 41, 42, 45, 47, 48, 52)] <- 6-d[,c(17, 22, 23, 24, 26, 29, 32, 33, 34, 35, 39, 41, 42, 45, 47, 48, 52)]

d$TAQ_Partner <- apply(d[,c(17:21, 23:25, 27, 30)], 1, mean, na.rm=T)
d$TAQ_Family <- apply(d[,32:37], 1, mean, na.rm=T)
d$TAQ_SameSex <- apply(d[,c(38:43)], 1, mean, na.rm=T)
d$TAQ_OppSex <- apply(d[,c(44:49)], 1, mean, na.rm=T)
d$TAQ_Stranger <- apply(d[,c(50, 51, 53)], 1, mean, na.rm=T)

# STAI #

# Reverse scoring
d[,c(54, 56, 59, 60, 63, 66, 67, 69, 72)] <- 5-d[,c(54, 56, 59, 60, 63, 66, 67, 69, 72)]

d$STAI <- apply(d[,54:73], 1, sum)

# BDI #

# Score is indicated in the first character of each response option
for (col in 74:94) {
    d[,col] <- as.numeric(substr(d[,col], 1,1))
}

d$BDI <- apply(d[,74:94], 1, sum)

# IPQ #

d$IPQ <- apply(d[,c(95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115, 117, 119, 121, 123, 125)], 1, function(x){sum(x %in% c(1, "vero"))})

# FSCRS #

d$Autocritica_Odio <- apply(d[,c(138, 139, 141, 144, 151)], 1, mean, na.rm=T)
d$Autocritica_Inadeguatezza <- apply(d[,c(130, 131, 133, 135, 136, 143, 146, 147, 149)], 1, mean, na.rm=T)
d$Autocritica_Gentilezza <- apply(d[,c(132, 134, 137, 140, 142, 145, 148, 150)], 1, mean, na.rm=T)

# CPAQ #

# reverse scoring
d[,c(155, 158, 162, 164, 165, 167, 168, 169, 171)] <- 6-d[,c(155, 158, 162, 164, 165, 167, 168, 169, 171)]

d$CPAQ_PW <- apply(d[,c(155, 158, 162, 164, 165, 167, 168, 169, 171)], 1, sum)
d$CPAQ_AE <- apply(d[,c(152, 153, 154, 156, 157, 159, 160, 161, 163, 166, 170)], 1, sum)

# FIQ #

# converting 'days of the week' questions on a 1-10 scale
d[,182] <- 10-d[,182]/7*10
d[,183] <- d[,183]/7*10

# rescaling items on physical functioning on a 1-10 scale
d[,172:181] <- d[,172:181]/3*10

d$FIQ_Phys <- apply(d[,172:181], 1, mean)
d$FIQ_TOT <- d$FIQ_Phys + d[,182] + d[,183] +apply(d[,184:190], 1, sum)

# DPES #

d$DPES_Happiness <- apply(d[,191:201], 1, mean, na.rm=T)
d$DPES_Pride <- apply(d[,202:206], 1, mean, na.rm=T)
d$DPES_Love <- apply(d[,207:212], 1, mean, na.rm=T)
d$DPES_Compass <- apply(d[,213:217], 1, mean, na.rm=T)
d$DPES_Amus <- apply(d[,218:222], 1, mean, na.rm=T)
d$DPES_Awe <- apply(d[,223:227], 1, mean, na.rm=T)

# Selecting only scale scores (removing single items and unused variables from the dataset)
data_fibro <- d[,c(5,228:248)]

rm(d, col)

#### CHRONIC PAIN DATA LOADING & SCORING ####

data_pain <- read.xlsx("Data/DoloreCronico.xlsx", 1)
colnames(data_pain)[5:13] <- c("TitoloStudio", "Occupazione", "Professione", "Medici_anno", "Anni_diagnosi", "Psicoterapeuta_Disorder", 
                          "Psicoterapeuta_Generico", "Farmaci_Binario", "Farmaci_Numero")

# Computing age
data_pain[,2] <- 2024 - as.numeric(data_pain[,2])
colnames(data_pain)[2] <- "Age"

data_pain$Sesso <- data_pain$Sesso == "Maschio"


# TAQ #

# Reverse scoring
data_pain[,c(14, 19, 20, 21, 23, 26, 29, 30, 31, 32, 36, 38, 39, 42, 44, 45, 49)] <- 6-data_pain[,c(14, 19, 20, 21, 23, 26, 29, 30, 31, 32, 36, 38, 39, 42, 44, 45, 49)]

data_pain$TAQ_Partner <- apply(data_pain[,c(14:18, 20:22, 24, 27)], 1, mean, na.rm=T)
data_pain$TAQ_Family <- apply(data_pain[,29:34], 1, mean, na.rm=T)
data_pain$TAQ_SameSex <- apply(data_pain[,c(35:40)], 1, mean, na.rm=T)
data_pain$TAQ_OppSex <- apply(data_pain[,c(41:46)], 1, mean, na.rm=T)
data_pain$TAQ_Stranger <- apply(data_pain[,c(47, 48, 50)], 1, mean, na.rm=T)


# STAI #

# Reverse scoring
data_pain[,c(51, 53, 56, 57, 60, 63, 64, 66, 69)] <- 5-data_pain[,c(51, 53, 56, 57, 60, 63, 64, 66, 69)]

data_pain$STAI <- apply(data_pain[,51:70], 1, sum)

# BDI #

# In this dataset it was already computed
data_pain$BDI <- apply(data_pain[,71:91], 1, sum)

# IPQ #

data_pain$IPQ <- apply(data_pain[,c(92, 94, 96, 98, 100, 102, 104, 106, 108, 110, 112, 114, 116, 118, 120, 122)], 1, function(x){sum(x == "vero")})

# FSCRS #

data_pain$Autocritica_Odio <- apply(data_pain[,c(132, 133, 135, 138, 145)], 1, mean, na.rm=T)
data_pain$Autocritica_Inadeguatezza <- apply(data_pain[,c(124, 125, 127, 129, 130, 137, 140, 141, 143)], 1, mean, na.rm=T)
data_pain$Autocritica_Gentilezza <- apply(data_pain[,c(126, 128, 131, 134, 136, 139, 142, 144)], 1, mean, na.rm=T)

# CPAQ #

# reverse scoring
data_pain[,c(149, 152, 156, 158, 159, 161, 162, 163, 165)] <- 6-data_pain[,c(149, 152, 156, 158, 159, 161, 162, 163, 165)]

data_pain$CPAQ_PW <- apply(data_pain[,c(149, 152, 156, 158, 159, 161, 162, 163, 165)], 1, sum)
data_pain$CPAQ_AE <- apply(data_pain[,c(146, 147, 148, 150, 151, 153, 154, 155, 157, 160, 164)], 1, sum)

# FIQ #

# converting 'days of the week' questions on a 1-10 scale
data_pain[,176] <- 10-data_pain[,176]/7*10
data_pain[,177] <- data_pain[,177]/7*10

# rescaling items on physical functioning on a 1-10 scale
data_pain[,166:175] <- data_pain[,166:175]/3*10

data_pain$FIQ_Phys <- apply(data_pain[,166:175], 1, mean, na.rm=T)
data_pain$FIQ_TOT <- data_pain$FIQ_Phys + apply(data_pain[,176:184], 1, sum)

# DPES #

data_pain$DPES_Happiness <- apply(data_pain[,185:195], 1, mean, na.rm=T)
data_pain$DPES_Pride <- apply(data_pain[,196:200], 1, mean, na.rm=T)
data_pain$DPES_Love <- apply(data_pain[,201:206], 1, mean, na.rm=T)
data_pain$DPES_Compass <- apply(data_pain[,207:211], 1, mean, na.rm=T)
data_pain$DPES_Amus <- apply(data_pain[,212:216], 1, mean, na.rm=T)
data_pain$DPES_Awe <- apply(data_pain[,217:221], 1, mean, na.rm=T)

# Selecting only scale scores (removing single items and unused variables from the dataset)
data_pain <- data_pain[,c(2, 223:243)]

#### MERGING THE TWO DATASETS ####

data_fibro$Disorder <- rep("Fibromyalgia", nrow(data_fibro))
data_pain$Disorder <- rep("Chronic Pain", nrow(data_pain))

data <- base::rbind(data_fibro, data_pain)

data$Disorder <- factor(data$Disorder)

rm(data_fibro, data_pain)

#### COMPARING THE PROFILES OF THE TWO SAMPLES ####

# COMPARISONS FOR EACH SUBSCALE #

describe(data[data$Disorder == "Fibromyalgia", 1:22])
describe(data[data$Disorder == "Chronic Pain", 1:22])


multivariate_model <- lm(as.matrix(data[,2:22])~ Disorder + Age, data=data)

summary(multivariate_model)

eta_squared(multivariate_model, partial = TRUE)

# Rescaling scale scores to range 0-1 for better readability on the graph
data_rescaled <- data

data_rescaled$TAQ_Partner <- (data_rescaled$TAQ_Partner-1)/4
data_rescaled$TAQ_Family <- (data_rescaled$TAQ_Family-1)/4
data_rescaled$TAQ_SameSex <- (data_rescaled$TAQ_SameSex-1)/4
data_rescaled$TAQ_OppSex <- (data_rescaled$TAQ_OppSex-1)/4
data_rescaled$TAQ_Stranger <- (data_rescaled$TAQ_Stranger-1)/4
data_rescaled$STAI <- (data_rescaled$STAI-20)/60
data_rescaled$BDI <- (data_rescaled$BDI)/63
data_rescaled$IPQ <- (data_rescaled$IPQ)/16
data_rescaled$Autocritica_Odio <- (data_rescaled$Autocritica_Odio-1)/4
data_rescaled$Autocritica_Inadeguatezza <- (data_rescaled$Autocritica_Inadeguatezza-1)/4
data_rescaled$Autocritica_Gentilezza <- (data_rescaled$Autocritica_Gentilezza-1)/4
data_rescaled$CPAQ_PW <- (data_rescaled$CPAQ_PW)/54
data_rescaled$CPAQ_AE <- (data_rescaled$CPAQ_AE)/66
data_rescaled$FIQ_Phys <- (data_rescaled$FIQ_Phys)/10
data_rescaled$FIQ_TOT <- (data_rescaled$FIQ_TOT)/100
data_rescaled$DPES_Happiness <- (data_rescaled$DPES_Happiness-1)/6
data_rescaled$DPES_Pride <- (data_rescaled$DPES_Pride-1)/6
data_rescaled$DPES_Love <- (data_rescaled$DPES_Love-1)/6
data_rescaled$DPES_Compass <- (data_rescaled$DPES_Compass-1)/6
data_rescaled$DPES_Amus <- (data_rescaled$DPES_Amus-1)/6
data_rescaled$DPES_Awe <- (data_rescaled$DPES_Awe-1)/6

# Reshape the data to long format
long_data <- data_rescaled %>%
    pivot_longer(cols = 2:22, names_to = "variable", values_to = "value")

# Summarize the data
summary_data <- long_data %>%
    group_by(Disorder, variable) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              se = sd(value, na.rm = TRUE) / sqrt(n()))

# Ordering the factor to plot the variables in non-alphabetical order
summary_data$variable <- factor(summary_data$variable, levels = rev(c(
    "TAQ_Partner", "TAQ_Family", "TAQ_SameSex", "TAQ_OppSex", "TAQ_Stranger",
    "STAI", "BDI", "IPQ", "Autocritica_Odio", "Autocritica_Inadeguatezza", "Autocritica_Gentilezza",
    "CPAQ_PW", "CPAQ_AE", "FIQ_Phys", "FIQ_TOT", "DPES_Happiness", "DPES_Pride", "DPES_Love",
    "DPES_Compass", "DPES_Amus", "DPES_Awe"
)))

# Ensure long_data has the same factor levels as summary_data
long_data <- long_data %>%
    mutate(variable = factor(variable, levels = levels(summary_data$variable)))

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Line plot of means with confidence intervals
p1 <- ggplot(summary_data, aes(x = variable, y = mean, color = Disorder, group = Disorder)) +
    geom_line(size = 1.2) +  
    geom_point(size = 3, shape = 21, fill = "white") +  
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, size = 0.8) +  
    theme_minimal(base_size = 14, base_family = "Arial") + 
    labs(
        title = "Comparison of Means Between Groups",
        x = "Variables",
        y = "Mean Score"
    ) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 10)),  
        plot.title = element_text(hjust = 0.5, face = "bold"),  
        legend.position = "none",           # Remove legend here
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
    ) +
    ylim(c(0, 1)) +
    coord_flip() +
    scale_color_manual(values = cbPalette) +
    scale_x_discrete(labels = c(
        TAQ_Partner = "TAQ Partner",
        TAQ_Family = "TAQ Family",
        TAQ_SameSex = "TAQ Same Sex",
        TAQ_OppSex = "TAQ Opposite Sex",
        TAQ_Stranger = "TAQ Stranger",
        STAI = "STAI Y-2",
        BDI = "BDI",
        IPQ = "IPQ",
        Autocritica_Odio = "FSCRS Hated-Self",
        Autocritica_Inadeguatezza = "FSCRS Inadequate-Self",
        Autocritica_Gentilezza = "FSCRS Reassured-Self",
        CPAQ_PW = "CPAQ PW",
        CPAQ_AE = "CPAQ AE",
        FIQ_Phys = "FIQ Physical Functioning",
        FIQ_TOT = "FIQ Total Score",
        DPES_Happiness = "DPES Happiness",
        DPES_Pride = "DPES Pride",
        DPES_Love = "DPES Love",
        DPES_Compass = "DPES Compassion",
        DPES_Amus = "DPES Amusement",
        DPES_Awe = "DPES Awe"
    ))

# Boxplots of scores
p2 <- ggplot(long_data, aes(x = variable, y = value, fill = Disorder)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    theme_minimal(base_size = 14, base_family = "Arial") +
    labs(
        title = "Distribution by Variable and Group",
        x = "Variables",
        y = NULL  # Remove y-axis label
    ) +
    coord_flip() +
    scale_fill_manual(values = cbPalette) +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis labels
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
    )

# Combine the plots side by side
combined_plot <- p1 + p2 + plot_layout(ncol = 2)
combined_plot

#### CONTROLLING FOR AGE ####
data_Agecorrected <- complete(mice(data), 1)

for (scale in 2:22) {
  data_Agecorrected[,scale] <- lm(data_Agecorrected[,scale]  ~ data_Agecorrected$Age)$residuals
}


#### COMPARING CORRELATIONS ####

#creation of the two correlation matrices using age corrected data
corf <- cor(data_Agecorrected[data_Agecorrected$Disorder == "Fibromyalgia",2:22], use="pair")
corp <- cor(data_Agecorrected[data_Agecorrected$Disorder == "Chronic Pain",2:22], use="pair")

#jennrich test
cortest.jennrich(corf,
                 corp, 
                 sum(data$Disorder == "Fibromyalgia"), 
                 sum(data$Disorder == "Chronic Pain"))

# HEATMAP OF COMPARISONS

#Rounding correlations
corf <- corf %>% round(2)
corp <- corp %>% round(2)

#Creation of matrix of absolute differences
diffMat <- (corf - corp) %>% 
  abs

#Setting row and col names
rownames(diffMat) <- c("TAQ Partner",
                       "TAQ Family",
                       "TAQ Same Sex",
                       "TAQ Opposite Sex",
                       "TAQ Stranger",
                       "STAI Y-2",
                       "BDI",
                       "IPQ",
                       "FSCRS Hated-Self",
                       "FSCRS Inadequate-Self",
                       "FSCRS Reassured-Self",
                       "CPAQ PW",
                       "CPAQ AE",
                       "FIQ Physical Functioning",
                       "FIQ Total Score",
                       "DPES Happiness",
                       "DPES Pride",
                       "DPES Love",
                       "DPES Compassion",
                       "DPES Amusement",
                       "DPES Awe"
)

colnames(diffMat) <- c("TAQ Partner",
                       "TAQ Family",
                       "TAQ Same Sex",
                       "TAQ Opposite Sex",
                       "TAQ Stranger",
                       "STAI Y-2",
                       "BDI",
                       "IPQ",
                       "FSCRS Hated-Self",
                       "FSCRS Inadequate-Self",
                       "FSCRS Reassured-Self",
                       "CPAQ PW",
                       "CPAQ AE",
                       "FIQ Physical Functioning",
                       "FIQ Total Score",
                       "DPES Happiness",
                       "DPES Pride",
                       "DPES Love",
                       "DPES Compassion",
                       "DPES Amusement",
                       "DPES Awe"
)

# Creation of cellnotes matrix
cellNotes <- matrix(NA,nrow=21,ncol=21)

## Lower triangle 
lowTri <- paste(corf,"/",corp,sep="") %>%
  gsub("0.",".",.,fixed=T) %>%
  matrix(ncol=21,nrow=21)

cellNotes[lower.tri(cellNotes)] <- lowTri[lower.tri(cellNotes)]
  

## Upper triangle
uppTri <- (corf - corp) %>%
  abs %>%
  round(2) %>%
  gsub("0.",".",.,fixed=T) %>%
  matrix(ncol=21,nrow=21)

cellNotes[upper.tri(cellNotes)] <- uppTri[upper.tri(cellNotes)]

## Diagonal
diag(cellNotes) <- "-"

#Creating heatmap
heatmap.2(diffMat,
          Rowv = F,
          Colv = F,
          dendrogram="none",
          symm=T,
          scale="none",
          revC=F,
          cellnote=(cellNotes),
          trace = "none",
          notecol = "black",
          key=F,
          keysize=0.2,
          margins = c(12,12)
)




#### CLUSTERING ####

dta <- data[,2:22]

# Reshape data long
long_df <- pivot_longer(dta, cols = colnames(dta), 
                        names_to = "variable", 
                        values_to = "value")

data_Agecorrected <- complete(mice(data), 1)

for (scale in 2:22)
{
    data_Agecorrected[,scale] <- lm(data_Agecorrected[,scale]  ~ data_Agecorrected$Age)$residuals
}

data_Agecorrected <- data_Agecorrected[,2:22]

# Consensus clustering -- may take a long time to run
set.seed(108)

CC_diceR <- dice(data_Agecorrected,
                 nk = 2, # 2 clusters
                 reps = 250, # 250 BCs
                 p.item=0.8, # % of subsamples
                 # algorithms = c("diana", "ap", "block", "som", "cmeans", "km", "pam", "hc", "hdbscan", "gmm", "nmf", "sc"), 
                 algorithms = c("km", "pam", "hc", "hdbscan", "gmm", "nmf", "sc"), 
                 hc.method = "ward.D2", # use ward linkage method for hierarchical Clustering 
                 distance="euclidean",
                 cons.funs = c("LCE"), # one consensus function
                 k.method="all", # keep all consensus results for different k
                 plot=TRUE, # plot consensus matrix,
                 seed= 108, 
                 reweigh=TRUE,
                 prep.data="none"
                 
)


# checking how well the two clusters align with disorder
table(CC_diceR$clusters, data$Disorder)
adjustedRandIndex(CC_diceR$clusters, data$Disorder)

colfunc <- colorRampPalette(c("#E2E2E2", "#419AB0"))
colours<-colfunc(4)

graph_cdf(CC_diceR$E) +
    theme(legend.position = "bottom") +
    facet_grid( ~ Method,labeller = as_labeller(labels)) +
    theme_few() + 
    scale_colour_manual(values=colours)


graph_delta_area(CC_diceR$E) +
    theme_few() + 
    facet_grid( ~ Method,labeller = as_labeller(labels)) 

# Consensus matrix graph with disorder beside the graph
consensus_mat <- consensus_matrix(CC_diceR$E[,,"PAM_Euclidean",])
colnames(consensus_mat) <- paste0("C",1:ncol(consensus_mat))

annotations <- data.frame(Disorder = data$Disorder)
rownames(annotations) <- colnames(consensus_mat)

annotations_colors <- list(
    Disorder = c(
        "Fibromyalgia" = "#E18D96", 
        "Chronic Pain" = "#F2C57C"
    )
)

pheatmap(consensus_mat,
         legend=T,
         color = colorRampPalette(c("white", "#6A9FB5"))(100),
         annotation_col= annotations,
         annotation_colors = annotations_colors,
         treeheight_row = 0,
         treeheight_col = 0,
         border_color = NA,
         show_colnames = F,
         main = "Consensus matrix"
)
