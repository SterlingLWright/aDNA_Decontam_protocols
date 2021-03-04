##Sterling Wright ##
##May 21, 2020 ## 

###DECONTAM PAPER ### 
setwd("~/Files/")

#You will need the package gplots for the heatmap.2 function# 
####Packages for the heatmap ####
library(gplots)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(extrafont)
library(scales)
library(reshape2)

####Data ####
Decontam_P<-read.csv("Phyla-Shotgun-absolutecounts-rarefied-decontam.csv")
Decontam_G<-read.csv("Genera-Shotgun-absolutecounts-rarefied-decontam.csv")
Decontam_S<-read.csv("Species-Shotgun-absolutecounts-rarefied-decontam.csv")

#Transform the data into a data matrix
Decontam_P.df<-as.data.frame.matrix(Decontam_P)
Decontam_G.df<-as.data.frame.matrix(Decontam_G)
Decontam_S.df<-as.data.frame.matrix(Decontam_S)

Decontam_P.Long<-melt(Decontam_P.df, id.vars=c("Samples"), value.name = "Counts")
names(Decontam_P.Long)[2]<-paste("Taxon")

Decontam_G.Long<-melt(Decontam_G.df, id.vars=c("Samples"), value.name = "Counts")
names(Decontam_G.Long)[2]<-paste("Taxon")

Decontam_S.Long<-melt(Decontam_S.df, id.vars=c("Samples"), value.name = "Counts")
names(Decontam_S.Long)[2]<-paste("Taxon")

#Create the heatmap #
#tiff("Decontam_P_heatmap.tiff", units = "in", width = 12, height = 16, res = 300)
####Create a heatmap ####
ggplot(data = Decontam_P.Long, mapping = aes(x =  Samples,
                                                 y = Taxon,
                                                 fill = Counts)) +
  geom_tile() + 
  xlab(label = "Samples") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), axis.text.y = element_text(size = 6)) 
#dev.off()

##GENUS ## 
#tiff("Decontam_G_heatmap.tiff", units = "in", width = 12, height = 16, res = 300)
ggplot(data = Decontam_G.Long, mapping = aes(x =  Samples,
                                             y = Taxon,
                                             fill = Counts)) +
  geom_tile() + 
  xlab(label = "Samples") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), axis.text.y = element_text(size = 3)) 
#dev.off()

##Species ##
#tiff("Decontam_S_heatmap.tiff", units = "in", width = 12, height = 16, res = 300)
ggplot(data = Decontam_S.Long, mapping = aes(x =  Samples,
                                             y = Taxon,
                                             fill = Counts)) +
  geom_tile() + 
  xlab(label = "Samples") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), axis.text.y = element_text(size = 1)) 
#dev.off()

###Stacked Bar Plots ###
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(extrafont)
library(scales)
library(reshape2)

#Load the data into R #
#***Make sure you transpose the data (i.e. have the samples be the rownames) before loading into R #
TOP20Phyla.Decontam<-read.csv("Decont-Phyla-relativeabundance-TOP20.csv")
#Transform the data into a R.dataframe #
TOP20Phyla.Decontam.df<-as.data.frame.matrix(TOP20Phyla.Decontam)
#View(TOP20Phyla.MaltvsRefseq.df)
#TOP20Phyla.MaltvsRefseq.df$Sample<-rownames(TOP20Phyla.MaltvsRefseq.df)
#Use the melt function to create a proportion column (percentages) for each sample 
PhylaLong<-melt(TOP20Phyla.Decontam.df, id.vars=c("Samples"), value.name = "Proportion")
#View(PhylaLong)
#Rename your second column as Taxon 
names(PhylaLong)[2]<-paste("Taxon")


#a<-ifelse(PhylaLong$Samples == "AADDER", "red", "blue")

#Create a tiff image of a stacked bar plot # 
#tiff("MALTvsRefseq_Phyla_12MAY20.tiff", units = "in", width = 8, height = 16, res = 300)

##How can we use facet_wrap() to separate each one based on group # 
ggplot() + geom_bar(aes(y= Proportion, x = Samples,
                        fill = Taxon),
                    data = PhylaLong,
                    stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line("black"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

#dev.off()

TOP20Genera.Decontam<-read.csv("Decont-Genera-relativeabundance-TOP20.csv")
TOP20Genera.Decontam.df<-as.data.frame.matrix(TOP20Genera.Decontam)
GeneraLong<-melt(TOP20Genera.Decontam.df, id.vars=c("Samples"), value.name = "Proportion")
names(GeneraLong)[2]<-paste("Taxon")

ggplot() + geom_bar(aes(y= Proportion, x = Samples,
                        fill = Taxon),
                    data = GeneraLong,
                    stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line("black"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

TOP20Species.Decontam<-read.csv("Decont-Species-relativeabundance-TOP20.csv")
TOP20Species.Decontam.df<-as.data.frame.matrix(TOP20Species.Decontam)
SpeciesLong<-melt(TOP20Species.Decontam.df, id.vars=c("Samples"), value.name = "Proportion")
names(SpeciesLong)[2]<-paste("Taxon")

ggplot() + geom_bar(aes(y= Proportion, x = Samples,
                        fill = Taxon),
                    data = SpeciesLong,
                    stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line("black"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))


####Beta Diverstiy analyses #### 
library(vegan)
diversity(Decontam_G_numeric, index = "shannon")
Decontam_G_numeric<-lapply(Decontam_G.df, as.numeric)

View(Decontam_P.Long)


####Subsetting #### 
setwd("C:/Users/Sterling L. Wright/Desktop/microArch/DecontamPaper/Files/NO_EBCs/")
Decontam_NO_EBC<-read.csv("Decontam_Species_No_EBC.csv")
Decontam_NO_EBC.df<-as.data.frame.matrix(Decontam_NO_EBC)
Decontam_Group1<-subset(Decontam_NO_EBC.df, Decontam_NO_EBC.df$Samples == "Group1_No_treatment", select(2:183))
Decontam_Group1

###

setwd("C:/Users/Sterling L. Wright/Desktop/microArch/DecontamPaper/Files/NO_EBCs/")
Decontam_NO_EBC<-read.csv("Decontam_Species_No_EBC.csv")
Decontam_NO_EBC.df<-as.data.frame.matrix(Decontam_NO_EBC)

SpeciesLong<-melt(Decontam_NO_EBC.df, id.vars=c("Samples"), value.name = "Counts")
names(SpeciesLong)[2]<-paste("Taxon")

ggplot() + geom_bar(aes(y= Proportion, x = Samples,
                        fill = Taxon),
                    data = SpeciesLong,
                    stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line("black"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))


ggplot(data = SpeciesLong, mapping = aes(x =  Samples,
                                             y = Taxon,
                                             fill = Counts)) +
  geom_tile() + 
  xlab(label = "Samples") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), axis.text.y = element_text(size = 2)) 

####Presence-Absence ####
setwd("C:/Users/Sterling L. Wright/Desktop/microArch/DecontamPaper/Files/Presence-absence-test/")
Decontam_PA<-read.csv("Decontam-presence-absence-NOEBC-rarefied-table.csv")

Decontam_PA.df<-as.data.frame.matrix(Decontam_PA)

SpeciesLong<-melt(Decontam_PA.df, id.vars=c("Samples"), value.name = "Counts")
names(SpeciesLong)[2]<-paste("Taxon")

#ggplot() + geom_bar(aes(y= Counts, x = Samples,
#                        fill = Taxon),
#                    data = SpeciesLong,
#                    stat = "identity") +
#  theme(axis.text.x = element_text(angle = 70, hjust = 1), panel.border = element_blank(),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank(),
#        axis.line = element_line("black"),
#        plot.background = element_rect(fill = "white"),
#        panel.background = element_rect(fill = "white"))

####Heatmap ####
ggplot(data = SpeciesLong, mapping = aes(x =  Samples,
                                         y = Taxon,
                                         fill = Counts)) +
  geom_tile() + 
  xlab(label = "Samples") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), axis.text.y = element_text(size = 2)) 


####Sourcetracker ####
setwd("C:/Users/Sterling L. Wright/Desktop/microArch/DecontamPaper/Files/Sourcetracker/")
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(extrafont)
library(scales)

Decontam_Sourcetracker<-read.csv("Decontam_sourcetracker_results.csv")
tiff("Decontam_SourceTrackerResults_05JUL20.tiff", units = "in", width = 5, height = 4, res = 300)
ggplot() + geom_bar(aes(y = Decontam_Sourcetracker$Percentages, x = Decontam_Sourcetracker$SampleID, fill = Decontam_Sourcetracker$Environment), data = Decontam_Sourcetracker, stat = "identity") + ggtitle("Sourcetracker results") +
  labs(x = "Samples", y = "Percentage") +
  theme_bw() +
  scale_fill_manual(name = "Environment", values = c("firebrick", "lightskyblue", "thistle", "darkseagreen", "seashell4", "turquoise", "wheat"))+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
dev.off()

Decontam_SUBSAMPLED<-read.csv("Decontam_sourcetracker_SUBSAMPLED_results.csv")
Decontam_NoTreatment<-subset(Decontam_SUBSAMPLED, Decontam_SUBSAMPLED$SampleID == "No_Treatment")
Decontam_UV<-subset(Decontam_SUBSAMPLED, Decontam_SUBSAMPLED$SampleID == "UV")
Decontam_Bleach<-subset(Decontam_SUBSAMPLED, Decontam_SUBSAMPLED$SampleID == "Bleach")
Decontam_UV_Bleach<-subset(Decontam_SUBSAMPLED, Decontam_SUBSAMPLED$SampleID == "UV_&_Bleach")
Decontam_EDTA<-subset(Decontam_SUBSAMPLED, Decontam_SUBSAMPLED$SampleID == "EDTA")

NoTreatment_Calculus<-subset(Decontam_NoTreatment, Decontam_NoTreatment$Environment == "Calculus")
NoTreatment_Soil<-subset(Decontam_NoTreatment, Decontam_NoTreatment$Environment == "Soil")

UV_Bleach_Calculus<-subset(Decontam_UV_Bleach, Decontam_UV_Bleach$Environment == "Calculus")
UV_Bleach_Soil<-subset(Decontam_UV_Bleach, Decontam_UV_Bleach$Environment == "Soil")                             


####Sourcetracker results without calculus and only oral ####
setwd("C:/Users/Sterling L. Wright/Desktop/microArch/DecontamPaper/Files/Sourcetracker/No_calculus/")
Oral_Sourcetracker_results<-read.csv("Sourcetracker_no_calculus.csv")
tiff("Decontam_SourceTrackerResults_Oral_10JUL20.tiff", units = "in", width = 5, height = 4, res = 300)
ggplot() + geom_bar(aes(y = Oral_Sourcetracker_results$Percentages, x = Oral_Sourcetracker_results$SampleID, fill = Oral_Sourcetracker_results$Environment), data = Oral_Sourcetracker_results, stat = "identity") + ggtitle("Sourcetracker results") +
  labs(x = "Samples", y = "Percentage") +
  theme_bw() +
  scale_fill_manual(name = "Environment", values = c("firebrick", "lightskyblue", "thistle", "darkseagreen", "seashell4", "turquoise", "wheat"))+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
dev.off()

####Now try the same thing but this time with plaque as a source #### 
Plaque_Sourcetracker_results<-read.csv("Sourcetracker_Plaque-results.csv")
tiff("Decontam_SourceTrackerResults_Plaque_10JUL20.tiff", units = "in", width = 5, height = 4, res = 300)
ggplot() + geom_bar(aes(y = Plaque_Sourcetracker_results$Percentages, x = Plaque_Sourcetracker_results$SampleID, fill = Plaque_Sourcetracker_results$Environment), data = Plaque_Sourcetracker_results, stat = "identity") + ggtitle("Sourcetracker results") +
  labs(x = "Samples", y = "Percentage") +
  theme_bw() +
  scale_fill_manual(name = "Environment", values = c("firebrick", "lightskyblue", "thistle", "darkseagreen", "seashell4", "wheat"))+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
dev.off()


####Top 20 - Relative abundances -- taxa present in at least 59% of the samples ####
setwd("C:/Users/Sterling L. Wright/Desktop/microArch/DecontamPaper/Files/NO_EBCs/")

TOP20Species.Decontam<-read.csv("Top20-RelativeAbundance.csv")
TOP20Species.Decontam.df<-as.data.frame.matrix(TOP20Species.Decontam)
SpeciesLong<-melt(TOP20Species.Decontam.df, id.vars=c("Samples"), value.name = "Proportion")
names(SpeciesLong)[2]<-paste("Taxon")

tiff("Decontam_Species_19JUL20.tiff", units = "in", width = 16, height = 8, res = 300)
ggplot() + geom_bar(aes(y= Proportion, x = Samples,
                        fill = Taxon),
                    data = SpeciesLong,
                    stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, hjust = 1), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line("black"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))
dev.off()

# Section One ---------------------------------

# Section Two =================================

### Section Three ############################# 

Sys.getenv("PATH")
Sys.which("stats.dll")

writeLines('PATH="${RTOOLS40_HOME}C:/Program Files/R/R-3.6.2/library/"', con = "~/.Renviron")
