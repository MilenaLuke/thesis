library(rglobi)
library(dplyr)
library(taxonomizr)
library(data.table)

foxdata <- read.csv(file="ForMilena.csv")

# Doppelte + NA + unidentified/uncultured entfernen
unique_foxdata <- unique(foxdata)
fdata <- unique_foxdata[!is.na(unique_foxdata$species),]
fd <- fdata %>% filter(!grepl('unidentified|uncultured', species))

# Subset erstellen: nur Eukaryota 
fd_euk <- subset(fd,fd$superkingdom == "Eukaryota")

# Beispieldatensatz mit 5 Reihen erstellen
test1 <- fd_euk[1:5,]

# Für jeden Eukaryoten Interaktionen finden (man erhält 1 Liste pro Eukaryot)
inter_list <- list("parasiteOf","pathogenOf","endoparasiteOf","ectoparasiteOf")
lapplyfunction <- function(x) {
  get_interactions_by_taxa(sourcetaxon = x, interactiontype = inter_list)}
a <- lapply(test1[,6],lapplyfunction)

b <- a[unlist(lapply(a,nrow)) > 0]

#Doppelte Interaktionen entfernen und aus Liste Dataframe machen
c <- data.frame(lapply(b[],unique))

# NCBI ID hinter jedes target_taxon schreiben
c$NCBI_ID <- unlist(lapply(c$target_taxon_name,getId))

# entsprechenden target_taxon_path einfügen
fd2 <- cbind.data.frame(c,(data.frame(getTaxonomy(c$NCBI_ID))))

fd3 <- fd2[!is.na(fd2$NCBI_ID),]

# Finden, wo letzte Übereinstimmung war (zB wo alle gleiche Familie haben)

category <- if ((nrow(data.frame(table(fd3$species))))==1) {
  print("Species")
} else if ((nrow(data.frame(table(fd3$genus))))==1){
  print("Genus") 
} else if ((nrow(data.frame(table(fd3$family))))==1){
  print("Family") 
} else if ((nrow(data.frame(table(fd3$order))))==1){
  print("Order") 
} else if ((nrow(data.frame(table(fd3$class))))==1){
  print("Class") 
} else if ((nrow(data.frame(table(fd3$phylum))))==1){
  print("Phylum") 
} else if ((nrow(data.frame(table(fd3$superkingdom))))==1){
  print("Superkingdom") 
} else {
  print("Mistake")
}

# Kategorie an fd3 Datensatz anfügen ("Class" - oder sollte das "Mammalia" sein?)
fd3$LCA <- category




# -------------------
## ausweiten auf 10 Reihen
# Beispieldatensatz mit 10 Reihen erstellen
test2 <- fd_euk[1:10,]

# Für jeden Eukaryoten Interaktionen finden (man erhält 1 Liste pro Eukaryot)
a2 <- lapply(test2[,6],lapplyfunction)

b2 <- a2[unlist(lapply(a2,nrow)) > 0]

#Doppelte Interaktionen entfernen und aus Liste Dataframe machen
c2 <- lapply(b2,unique)

d2 <- data.frame(c2[4])
d2$NCBI_ID <- unlist(lapply(d2$target_taxon_name,getId))
v2 <- data.frame(getTaxonomy(d2$NCBI_ID))
fd4 <- cbind.data.frame(d2,v2)
fd5 <- fd4[!is.na(fd4$NCBI_ID),]

category2 <- if ((nrow(data.frame(table(fd5$species))))==1) {
  print("Species")
} else if ((nrow(data.frame(table(fd5$genus))))==1){
  print("Genus") 
} else if ((nrow(data.frame(table(fd5$family))))==1){
  print("Family") 
} else if ((nrow(data.frame(table(fd5$order))))==1){
  print("Order") 
} else if ((nrow(data.frame(table(fd5$class))))==1){
  print("Class") 
} else if ((nrow(data.frame(table(fd5$phylum))))==1){
  print("Phylum") 
} else if ((nrow(data.frame(table(fd5$superkingdom))))==1){
  print("Superkingdom") 
} else {
  print("Mistake")
}

fd5$LCA <- category2

#-----
# so können Unterlisten nach Anwendung von lapply zu einem großen Dataframe
# zusammengefügt werden
require(data.table)
e2 <- setDF(rbindlist(lapply(b2,unique)))


nrow(c2[[4]])


