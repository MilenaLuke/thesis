library(rglobi)
library(dplyr)

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

# Leere Listen (= keine Interaktionen gefunden) entfernen
c <- a
for (i in 1:5) {
  b <- nrow(c[[i]])
  if (b == 0)
    next
  i+1
  p <- c[[i]]
}
z <- unique(p)

# Listen nach Target taxon (z.B. vulpes) filtern
z_new <- z[grepl("Mus",z$target_taxon_name, ignore.case = TRUE),] 

# Fuchsdatensatz mit gefundenen Interaktionen mergen
fd_plusinteraktionen <- merge(fd,z_new,by.x="species",by.y="source_taxon_path",
                              all.x= TRUE)



##----- Test ausweiten auf 10 Arten ------- FUNKTIONIERT NICHT
test2 <- fd_euk[1:10,]

# Für jeden Eukaryoten Interaktionen finden (man erhält 1 Liste pro Eukaryot)
a10 <- lapply(test2[,7],lapplyfunction)

# Leere Listen (= keine Interaktionen gefunden) entfernen
# Problem: man bekommt mehr als eine Unterliste
# ich kann nicht mehr auf Unterlisten zugreifen, da jetzt dataframe
# "unique" funktioniert nicht mehr, wird nur bei letzter Unterliste angewandt
c10 <- a10

x <- for (i in 1:10) {
  b <- nrow(a10[[i]])
  if (b == 0)
    next
  i+1
  m <- a10[[i]]
}

z10 <- unique(m)


#------ anderer Ansatz
test500 <- fd_euk[1:500,]

# Für jeden Eukaryoten Interaktionen finden (man erhält 1 Liste pro Eukaryot)
inter_list <- list("parasiteOf","pathogenOf","endoparasiteOf","ectoparasiteOf")
lapplyfunction2 <- function(x) {
  get_interactions_by_taxa(sourcetaxon = x, interactiontype = inter_list,
                           targettaxon = "Vulpes vulpes")}
n500 <- lapply(test500[,7],lapplyfunction2)

vulpes500 <- unique(n500)

# auf einzelne Unterlisten zugreifen
q2 <- unique(vulpes500[[2]])
q3 <- unique(vulpes500[[3]])
q4 <- unique(vulpes500[[4]])
q5 <- unique(vulpes500[[5]])
q6 <- unique(vulpes500[[6]])
q7 <- unique(vulpes500[[7]])

# weitere Probleme: ein Source taxon (zB Alaria Alata) hat zwei Interaktionstypen
# mit unterschiedlichen target taxon Bezeichnungen (vulpes und vulpes  vulpes)
# wie kann das am Ende gemerged werden?



# -------- sonstiges
# Anzahl von Reihen in Unterlisten herausfinden 
x <- for (i in 1:500) {
  print(nrow(vulpes500[[i]]))
  i+1
}
# bei leeren Listen keine Anzahl ausgeben
x <- for (i in 1:500) {
  b <- nrow(n500[[i]])
  if (b == 0)
    next
  i+1
  print(nrow(n500[[i]]))
}
