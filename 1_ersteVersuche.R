## --> auflisten/darstellen wie die Daten über die großen taxonomischen Gruppen verteilt sind

## Allgemeine Probleme vorab lösen
install.packages("rglobi") # und rread installieren (sonst funktionieren Funktionen von rglobi scheinbar nicht)
library(rglobi)

## library wechseln zu rglobi
.libPaths("C:/Users/Milena/Documents/R/win-library/4.1/rglobi")

## Datensatz einlesen und als "foxdata" speichern
foxdata <-read.csv(file="ForMilena.csv")

## macht neuen Dataframe und löscht "NA" bei Species raus -> evtl auch bei anderen NA rauslöschen?
validfoxdata<-foxdata[!is.na(foxdata$species),]

## Dataframe nur mit "NA" bei species
notvalidfoxdata<-foxdata[is.na(foxdata$species),]

nrow(validfoxdata) # Anzahl ohne "NA" in species
nrow(foxdata) #(19287) # Anzahl mit "NA" in species
nrow(notvalidfoxdata) #(7743) # Anzahl nur "NA" 

## zeigt Anzahl pro Klasse/Superkingdom bei gültigen Daten an
nb_validfoxclass<-sapply(split(validfoxdata,validfoxdata$class),
                         function(x)nrow(x))
nb_validfoxking<-sapply(split(validfoxdata,validfoxdata$superkingdom),
                        function(x)nrow(x))
table(foxdata$class)

## Barplot Aufteilung nach Superkingdom
barplot(nb_validfoxking,main="Counts per Superkingdom",xlab="Superkingdom",ylab="Counts",ylim=c(0,8000))

## Barplot Aufteilung nach Klassen --> Problem: es werden nicht alle Namen der Klassen auf x-Achse angezeigt, zu viele?
barplot(nb_validfoxclass,main="Counts per Class",xlab="Class",ylab="Counts",ylim=c(0,1600))
t <-table(foxdata$class)
t[order(t)]
sort(t,decreasing=TRUE)[1:5]


## macht aus integer einen dataframe --> Vorteil: sieht in Tabelle irgendwie übersichtlicher aus
data.frame(nb_validfoxclass)

## ------> Klassen "Clostridia" und "Conoidasida" kommen am häufigsten vor

---------------------------------------
  
  ## --> rglobi nutzen um Informationen über die Arten zu sammeln: Für welche Arten funktioniert das, für welche nicht?
  
  ## CLOSTRIDIA
  
  ## zeigt alle Interaktionen von Clostridia 
  clostridia_interaction <-get_interactions_by_taxa(sourcetaxon="clostridia")

## -> man könnte "targettaxon" definieren, mit "vulpes" funktioniert es aber nicht, deswegen Canidae

## zeigt alle Interaktion von Clostridia mit Canidae
inter_clostridia_canidae <-get_interactions_by_taxa(sourcetaxon="clostridia",targettaxon = "Canidae")

## gibt dataframe mit welcher Art Clostridia interagiert
nb_clostridia_targettaxon<-sapply(split(inter_clostridia_canidae,inter_clostridia_canidae$target_taxon_name),function(x)nrow(x))
data.frame(nb_clostridia_targettaxon)
table(inter_clostridia_canidae$target_taxon_name,inter_clostridia_canidae$interaction_type)

## CONOIDASIDA

## zeigt Interaktionen von Conoidasida mit Canidae
inter_Conoidasida_canidae <-get_interactions_by_taxa(sourcetaxon="Conoidasida",targettaxon = "Canidae")

## gibt dataframe mit welcher Art Conoidasida interagiert
nb_Conoidasida_targettaxon<-sapply(split(inter_Conoidasida_canidae,inter_Conoidasida_canidae$target_taxon_name),function(x)nrow(x))
data.frame(nb_Conoidasida_targettaxon)

## VULPES

## zeigt alle "hasParasite" Interaktionen von Vulpes 
vulpes_parasites_interaction <- get_interactions_by_taxa(sourcetaxon="Vulpes",interactiontype = "hasParasite")

## Dataframe mit Vulpes "hasParasites" Interaktionen, Anzahl je Parasit --> Problem: wie komme ich an übergeordnete Taxa? 
## Ganzer Taxon path ist eine Zelle, wie greife ich auf ein Element davon zu?
## sieht zB so aus "Animalia | Arthropoda | Insecta | Siphonaptera | Pulicoidea | Pulicidae | Pulex | Pulex simulans"
nb_vulpes_parasites_interaction <- sapply(split(vulpes_parasites_interaction,vulpes_parasites_interaction$target_taxon_name),function(x)nrow(x))
data.frame(nb_vulpes_parasites_interaction)

## zeigt alle "eats" Interaktionen von Vulpes
vulpes_eats_inter <- get_interactions_by_taxa(sourcetaxon="Vulpes",interactiontype = "eats")

## Dataframe mit Vulpes "eats" Interaktionen, Anzahl pro target taxon --> Ergebnis fragwürdig...
## auch hier gleiches Problem, ich komme nicht an die übergeordneten Taxa
nb_vulpes_eats_inter <- sapply(split(vulpes_eats_inter ,vulpes_eats_inter$target_taxon_name),function(x)nrow(x))
data.frame(nb_vulpes_eats_inter)

## --> Problem: Vulpes Parasiten als Species, Conoidasida und Clostridia sind aber Klassen 
## --> auch wenn ich zwei Dataframes mit Artennamen hätte, wie vergleiche ich 2 Dataframes?

# ------------------------------------
library(rglobi)
library(stringi)
foxdata <-read.csv(file="ForMilena.csv")

# "NA" bei Species löschen
validfoxdata<-foxdata[!is.na(foxdata$species),]
# doppelte aus validfoxdata (Daten ohne NA in species) entfernen
# man erhält aus ursprünglichen 11844 Reihen nur noch 2771
df1 <-unique(validfoxdata)

# Interaktionen von Füchsen aus globi finden - 1024 Interaktionen
vulpes_interactions<-get_interactions_by_taxa(sourcetaxon = "Vulpes")
# Doppelte Interaktionen entfernen
u_vulpes_inter<-unique(vulpes_interactions) # 551 Interaktionen
#nur folgende Vulpes Interaktionen finden: hasEctoparasite,hasParasite,hasPathogen,hostOf
geordnet<-u_vulpes_inter[order(u_vulpes_inter$interaction_type),]
geordnet_ohneeats<-geordnet[460:551,] #Interaktion "eats" entfernt
#Interaktionen "kill" und "interacts with" entfernen
geordnet_ohnekilleduinteracts<-geordnet_ohneeats[order(geordnet_ohneeats$interaction_type),]
relevant_v_inter <-geordnet_ohnekilleduinteracts[1:71,] #nur 71 Interaktionen
# Dataframe 2 (df2) enthält nur relevante Interaktionen von Vulpes
df2 <- relevant_v_inter


# 2. Versuch: Canidae Interaktionen
# relevante Interaktionen von Canidae finden und in df3 packen
c<-get_interactions_by_taxa(sourcetaxon = "Canidae")
orderc<-c[order(c$interaction_type),]
ohneeats<-orderc[337:1024,]
c_new<-ohneeats[1:672,]
df3<-c_new[1:604,]
df3<-unique(df3)

# Df1 und df2 zusammenführen - Interaktionen Vulpes + Fuchsdatensatz
new_df<-merge(df1,df2,by.x="species",by.y="target_taxon_name")

# Df1 und df3 zusammenführen - Interaktionen Canidae + Fuchsdatensatz
new_df2<-merge(df1,df3,by.x="species",by.y="target_taxon_name")

#fügt Interaktionen(Canidae,Vulpes) + Fuchsdaten zusammen
x<-unique(rbind(new_df,new_df2))

# ----
# Was könnte Nahrung des Fuchses sein?
library(rglobi)
foxdata <-read.csv(file="ForMilena.csv")

# "NA" bei Species löschen
validfoxdata<-foxdata[!is.na(foxdata$species),]
# doppelte aus validfoxdata (Daten ohne NA in species) entfernen
# man erhält aus ursprünglichen 11844 Reihen nur noch 2771
df1 <-unique(validfoxdata)

#---
# Nahrung von Vulpes finden (nur "eats" Interaktionen filtern)
# Interaktionen von Füchsen aus globi finden - 1024 Interaktionen
vulpes_interactions<-get_interactions_by_taxa(sourcetaxon = "Vulpes")
# Doppelte Interaktionen entfernen
u_vulpes_inter<-unique(vulpes_interactions) # 551 Interaktionen
geordnet<-u_vulpes_inter[order(u_vulpes_inter$interaction_type),]
vulpes_eats <- geordnet[1:459,]
df4<-vulpes_eats

eats_df <- merge(df1,df4,by.x="species",by.y="target_taxon_name")

#Nahrung von Canidae finden (nur "eats" Interaktionen filtern)
c<-get_interactions_by_taxa(sourcetaxon = "Canidae")
orderc<-c[order(c$interaction_type),]
ohneeatenby <-orderc[15:1024,]
df5<-ohneeatenby[1:322,]

eats_df2 <-merge(df1,df5,by.x="species",by.y="target_taxon_name")

foxeats <- unique(rbind(eats_df,eats_df2))

### zweiter Versuch: direkt Interaktionstyp spezifizieren
vulpes_nahrung <-unique(get_interactions_by_taxa(sourcetaxon="vulpes",
                                                 interactiontype = "eats"))
merge_foxdata_nahrung_vulpes<-merge(df1,vulpes_nahrung,by.x="species",
                                    by.y="target_taxon_name")

# ergibt 34 matches

# direkt Interaktionstyp Canidae:
canidae_nahrung <-unique(get_interactions_by_taxa(sourcetaxon="canidae",interactiontype = "eats"))
merge_foxdata_nahrung_canidae<-merge(df1,canidae_nahrung,by.x="species",by.y="target_taxon_name")

fox_nahrung <-unique(rbind(merge_foxdata_nahrung_canidae,merge_foxdata_nahrung_vulpes))

library(rglobi)
library(stringi)
foxdata <-read.csv(file="ForMilena.csv")

# "NA" bei Species löschen
validfoxdata<-foxdata[!is.na(foxdata$species),]
# doppelte aus validfoxdata (Daten ohne NA in species) entfernen
# man erhält aus ursprünglichen 11844 Reihen nur noch 2771
df1 <-unique(validfoxdata)

# ----------------------
# Interaktionen von Vulpes aus globi finden (1024 Interaktionen)

vulpes_interactions<-get_interactions_by_taxa(sourcetaxon = "Vulpes")
# Doppelte Interaktionen entfernen (man erhält 551 Interaktionen)
u_vulpes_inter<-unique(vulpes_interactions)
#nur folgende Vulpes Interaktionen finden: hasEctoparasite,hasParasite,hasPathogen,hostOf
geordnet<-u_vulpes_inter[order(u_vulpes_inter$interaction_type),]
geordnet_ohneeats<-geordnet[460:551,] #Interaktion "eats" entfernt
#Interaktionen "kill" und "interacts with" entfernen
geordnet_ohnekilleduinteracts<-geordnet_ohneeats[order(geordnet_ohneeats$interaction_type),]
relevant_v_inter <-geordnet_ohnekilleduinteracts[1:71,] #nur 71 Interaktionen
# Dataframe 2 (df2) enthält nur relevante Interaktionen von Vulpes
df2 <- relevant_v_inter

## VIEL EINFACHER: Interaktionstyp direkt spezifizieren 
vulpes_parasites<-unique(get_interactions_by_taxa(sourcetaxon = "vulpes",interactiontype= list("hasParasite","hasPathogen","hostOf","hasEctoparasite")))


# ------------------------
# Interaktionen von Canidae aus globi finden (1024 Interaktionen)

c<-get_interactions_by_taxa(sourcetaxon = "Canidae")
orderc<-c[order(c$interaction_type),]
ohneeats<-orderc[337:1024,]
c_new<-ohneeats[1:672,]
df3<-c_new[1:604,]
df3<-unique(df3)

## VIEL EINFACHER: Interaktionstyp direkt spezifizieren # 526
canidae_parasites<-unique(get_interactions_by_taxa(sourcetaxon = "Canidae",interactiontype= list("hasParasite","hasPathogen","hostOf","hasEctoparasite")))

#------------------------
# Welche "Dinge" im Fuchskot könnten Parasiten des Fuchses sein? 
# Folgende "Dinge" werden als Interaktion (hasParasite, hasPathogen,...) in globi aufgeführt und kommen ebenfalls im Kot vor:

# Df1 und df2 zusammenführen - Interaktionen Vulpes + Fuchsdatensatz # nur 7 matches
new_df<-merge(df1,df2,by.x="species",by.y="target_taxon_name")

# Df1 und df3 zusammenführen - Interaktionen Canidae + Fuchsdatensatz
new_df2<-merge(df1,df3,by.x="species",by.y="target_taxon_name")

#fügt Interaktionen(Canidae,Vulpes) + Fuchsdaten zusammen
x<-unique(rbind(new_df,new_df2))

----------
  
  ### VIEL EINFACHER und keine 1024 Grenze
  # Df1 und vulpes_parasites zusammenfpühren # 71 matches
  merge_vulpes_parasites <-merge(df1,vulpes_parasites, by.x="species",by.y="target_taxon_name")

# Df1 und canidae_parasites zusammenfpühren #59 matches
merge_canidae_parasites <-merge(df1,canidae_parasites, by.x="species",by.y="target_taxon_name")

#Canidae und vulpes matches zusammenführen #103 matches
a<-unique(rbind(merge_vulpes_parasites,merge_canidae_parasites))
