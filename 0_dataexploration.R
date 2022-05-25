library(ggplot2)
library(dplyr)

foxdata <- read.csv(file="ForMilena.csv")

# 1_Barplot: Übersicht Doppelte und nicht NA spezifiziert
totalcounts_fd <- nrow(foxdata) # Total Counts
unique_fd <- nrow(unique(foxdata)) # Only unique counts
unique_fd_s <-unique(foxdata[!is.na(foxdata$species),]) # ohne NA in species
number_uniquefds <- nrow(unique_fd_s)
fd <- unique_fd_s %>% filter(!grepl('unidentified|uncultured', species)) 
fd_2 <- nrow(fd)

x <- c(totalcounts_fd,unique_fd,number_uniquefds,fd_2)
y <- c("Total counts","Unique counts","without NA in species", 
       "without unidentified/uncultured")

tab <- matrix(x, ncol=4, byrow=TRUE)
colnames(tab) <- y
tab <- as.table(tab)
barplot(tab, main = "Barchart", ylab= "Artenanzahl", ylim = c(0,20000))

# 2_Barplot: Verteilung Artenanzahl über Superkingdom
table_kingdom <- table(fd$superkingdom)
barplot(table_kingdom,main = "Anzahl Arten pro Superkingdom", 
        ylim=c(0,2500),xlab="Superkingdom", ylab="Artenanzahl")

# 3_ 5 häufigste Bakterienarten (Barplot mit Genus)
bacteria_sub <- subset(fd,fd$superkingdom=="Bacteria")
bacteria_genus <- bacteria_sub[!is.na(bacteria_sub$genus),]
b_genus <- as.data.frame(table(bacteria_genus$genus))
b_genus5 <- b_genus[order(b_genus$Freq,decreasing=TRUE),][1:5,]
barplot(b_genus5$Freq, main="Five most common Bacteria genus",
        ylab="Name of Genus",xlab= "Frequency", horiz=TRUE,col="darkblue", 
        names.arg = c(b_genus5$Var1))

# 4_ 5 häufigste Bakterienfamilien (Barplot mit Familien)
bacteria_sub <- subset(fd,fd$superkingdom=="Bacteria")
bacteria_fam <- bacteria_sub[!is.na(bacteria_sub$family),]
b_family <- as.data.frame(table(bacteria_fam$family))
b_family5 <- b_family[order(b_family$Freq,decreasing=TRUE),][1:5,]
barplot(b_family5$Freq, main="Five most common Bacteria families",
        ylab="Name of family",xlab= "Frequency", horiz=TRUE,col="darkblue", 
        names.arg = c(b_genus5$Var1))

# 5_ 5 häufigste Eukaryotenfamilien
eukaryota_sub <- subset(fd,fd$superkingdom=="Eukaryota")
Euk_fam <- eukaryota_sub[!is.na(eukaryota_sub$family),]
e_family <- as.data.frame(table(Euk_fam$family))
e_family5 <- e_family[order(e_family$Freq,decreasing=TRUE),][1:5,]
barplot(e_family5$Freq, main="Five most common Eukaryota families",
        ylab="Name of family",xlab= "Frequency", horiz=TRUE,col="darkblue", 
        names.arg = c(b_genus5$Var1))

# 6_ 5 häufigste Eukaryoten Arten (Genus)
Euk_genus <- eukaryota_sub[!is.na(eukaryota_sub$genus),]
e_genus <- as.data.frame(table(Euk_genus$genus))
e_genus5 <- e_genus[order(e_genus$Freq,decreasing=TRUE),][1:5,]
barplot(e_genus5$Freq, main="Five most common Eukaryota Genus",
        ylab="Name of Genus",xlab= "Frequency", horiz=TRUE,col="darkblue", 
        names.arg = c(b_genus5$Var1))

# 7 wie Nr.6 nur mit ggplot
ggplot(e_genus5, aes(x=Freq,y=Var1))+geom_bar(stat="identity",fill="steelblue") +
  labs(title="Five most common Eukarypta genus", x="Frequency", 
       y = "Name of Genus") + geom_text(aes(label=..count..),stat='count',
                                        position=position_dodge(0.9),vjust=-0.2)


