# Das r Package "rGADEM" wird installiert:
#source("https://bioconductor.org/biocLite.R")
biocLite("rGADEM")
# "rGADEM" wird aus der Bibliothek geladen und kann dann genutzt werden: 
library("rGADEM")
#Dem Objekt "pwd" wird ein leerer character Vektor zugeordnet:
pwd <- ""
#Dem Objekt "path" wird ein dataframe("extdata/Test_100.fasta") zugeordnet, welches mit der "system.file" Funktion 
#(https://www.rdocumentation.org/packages/devtools/versions/1.13.6/topics/system.file) aus dem "rGADEM" package geladen wurde:
path<- system.file("extdata/Test_100.fasta", package="rGADEM")
#Mithilfe der "paste" Funktion (https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/paste) 
#werden die Vektoren "pwd" & "path" verkettet und sep="" seperiert die einzelnen Elemente. Der dabei entstandene
#Vektor wird dem Objekt "FastaFile" zugeordnet: 
FastaFile <- paste(pwd, path, sep="")
#Mithilfe der "readDNAStringSet" Funktion werden aus "FastaFile" die DNA Strings "fasta" gelesen und dem Objekt "sequences" zugeordnet:
sequences <- readDNAStringSet(FastaFile, "fasta")
#Mit "GADEM" greift auf ein C Programm zurück, welches aus der gegebenen DNA-Sequenz Motive ermittelt und die Prädiktivität der Motive 
#für die DNA-Sequenz errechnet:
gadem <- GADEM(sequences, verbose=1)
#Mithilfe der Funktion "consensus" wird die Konsensus Sequenz aus dem zuvor definierten Objekt "gadem" wiedergegeben:
consensus(gadem)
#Die in gadem enthaltenen Motive werden mithilfe der GADEM Funktion "motiflist" in einem Array dargestellt und Dem Objekt "pwm" 
#(position weight matrices) zugeordnet. 
pwm <- gadem@motifList[[1]]@pwm
#Mithilfe dem Paket "seqLogo" wird die Konsensus-Sequenz in einem sequence logo Diagramm dargestellt:
seqLogo(pwm)

