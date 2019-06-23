if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biostrings")

# Lade "Biostrings" Package aus der Bibliothek:
library(Biostrings)
#Dem Objekt "s1" wird die Basensequenz "GAATC" zugeordnet:
s1 <- "GAATC"
#Dem Objekt "s2" wird die Basensequenz "CATAC" zugeordnet:
s2 <- "CATAC"
#Dem Objekt "sigma" wird eine Scoring Matrix mit Match = 10 und mismatch = -5 über die Funktion 
#"nucleotideSubstitutionMatrix"(Funktion aus dem "Biostrings Package) zugeordnet:
sigma <- nucleotideSubstitutionMatrix(match = 10, mismatch = -5, baseOnly = TRUE)
#"sigma" wird ausgegeben:
sigma
#dem Objekt "alignemt" wird über die in "Biostrings" enthaltene Funktion "pairwaiseAlignment" 
#das bestmögliche Alignment von "s1" und "s2" ,mithilfe der Scoring Matrix "sigma" und einem 
#gap-penalty von -4, zugeordnet:
alignment <- pairwiseAlignment(s1, s2, substitutionMatrix = sigma, gapOpening = -4,
gapExtension = -4, scoreOnly = FALSE)
#"alignment" wird ausgegeben:
alignment


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("DECIPHER")

#Lade "DECIPHER" Package aus der Bibliothek:
library(DECIPHER)
#Dem Objekt "fas" wird die Beispiel Sequenz aus 5.1 zugeordnet:
fas <- system.file("extdata","50S_ribosomal_protein_L2.fas", package="DECIPHER")
#Dem Objekt "dna" wird die mit der Funktion "readDNAStringSet" gelesene Sequenz aus "fas" zugeordnet:
dna <- readDNAStringSet(fas)
#Dem Objekt DNA wird mithilfe der "AlignSeqs" Funktion aus dem DECIPHER Paket die angeordnete Sequenz 
#zugeordnet:
DNA <- AlignSeqs(dna)
#"DNA" wird ausgegeben:
DNA
