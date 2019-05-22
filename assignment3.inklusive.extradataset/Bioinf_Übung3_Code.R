#In der Funktion "patter_to_number" wird das pattern in die dazugehörige Zahl umgerechnet
#Eingabe "pattern": das pattern(k-mer) das in eine Zahl umgewandelt werden soll
#Ausgabe: Die Zahl die dem pattern entspricht
pattern_to_number <- function(pattern){
  #if-Schleife wird initiiert, wenn das pattern eine Länge von 0 hat wird eine 0 zurückgegeben.
  if(nchar(pattern)==0){
    return(0)
  }
  #Dem Objekt "symbol" wird mithilfe der Funktion "substr" (http://rfunction.com/archives/1692))
  #das letzte Symbol aus dem Pattern zugeordnet
  symbol<- substr(pattern,nchar(pattern),nchar(pattern))
  #Dem Objekt "prefix" werden mithilfe der Funktion "substr" alle Symbole aus dem pattern 
  #außer das letzte zugeordnet
  prefix<- substr(pattern,1,nchar(pattern)-1)
  #Mithilfe der Funktion return(https://www.rdocumentation.org/packages/qrmtools/versions/0.0-10/topics/returns)
  #wird die Zahl des untersuchten patterns ausgegeben. Die Ermittlung der Zahl erfolgt aus der Formel zur 
  #Umrechnung des Symbols zur entsprechenden Zahl. 
  return(4*pattern_to_number(prefix)+symbol_to_number(symbol))
}
#In der Funktion "symbol_to_number" werden den Buchstaben A,C,G,T die Zahlen 0,1,2,3 zugeordnet
#Eingabe "symbol": das Symbol das zur passenden Zahl umgewandelt werden soll
#Ausgabe: Die Zahl die dem Symbol entspricht
symbol_to_number<-function(symbol){
  #Es werden vier if-Schleifen initiiert, die dazu führen, dass wenn das Symbol A bzw. C,G oder T entspricht,
  #die Zahl 0 bzw. 1,2 oder 3 zurückgegeben wird:
  if(symbol=="A"){
    return(0)
  }
  if(symbol=="C"){
    return(1)
  }
  if(symbol=="G"){
    return(2)
  }
  if(symbol=="T"){
    return(3)
  }
}
#In der Funktion "number_to_pattern" wird einer Zahl(index) das entsprechende pattern zugeordnet
#Eingabe "index": Die Zahl des index des gesuchten pattern
#Eingabe "k" : Länge des pattern
#Ausgabe: Das pattern welches zum index gehört
number_to_pattern<- function(index,k){
  #If-Schleife: wenn das gesuchte Pattern aus einem Symbol besteht kann es direkt mithilfe der 
  #Funktion "number_to_symbol" ermittelt werden. 
  if(k==1){
    return(number_to_symbol(index))
  }
  #Dem Objekt "prefix_index" wird die Funktion "quotient" zugeordnet
  prefix_index<- quotient(index,x=4)
  #Dem Objekt "r" wird die Funktion "remainder" zugeordnet
  r<- remainder(index,x=4)
  #Dem Objekt "symbol" wird das Symbol, das zu der Zahl passt, die mithilfe "remainder" berechnet wurde, zugeordnet
  symbol<-number_to_symbol(r)
  #Dem Objekt "prefix_pattern" werden die Symbole, die zu der Zahl passen, die mithilfe "quotient" berechnet wurden, zugeordnet
  prefix_pattern<- number_to_pattern(prefix_index,k-1)
  #Die Symbole aus "prefix_pattern" und "symbol" werden mithilfe der "paste" funktion (https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/paste)
  #verknüpft und ausgegeben.
  return(paste(prefix_pattern,symbol,sep=""))
}
#In der Funktion "quotient" wird berechnet wie viele ganze male der Umrechnungsfaktor 4 in die Zahl des index passt
quotient<- function(index,x=4){
  # %/% http://rfunction.com/archives/1648
  index %/% 4
}
#In der Funktion "remainder" wird berechnet was übrig bleibt nachdem der index durch die maximale Anzahl von 4 geteilt wurde.
remainder<- function(index,x=4){
  # %% http://rfunction.com/archives/1648
  index %% 4
}
#In der Funktion "number_to_symbol" werden den Zahlen 0,1,2,3 die Buchstaben A,C,G,T zugeordnet
#Eingabe "x": Die Zahl die in das Symbol umgewandelt werden soll
#Ausgabe: Das Symbol das der Zahl entspricht 
number_to_symbol<-function(x){
  #Es werden vier If-Schleifen initiiert, die dazu führen, dass wenn die Zahl 0 bzw. 1,2 oder 3 ist, das Symbol A bzw. C,G oder T zurückgegeben wird:  
  if(x==0){
    return("A")
  }
  if(x==1){
    return("C")
  }
  if(x==2){
    return("G")
  }
  if(x==3){
    return("T")
  }
}

######Funktion "faster_frequent_words" funktioniert leider aus irgendeinem Grund nicht 
#In der Funktion "faster_frequent_words" wird eine DNA-Sequenz nach den sich am öftesten k-meren abgesucht
#Eingabe "text":Die DNA-Sequenz die untersucht werden soll
#Eingabe "k":Die Länge des gesuchten pattern
#Ausgabe: Die am häufigsten vorkommenden k-mere in der Sequenz
faster_frequent_words<-function(text,k){
  #Dem Objekt "frequent_patterns" wird eine leere Liste zugeordnet
  frequent_patterns<- {}
  #Dem Objekt "frequency_array" wird die Funktion "computing_frequencies" zugeordnet
  frequency_array <- computing_frequencies(text,k)
  #Dem Objekt "max_count" werden die maximalen Werte aus "frequency_array" mithilfe der "max" Funktion (https://www.rdocumentation.org/packages/rapportools/versions/)
  #zugeordnet
  max_count<- max(frequency_array)
 #For-Schleife, die wenn ein Wert aus "frequency_array" dem Maximum entspricht, das dazugehörige pattern mithilfe "number_to_pattern" bildet 
  # und dieses pattern dann in die Liste "frequent_patterns" überführt
  for(i in 0:(4^k-1)){
    if(frequency_array[i+1]==max_count)
    pattern<- number_to_pattern(i,k)
    append(frequent_patterns,pattern)
    }
# #Mithilfe der Funktion "return"(https://www.rdocumentation.org/packages/qrmtools/versions/0.0-10/topics/returns)
  #und der Funktion "unique", die alle duplikate aus einem Vektor entfernt (Referenz zu "unique": https://www.rdocumentation.org/packages/base/versions/3.5.3/topics/unique)
  #werden die am häufigsten in der DNA-Sequenz vorkommenden k-mere herausgegeben:
  return(unique(frequent_patterns))
}

#In der Funktion "computing_frequencies" wird ein array erstellt der die Häufigkeit enthält wie oft jedes k-mer im text vorkommt
#Eingabe "text": Die DNA-Sequenz die untersucht werden soll
#Eingabe "k": Länge des k-mers
#Ausgabe: Die Häufigkeit aller möglicher k-mere
computing_frequencies<- function(text,k){
  #Dem Objekt "frequency_array" wird eine leere Liste zugeordnet
  frequency_array<- {}
  #For-Schleife, die der Liste "frequency_array" so viele Nullen wie mögliche k-mere zuordnet
  for(i in 0:(4^k-1)){
    frequency_array[i]<-0
    }
  #For-Schleife, die die vorkommenden pattern in der DNA-Sequenz scannt, diese pattern dann in die zugehörige Zahl(index) mit der "pattern_to_number" Funktion
  #umwandelt und anschließend die Zahl die zu diesem Index gehört um 1 erhöht
  for (i in 0:(nchar(text)-k)){
  pattern<-substr(text,i+1,i+k)
  j<- pattern_to_number(pattern)
  frequency_array[j+1]<-frequency_array[j+1]+1
  }
#Der fertige array mit den Häufigkeiten wird ausgegeben
return(frequency_array)
}

