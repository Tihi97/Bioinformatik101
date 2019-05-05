#In der Funktion "frequent_words" wird eine DNA-Sequenz nach sich wiederholenden Basenabfolgen abgesucht
#Eingabe-Parameter "text": Die DNA-Sequenz ,die untersucht werden soll
#Eingabe-Parameter "k" : Anzahl der Basen, die die gesuchte Basenabfolge (k-mer) lang sein soll
#Ausgabe: Die Funktion gibt die in der DNA-Sequenz am häufigsten vorkommenden k-mere aus
frequent_words<-function(text,k){
  
  #Die Liste "frequent_patterns" in der die k-mere eingetragen werden, wird als leere Liste angelegt:
  frequent_patterns<- {}
  
  #Die Liste "count" in der die Häufigkeit der k-mere eingetragen wird, wird als leere Liste angelegt:
  count<- {}
  
  #Eine Schleife wird initiiert, die die DNA-Sequenz durchläuft und mithilfe der Funktion "substr" (Referenz zu "substr": http://rfunction.com/archives/1692)
  #jede Basenabfolge der Länge k heraussucht, indem sie quasi ein Raster der Länge k die DNA-Sequenz lang schiebt und nach jedem Schritt die Basenabfolge speichert,
  #diese werden dem Objekt "pattern" zugewiesen. Anschließend werden mithilfe der Funktion "pattern_count" die Häufigkeiten und die Basenabfolge jedes patterns in der Liste "count" gespeichert:
  for(i in 0:(nchar(text)-k)){
    pattern<-substr(text,i+1,i+k)
    count[i+1]<-pattern_count(text,pattern)
  }
  
  #Dem Objekt "max_count" wird mithilfe der Funktion "max" die höchsten Werte aus der Liste count zugwiesen.
  #Referenz zu "max":https://www.rdocumentation.org/packages/rapportools/versions/1.0/topics/max :
  max_count<-max(count)
  
  #Eine Schleife wird initiiert, die die DNA-Sequenz und zudem die Liste "count" durchläuft, wenn nun ein Wert aus "count" der maximalen Häufigkeit entspricht,
  #wird das pattern, das diesen Index besitzt der Liste "frequent_patterns" zugwiesen:
  for(i in 0:(nchar(text)-k)){
    if(count[i+1]==max_count)
      frequent_patterns<-append(frequent_patterns,substr(text,i+1,i+k))
  }
  
  #Mithilfe der Funktion "return", mit der Daten aus einem Vektor herausgegeben werden (Referenz zu "return": https://www.rdocumentation.org/packages/qrmtools/versions/0.0-10/topics/returns)
  #und der Funktion "unique", die alle duplikate aus einem Vektor entfernt (Referenz zu "unique": https://www.rdocumentation.org/packages/base/versions/3.5.3/topics/unique)
  #werden die am häufigsten in der DNA-Sequenz vorkommenden Basenabfolgen herausgegeben:
  return(unique(frequent_patterns))
}

#In der Funktion "pattern_count" wird die Häufigkeit einer bestimmten Basenabfolge gezählt
#Eingabe-Parameter "text" : Die DNA-Sequenz ,die untersucht werden soll
#Eingabe-Parameter "pattern" : Die Basenabfolge von der die Häufigkeit in der DNA-Sequenz gesucht wird
#Ausgabe: Die Funktion gibt an wie oft das angegebene pattern in der DNA-Sequenz vorkommt
pattern_count<-function(text,pattern){ 
  
  #Dem Objekt "count" wird die Zahl 0 zugewiesen, in "count" wird festgehalten wie oft das pattern in der DNA-Sequenz vorkommt:
  count<-0 
  
  #Dem Objekt "pattern_lenght" wird mithilfe der Funktion "nchar" die Anzahl der Charaktere im pattern zugwiesen.
  #Referenz zu "nchar" : https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/nchar :
  pattern_lenght<- nchar(pattern) 
  
  #Eine Schleife wird initiiert, die die DNA-Sequenz durchläuft und nach dem gesuchten pattern überprüft, wenn das pattern in der Sequenz gefunden wurde,
  #wird die Zahl des Objekts "count" um 1 erhöht:
  for(i in 0:(nchar(text)-pattern_lenght)){
    if(substr(rep(text),i+1,i+pattern_lenght)==pattern)
      count<-count+1
  }
  
  #Die Häufigkeit des untersuchten patterns in der DNA-Sequenz wird mit der return Funktion aus dem Objekt count angefordert:
  return(count)
}
