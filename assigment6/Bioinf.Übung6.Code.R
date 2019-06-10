#Lade "HMM" aus der library (musste vorher runtergeladen werden):
require(HMM)
#Dem Objekt "nSim" wird die Zahl 2000 zugeordnet:
nSim = 2000
#Dem Objekt "States" wird ein Vektor mit den Zuständen "Fair" und "Unfair" zugeordnet:
States = c("Fair", "Unfair")
#Dem Objekt "Symbols" werden die Zahlen von 1 bis 6 zugeordnet:
Symbols = 1:6
#Dem Objekt "transProb" werden die Übergangswahrscheinlichkeiten zwischen den beiden Zuständen "Fair und "Unfair" zugeordnet:
transProbs = matrix(c(0.99, 0.01, 0.02, 0.98), c(length(States),
                                                 length(States)), byrow = TRUE)
#Dem Objekt "emmissionProbs" werden die Wahrscheinlichkeit für jede Augenzahl des Würfels von den beiden Würfeln "Fair" und "Unfair" 
#zugeordnet (Bsp: "Unfair" 1:5 = 0.1, 6 = 0.5): 
emissionProbs = matrix(c(rep(1/6, 6), c(rep(0.1, 5), 0.5)),
                       c(length(States), length(Symbols)), byrow = TRUE)
#Dem Objekt "hmm" werden alle nötigen Kriterien eines Hidden Markov Modells (Zustände, Anfangswahrscheinlichkeiten, 
#Übergangswahrscheinlichkeiten, Beobachtungswahrscheinlichkeit, die möglichen Beobachtungen) zugeordnet:
hmm = initHMM(States, Symbols, transProbs = transProbs, emissionProbs =
                emissionProbs)
#Dem Objekt "sim" werden die Ergebnisse, wann welcher Würfel benutzt wurde, zugeordnet, die der Realität entsprechen.
sim = simHMM(hmm, nSim)
#Dem Ojekt "vit" werden die Ergebnisse, wann welcher der beiden Würfel geworfen wurde, zugeordnet, die der theoretischen Berechnung
#entsprechen. Die Berechnung erfolgt mit dem viterbi-Algorithmus:
vit = viterbi(hmm, sim$observation)
#Dem Objekt f werden die Wahrscheinlichkeiten für die beiden Würfel für jeden Wurf zugeordnet:
f = forward(hmm, sim$observation)
#Dem Objekt "i"..
i <- f[1, nSim]
#Dem Objekt "j"..
j <- f[2, nSim]
#"probObservations" wird die Formel zugewiesen, mit der die Wahrscheinlichkeiten der Beobachtungen berechnet werden können:
probObservations = (i + log(1 + exp(j - i)))
#########################################
## NO MORE DOCUMENTATION BELOW THIS LINE
#########################################
x = list(hmm = hmm, sim = sim, vit = vit)
# PLOT simulated throws at top #####################
mn = "Fair and unfair die"
xlb = "Throw nr."
ylb = ""
plot(x$sim$observation, ylim = c(-7.5, 6), pch = 3, main = mn,
     xlab = xlb, ylab = ylb, bty = "n", yaxt = "n")
axis(2, at = 1:6)
# PLOT Simulated, which die was used (ground truth) ###########
text(0, -1.2, adj = 0, cex = 0.8, col = "black", "True: green = fair die")
for (i in 1:nSim) {
  if (x$sim$states[i] == "Fair")
    rect(i, -1, i + 1, 0, col = "green", border = NA)
 else rect(i, -1, i + 1, 0, col = "red", border = NA)
}
  # PLOT Most probable path (viterbi) #######################
text(0, -3.2, adj = 0, cex = 0.8, col = "black", "Most probable path")
for (i in 1:nSim) {
  if (x$vit[i] == "Fair")
    rect(i, -3, i + 1, -2, col = "green", border = NA)
 else rect(i, -3, i + 1, -2, col = "red", border = NA)
}
  # PLOT Differences ####################
text(0, -5.2, adj = 0, cex = 0.8, col = "black", "Difference")
differing = !(x$sim$states == x$vit)
for (i in 1:nSim) {
  if (differing[i])
    rect(i, -5, i + 1, -4, col = rgb(0.3, 0.3, 0.3),border = NA)
  else rect(i, -5, i + 1, -4, col = rgb(0.9, 0.9, 0.9),
border = NA)
}