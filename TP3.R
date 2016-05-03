#TP3.R Auteurs : Corentin CHEDOTAL & Mayeul TOUCHARD

#Exercice 3.1

x <- 1:10 
message("x <- 1:10 donne le vecteur [1,2,...,10]")
sample(x) 
message("sample(x) tire aléatoirement tous les éléments de x et les place en vecteur")
sample(x, replace = TRUE) 
message("même chose que ci-dessus mais avec remise")
sample(x[x > 8])
message("sample(x[x > 8]) tire parmi les éléments de x supérieurs à 8")
sample(x[x > 9])
message("comme sample(x)")
sample(x[x > 10])
message("sample(x[x > 10]) retourne integer(0) comme aucun x n'est supérieur à 10")
resample <- function(x, ...) x[sample(length(x), ...)]
message("resample <- function(x, ...) x[sample(length(x), ...)] créer la fonction resample")
resample(x[x > 8])
message("resample(x[x > 8]) fait la même chose que sample(x[x > 8])")
resample(x[x > 9])
message("resample(x[x > 9]) tire parmi les éléments de x > 9 (ici 10)")
resample(x[x > 10])
message("comme sample(x[x > 10]")

#Exemple d'application : l'épreuve du lancer de dé

res_dé = sample(6, 500, replace = T)
table(res_dé)
res_dé_pipé = sample(6, 500, replace = TRUE, prob=c(0.5,0.1,0.1,0.1,0.1,0.1))
table(res_dé_pipé)
titre1 = "Fréquences obtenues pour \n 500 lancers de dé équilibré"
titre2 = "Fréquences obtenues pour \n 500 lancers de dé pipé"
par(mfrow=c(1,2))
barplot(table(res_dé)/500, main=titre1)
barplot(table(res_dé_pipé)/500, main=titre2)

#Autre application :

urne = c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0)
X = 0
Y = 0
nbX = c(0,0,0,0,0,0,0,0,0,0,0)
nbY = c(0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:500) {
  E1 = sample(urne, 10)
  E2 = sample(urne, 10, replace = TRUE)
  nbX[sum(E1)] = nbX[sum(E1)] + 1
  nbY[sum(E2)] = nbY[sum(E2)] + 1
}
titre1 = "Répartition obtenue pour \n 500 tirages sans remise"
titre2 = "Répartition obtenue pour \n 500 tirages avec remise"
par(mfrow=c(2,2))
barplot(nbX/500, main=titre1, names.arg=labels)
barplot(nbY/500, main=titre2, names.arg=labels)
labels=seq(0, 10)
titre3 = "Répartition théorique des tirages \n de boules blanches sans remise"
barplot(dhyper(0:10, 5, 10, 10), main=titre3, names.arg=labels)
titre4 = "Répartition théorique des tirages \n de boules blanches avec remise"
barplot(dbinom(0:10, 10, 1/3), main=titre4, names.arg=labels)

#Exercice 3.2

message("Sn = X1 + ... + Xn suit une loi binomiale de paramètre n = n et p = p, ici on a fixé n = 5 et p = 0.5")

calculRn = function(n, N, e, p) {
  sN = rbinom(N, n, p)
  sWn = sN / n
  tmp = abs(sWn - p)
  Rn = sum(tmp > e) / N
  return(Rn)
}
message("Pour la question 2) on a Rn =")
calculRn(5, 100, 0.1, 0.5)

message("Pour la question 3) pour les différents n on a")
calculRn(10, 100, 0.1, 0.5)
calculRn(25, 100, 0.1, 0.5)
calculRn(50, 100, 0.1, 0.5)
calculRn(75, 100, 0.1, 0.5)
calculRn(100, 100, 0.1, 0.5)
calculRn(150, 100, 0.1, 0.5)
calculRn(200, 100, 0.1, 0.5)
calculRn(500, 100, 0.1, 0.5)

message("Pour la question 4) pour e = 0.01 et les différents n on a")
calculRn(10, 100, 0.01, 0.5)
calculRn(100, 100, 0.01, 0.5)
calculRn(200, 100, 0.01, 0.5)
calculRn(500, 100, 0.01, 0.5)
calculRn(1000, 100, 0.01, 0.5)
calculRn(5000, 100, 0.01, 0.5)
calculRn(10000, 100, 0.01, 0.5)
calculRn(50000, 100, 0.01, 0.5)

message("On constate que Rn tend vers 0, on en conclut que P(Wn) converge vers p quand n tend vers l'infini")

#Exercice 3.3

approxPoisson = function(k, l) {
  return((l^(k)/factorial(k))*exp(-l))
}

comparaisonbinpois = function (x, n, p){
  poisson <- approxPoisson(x, 5)
  binomiale <- dbinom(x,n,p)
  A <- gl(5,1,5,labels=x)
  data <- cbind(binomiale,poisson)
  rownames(data) <- levels(A)
  barplot(t(data),beside=T,legend.text=colnames(data),
          col=c("grey50","grey80"),ylab="Probabilités", xlab="k")
  erreurn = max(abs(binomiale-dpois(x,5)))
  message("L'erreur entre la loi binomiale et la loi de Poisson est de ", erreurn)
}
comparaisonbinpois(0:10, 10, 0.5)
comparaisonbinpois(0:20, 20, 0.25)
comparaisonbinpois(0:50, 50, 0.1)
comparaisonbinpois(0:100, 100, 0.05)

message("On en conclut qu'une approximation par loi binomiale est d'autant plus proche de la loi de Poisson que n est grand")

#Exercice 3.4

message("Cette épidémie suit une loi binomiale de paramètre n=100 et p=0.02")

n = 100
p = 0.02
x = 2
repBino=pbinom(x, n, p)
message("La probabilité d'avoir plus d'un malade dans le village est de ", repBino)

x = 9.9
repBino=pbinom(x, n, p, lower.tail=TRUE)
message("La probabilité d'avoir au plus dix malades dans le village est de ", repBino)

message("Afin d'approximer la loi binomiale par une loi de Poisson on utilisera l=2")
x = 2
l = 2
repPois=ppois(x, l)
message("La probabilité par approximation avec loi de Poisson d'avoir plus d'un malade dans le village est de ", repPois)

message("Afin d'approximer la loi binomiale par une loi de Poisson on utilisera l=2")
x = 9.9
repPois=ppois(x, l, lower.tail=TRUE)
message("La probabilité par approximation avec loi de Poisson d'avoir au plus dix malades dans le village est de ", repPois)

message("L'approximation par loi de Poisson tend à être précise à la cinquième décimale près")

#Exercice 3.5

simulation = function(n, p){
  tablex = c(rbinom(0:499, n, p))

  plot(0:499, tablex, main="Mesures binomiales")

  tableaufreq = c(table(tablex)/500)

  par(mfrow =c(1,2))
  barplot(tableaufreq, ylab="fréquences",main="Fréquences empiriques")

  barplot(dbinom(0:15, n, p), ylab="fréquences",main="Fréquences théoriques",names.arg=0:15)

  par(mfrow =c(1,2))
  barplot(cumsum(tableaufreq), ylab="fréquences",main="Fréquences cumulées \n empiriques")

  barplot(cumsum(dbinom(0:15, n, p)),  ylab="fréquences",main="Fréquences cumulées \n théoriques",names.arg=0:15)

  moyenne = mean(tablex, 500)
  message("La moyenne empirique pour un échantillon de 500 est ", moyenne)

  esperance = weighted.mean(0:15,dbinom(0:15, n, p))
  message("L'espérance de la loi binomiale est ", esperance)
  message("La moyenne empirique et l'espérance sont assez proche")

  variance = var(tablex)
  message("La variance empirique de l'échantillon est ", variance)

  variance = n*p*(1-p)
  message("La variance de la loi binomiale est ", variance)
  message("Les deux variances sont proches")
}

simulation(15, 0.3)

message("Comme demandé à la question 12, une deuxième simulation est exécutée")
simulation(15, 0.3)

message("En conclusion :")
message("On remarque que la loi binomiale, bien que théorique, est une bonen approximation des différents résultats obtenus lors du tirage des échantillons.")
message("En effet, les résultats tels que l'espérance, la moyenen ou la variance sont relativement proches.")