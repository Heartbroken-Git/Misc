#Exercice 3.1

x <- 1:10
sample(x)
sample(x, replace = TRUE)
sample(x[x > 8])
sample(x[x > 9])
sample(x[x > 10])
resample <- function(x, ...) x[sample(length(x), ...)]
resample(x[x > 8])
resample(x[x > 9])
resample(x[x > 10])

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
for (i in 1:500) {
  E1 = sample(urne, 10)
  E2 = sample(urne, 10, replace = TRUE)
  X = X + sum(E1)
  Y = Y + sum(E2)
}
freqX = X/500
freqY = Y/500
titre1 = "Fréquences obtenues pour \n 500 tirages sans remise"
titre2 = "Fréquences obtenues pour \n 500 tirages avec remise"
par(mfrow=c(1,2))
barplot(X, freqX/500, main=titre1) #Pas fini
barplot(Y, freqY/500, main=titre2)