## Exemple 2 - MixSim


library("caret")
library("MixSim")
library("MASS")

# Création de jeux de données avec MixSim
# jeu 1
jeu1.Q <- MixSim(MaxOmega = 0.0, BarOmega = 0.0, K = 2, p = 2, sph = TRUE)
jeu1 <- simdataset(n = 500, Pi = jeu1.Q$Pi, Mu = jeu1.Q$Mu, S = jeu1.Q$S)


#jeu 2
jeu2.Q <- MixSim(MaxOmega = 0.10, BarOmega = 0.05, K = 3, p = 2, sph = TRUE)
jeu2 <- simdataset(n = 500, Pi = jeu2.Q$Pi, Mu = jeu2.Q$Mu, S = jeu2.Q$S)

#jeu 3
jeu3.Q <- MixSim(MaxOmega = 0.10, BarOmega = 0.05, K = 3, p = 2, sph = FALSE)
jeu3 <- simdataset(n = 500, Pi = jeu3.Q$Pi, Mu = jeu3.Q$Mu, S = jeu3.Q$S)


# Représentation graphique 
my_plot = function(jeu, predicts, title){
  plot(jeu$X, col = colors[predicts], pch = 19, cex = 0.8,
       xlab = "", ylab = "", axes = F, main = title)
  box()
}

colors <- c("red", "green", "blue")
par(mfrow=c(1,3))

# Fonction discriminante quadratique 


# ************ Jeu de données 1 ************ 

Sigma1_1 = matrix(jeu1.Q$S[1:4], nrow = 2)
Sigma2_1 = matrix(jeu1.Q$S[5:8], nrow = 2)

G1_1 <- c()
G2_1 <- c()

for (i in 1:500)
{
  X= jeu1$X[i,]
  G1_1 <- append(G1_1,-0.5*log(det(Sigma1_1))-0.5*(t(X-jeu1.Q$Mu[1,]))%*%solve(Sigma1_1)%*%(X-jeu1.Q$Mu[1,]) + log(jeu1.Q$Pi[1]))
  G2_1 <- append(G2_1,-0.5*log(det(Sigma2_1))-0.5*(t(X-jeu1.Q$Mu[2,]))%*%solve(Sigma2_1)%*%(X-jeu1.Q$Mu[2,]) + log(jeu1.Q$Pi[2]))
}
matrice_1 <- cbind(matrix(G1_1,500),matrix(G2_1,500))
matrice_1<-cbind(cbind(matrix(G1_1,500), matrix(G2_1,500)),matrix(apply(matrice_1, 1, which.max),500))
matrice_1

# ************ Jeu de données 2 ************ 


Sigma1_2 = matrix(jeu2.Q$S[1:4],nrow=2)
Sigma2_2 = matrix(jeu2.Q$S[5:8],nrow=2)
Sigma3_2 = matrix(jeu2.Q$S[9:12],nrow=2)



G1_2 <- c()
G2_2 <- c()
G3_2 <- c()

for (i in 1:500)
{
  X= jeu2$X[i,]
  G1_2 <- append(G1_2,-0.5*log(det(Sigma1_2))-0.5*(t(X-jeu2.Q$Mu[1,]))%*%solve(Sigma1_2)%*%(X-jeu2.Q$Mu[1,]) + log(jeu2.Q$Pi[1]))
  G2_2 <- append(G2_2,-0.5*log(det(Sigma2_2))-0.5*(t(X-jeu2.Q$Mu[2,]))%*%solve(Sigma2_2)%*%(X-jeu2.Q$Mu[2,]) + log(jeu2.Q$Pi[2]))
  G3_2 <- append(G3_2,-0.5*log(det(Sigma3_2))-0.5*(t(X-jeu2.Q$Mu[3,]))%*%solve(Sigma3_2)%*%(X-jeu2.Q$Mu[3,]) + log(jeu2.Q$Pi[3]))
  
}
matrice_2 <- cbind(matrix(G1_2,500),matrix(G2_2,500),matrix(G3_2,500))
matrice_2<-cbind(cbind(matrix(G1_1,500), matrix(G2_1,500),matrix(G3_2,500)),matrix(apply(matrice_2, 1, which.max),500))


# ************ Jeu de données 3 ************ 

Sigma1_3 = matrix(jeu3.Q$S[1:4],nrow=2)
Sigma2_3 = matrix(jeu3.Q$S[5:8],nrow=2)
Sigma3_3 = matrix(jeu3.Q$S[9:12],nrow=2)



G1_3 <- c()
G2_3 <- c()
G3_3 <- c()

for (i in 1:500)
{
  X= jeu2$X[i,]
  G1_3 <- append(G1_3,-0.5*log(det(Sigma1_3))-0.5*(t(X-jeu3.Q$Mu[1,]))%*%solve(Sigma1_3)%*%(X-jeu3.Q$Mu[1,]) + log(jeu3.Q$Pi[1]))
  G2_3 <- append(G2_3,-0.5*log(det(Sigma2_3))-0.5*(t(X-jeu3.Q$Mu[2,]))%*%solve(Sigma2_3)%*%(X-jeu3.Q$Mu[2,]) + log(jeu3.Q$Pi[2]))
  G3_3 <- append(G3_3,-0.5*log(det(Sigma3_3))-0.5*(t(X-jeu3.Q$Mu[3,]))%*%solve(Sigma3_3)%*%(X-jeu3.Q$Mu[3,]) + log(jeu3.Q$Pi[3]))
  
}
matrice_3 <- cbind(matrix(G1_3,500),matrix(G2_3,500),matrix(G3_3,500))
matrice_3<-cbind(cbind(matrix(G1_3,500), matrix(G2_3,500),matrix(G3_3,500)),matrix(apply(matrice_3, 1, which.max),500))

matrice_3


## ************* Plotting ************************ 
# 
my_plot(jeu1,jeu1$id,"Dataset1 with MixSim")
my_plot(jeu1,matrice_1[,3],"Dataset1 with Gi")

my_plot(jeu2,jeu2$id,"Dataset2 with MixSim")
my_plot(jeu2,matrice_2[,4],"Dataset2 with Gi")

my_plot(jeu3,jeu3$id,"Dataset3 with MixSim")
my_plot(jeu3,matrice_3[,4],"Dataset 3 with Gi")


#Matrice de confusion 

cm1 <- confusionMatrix(data=factor(matrix1[,3]), reference = factor(matrix1[,4]))
cm1

cm2 <- confusionMatrix(data=factor(matrix2[,4]), reference = factor(matrix2[,5]))
cm2

cm3 <- confusionMatrix(data=factor(matrix3[,4]), reference = factor(matrix3[,5]))
cm3










