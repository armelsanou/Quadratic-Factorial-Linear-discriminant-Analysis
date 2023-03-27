
library("caret")
library("MixSim")
library("MASS")

# Création de jeux de données avec MixSim
# jeu 1
jeu1.Q <- MixSim(MaxOmega = 0.0, BarOmega = 0.0, K = 2, p = 2, sph = TRUE)
jeu1 <- simdataset(n = 500, Pi = jeu1.Q$Pi, Mu = jeu1.Q$Mu, S = jeu1.Q$S)
jeu1.Q$S

#jeu 2
jeu2.Q <- MixSim(MaxOmega = 0.10, BarOmega = 0.05, K = 3, p = 2, sph = TRUE)
jeu2 <- simdataset(n = 500, Pi = jeu2.Q$Pi, Mu = jeu2.Q$Mu, S = jeu2.Q$S)
jeu2.Q$S
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

# Fonction discriminante linéaire 

# ************ Jeu de données 1 ************ 

G1_1=c()
G2_1=c()
for (i in 1:500){
  
  X = jeu1$X[i,]
  G1_1 <- append(G1_1,-0.5*(t(X-jeu1.Q$Mu[1,]))%*%(X-jeu1.Q$Mu[1,]) + log(jeu1.Q$Pi[1]))
  G2_1 <- append(G2_1,-0.5*(t(X-jeu1.Q$Mu[2,]))%*%(X-jeu1.Q$Mu[2,]) + log(jeu1.Q$Pi[2]))
  
  
}

x1<-cbind(matrix(G1_1,500), matrix(G2_1,500))
m1<-apply(x1, 1, which.max)
matrice1<-cbind(x1,matrix(m1,500),matrix(jeu1$id,500))
matrice1


my_plot(jeu1,jeu1$id,"Dataset1 with MixSim")
my_plot(jeu1,matrice1[,3],"Dataset1 with Gi")


CM1 <- confusionMatrix(data=factor(matrice1[,3]), reference = factor(matrice1[,4]))
CM1


# ************ Jeu de données 2 ************ 

G1_2 <- c()
G2_2 <- c()
G3_2 <- c()

for (i in 1:500)
{
  X= jeu2$X[i,]
  G1_2 <- append(G1_2,-0.5*(t(X-jeu2.Q$Mu[1,]))%*%(X-jeu2.Q$Mu[1,]) + log(jeu2.Q$Pi[1]))
  G2_2 <- append(G2_2,-0.5*(t(X-jeu2.Q$Mu[2,]))%*%(X-jeu2.Q$Mu[2,]) + log(jeu2.Q$Pi[2]))
  G3_2 <- append(G3_2,-0.5*(t(X-jeu2.Q$Mu[3,]))%*%(X-jeu2.Q$Mu[3,]) + log(jeu2.Q$Pi[3]))
  
}
x2<-cbind(matrix(G1_2,500), matrix(G2_2,500), matrix(G3_2,500))
x2
m2<-apply(x2, 1, which.max)
matrice2<-cbind(x2,matrix(m2,500),matrix(jeu2$id,500))
matrice2

CM2 <- confusionMatrix(data=factor(matrice2[,4]), reference = factor(matrice2[,5]))
CM22

my_plot(jeu2,jeu2$id,"Dataset2 with MixSim")
my_plot(jeu2,matrix2[,4],"Dataset2 with Gi")



# ************ Jeu de données 3 ************ 

G1_3 <- c()
G2_3 <- c()
G3_3 <- c()

for (i in 1:500)
{
  X= jeu2$X[i,]
  G1_3 <- append(G1_3,-0.5*(t(X-jeu3.Q$Mu[1,]))%*%(X-jeu3.Q$Mu[1,]) + log(jeu3.Q$Pi[1]))
  G2_3 <- append(G2_3,-0.5*(t(X-jeu3.Q$Mu[2,]))%*%(X-jeu3.Q$Mu[2,]) + log(jeu3.Q$Pi[2]))
  G3_3 <- append(G3_3,-0.5*(t(X-jeu3.Q$Mu[3,]))%*%(X-jeu3.Q$Mu[3,]) + log(jeu3.Q$Pi[3]))
  
}

x3<-cbind(matrix(G1_3,500), matrix(G2_3,500), matrix(G3_3,500))
x3
m3<-apply(x3, 1, which.max)
matrice3<-cbind(x3,matrix(m3,500),matrix(jeu3$id,500))
matrice3

CM3 <- confusionMatrix(data=factor(matrice3[,4]), reference = factor(matrice3[,5]))
CM3

my_plot(jeu3,jeu3$id,"Dataset3 with MixSim")
my_plot(jeu3,matrix3[,4],"Dataset 3 with Gi")


