#install.packages("R.matlab")
library(R.matlab)
library(mvtnorm)
estimativas_exemplo_GAN = readMat("C:/Users/vitor/OneDrive/Área de Trabalho/Tese INSPER/Roy model GAN code/main_roy.mat")

theta=estimativas_exemplo_GAN$theta2
theta_GAN=vector()
for(k in 1:ncol(estimativas_exemplo_GAN$theta2.AdvL)){
  theta_GAN[k]=mean(estimativas_exemplo_GAN$theta2.AdvL[,k])
}

theta_GAN_NN=vector()
for(k in 1:ncol(estimativas_exemplo_GAN$theta2.AdvL)){
  theta_GAN_NN[k]=mean(estimativas_exemplo_GAN$theta2.AdvN[,k])
}

escolha_primeiro_periodo=as.numeric(runif(300,0,1)>0.5)
X=t(estimativas_exemplo_GAN$X)
Y_11=X[X[,2]<2,1] #salario no primeiro periodo  e escolheu setor 1 n
Y_12=X[X[,2]>1,1] 

media_11=mean(X[X[,2]<2,1])
media_12=mean(X[X[,2]>1,1])
media_21_1=mean(X[X[escolha_primeiro_periodo==0,4]<2,3])
media_21_2=mean(X[X[escolha_primeiro_periodo==1,4]<2,3])
media_22_1=mean(X[X[escolha_primeiro_periodo==0,4]>1,3])
media_22_2=mean(X[X[escolha_primeiro_periodo==1,4]>1,3])

n_11=length(X[X[,2]<2,1])
n_12=length(X[X[,2]>1,1])
n_21_1=length(X[X[escolha_primeiro_periodo==0,4]<2,3])
n_21_2=length(X[X[escolha_primeiro_periodo==1,4]<2,3])
n_22_1=length(X[X[escolha_primeiro_periodo==0,4]>1,3])
n_22_2=length(X[X[escolha_primeiro_periodo==1,4]>1,3])


#Primeira parte da equação
mu_Q <- c(1.9,2.16)
Sigma_Q <- matrix(c(0.94/n_11,0.75/(n_11*n_12),0.75/(n_11*n_12),0.97/n_12),2,2)
f_Q_GAN=dmvnorm(x = c(media_11,media_12), mean = mu_Q, sigma = Sigma_Q)

#Segunda parte da equação
mu_P <- c(1.9,2.16)
Sigma_P <- matrix(c(0.94/n_21_1,0.75/(n_21_1*n_21_2),0.75/(n_21_1*n_21_2),0.97/n_21_2),2,2)
f_P_GAN=dmvnorm(x = c(media_21_1,media_21_2), mean = mu_P, sigma = Sigma_P)

#Terceira parte da equação
mu_R <- c(1.9,2.16)
Sigma_R <- matrix(c(0.94/n_22_1,0.75/(n_22_1*n_22_2),0.75/(n_22_1*n_22_2),0.97/n_22_2),2,2)
f_R_GAN=dmvnorm(x = c(media_22_1,media_22_2), mean = mu_R, sigma = Sigma_R)

f_GAN=f_Q_GAN*f_P_GAN*f_R_GAN



#Primeira parte da equação
mu_Q <- c(1.8,2)
Sigma_Q <- matrix(c(1/n_11,0.5/(n_11*n_12),0.5/(n_11*n_12),1/n_12),2,2)
f_Q=dmvnorm(x = c(media_11,media_12), mean = mu_Q, sigma = Sigma_Q)

#Segunda parte da equação
mu_P <- c(1.8,2)
Sigma_P <- matrix(c(1/n_21_1,0.5/(n_21_1*n_21_2),0.5/(n_21_1*n_21_2),1/n_21_2),2,2)
f_P=dmvnorm(x = c(media_21_1,media_21_2), mean = mu_P, sigma = Sigma_P)

#Terceira parte da equação
mu_R <-c(1.8,2)
Sigma_R <- matrix(c(1/n_22_1,0.5/(n_22_1*n_22_2),0.5/(n_22_1*n_22_2),1/n_22_2),2,2)
f_R=dmvnorm(x = c(media_22_1,media_22_2), mean = mu_R, sigma = Sigma_R)

f=f_Q*f_P*f_R


LR=2*log(f_GAN/f)
