#Tamanho da populacao de computadores
N<-150
N

#Criacao da populacao de computadores
computadores<-c()
for(i in 1:N){
computadores[i]<-i
}
computadores

#Tamanho da amostra
n<-10
n

#Realizando a Amostragem aleatoria simples
amostraSimp<-sample(computadores,n)
amostraSimp

