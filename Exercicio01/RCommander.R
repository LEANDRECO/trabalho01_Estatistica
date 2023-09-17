
Dataset <- 
  readXL("D:/Faculdade/4 - Período/Estatistica_Basica/Trabalho01/Exercicio01/Dados_Renda_Trabalho01.xlsx",
   rownames=FALSE, header=TRUE, na="", sheet="Dados_Trabalho_01", 
  stringsAsFactors=TRUE)
Dataset <- within(Dataset, {
  Situacao_Laboral <- as.factor(Situacao_Laboral)
})
#Amostragem Estratificada

#Determinando que o Identificador eh a variavel resposta
dados<-Dataset$Identificador

#Tamanho da populacao
N<-length(dados)

N

#Estratificando pela Situacao_Laboral

#Lembrete: os dados numericos da Situacao_Laboral foram convertidos para fatores

situacao<-Dataset$Situacao_Laboral


#Tamanho da amostra
n<-150

#Verificando quantos elementos tem na populacao de cada Situacao_Laboral
summary(situacao)

#Dividindo a populacao em grupos homogeneos

#Situacao_Laboral 1
sit1<-c()

#Situacao_Laboral 2
sit2<-c()

#Situacao_Laboral 3
sit3<-c()

#Situacao_Laboral 4
sit4<-c()

#Situacao_Laboral 5
sit5<-c()

#Situacao_Laboral 6
sit6<-c()

#Situacao_Laboral 7
sit7<-c() 

i1<-1
i2<-1
i3<-1
i4<-1
i5<-1
i6<-1
i7<-1

for(i in 1:N){
	if(situacao[i]=="1"){
		sit1[i1]<-dados[i]
		i1<-i1+1
	}
	else{
		if(situacao[i]=="2"){
			sit2[i2]<-dados[i]
			i2<-i2+1
		}
		else{
			if(situacao[i]=="3"){
				sit3[i3]<-dados[i]
				i3<-i3+1
			}
			else{
				if(situacao[i]=="4"){
					sit4[i4]<-dados[i]
					i4<-i4+1
				}
				else{
					if(situacao[i]=="5"){
						sit5[i5]<-dados[i]
						i5<-i5+1
					}
					else{
						if(situacao[i]=="6"){
							sit6[i6]<-dados[i]
							i6<-i6+1
						}
						else{
							sit7[i7]<-dados[i]
							i7<-i7+1
						}
					}
				}
			}
		}
	}
}

sit1
sit2
sit3
sit4
sit5
sit6
sit7

#Definindo a proporcao de amostras para cada Situacao_Laboral
f<-n/N

f

n1<-round(f*(length(sit1)),0)
n1

n2<-round(f*(length(sit2)),0)
n2

n3<-round(f*(length(sit3)),0)
n3

n4<-round(f*(length(sit4)),0)
n4

n5<-round(f*(length(sit5)),0)
n5

n6<-round(f*(length(sit6)),0)
n6

n7<-round(f*(length(sit7)),0)
n7

#Amostrando cada Situacao_Laboral

amostra1<-sample(sit1,n1)
amostra2<-sample(sit2,n2)
amostra3<-sample(sit3,n3)
amostra4<-sample(sit4,n4)
amostra5<-sample(sit5,n5)
amostra6<-sample(sit6,n6)
amostra7<-sample(sit7,n7)

#Juntando os elementos amostrados em um unico conjunto de dados
amostraEst<-c(amostra1,amostra2,amostra3,amostra4,amostra5,amostra6,amostra7)
amostraEst

#Media salarial
renda<-Dataset$renda

rendasAmostrais<-c()
for(i in 1:length(amostraEst)){
	rendasAmostrais[i]<-renda[amostraEst[i]]
}

rendasAmostrais

rendaMedia<-mean(rendasAmostrais)
rendaMedia
mediaSal<-rendaMedia/12
mediaSal


