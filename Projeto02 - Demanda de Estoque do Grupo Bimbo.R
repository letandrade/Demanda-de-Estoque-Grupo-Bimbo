##################Projeto 2 - Prevendo Demanda de Estoque com Base em Vendas ########################

#OBS:Para esse código vou prever a demanda da semana 10,do produto 2233 e agência = 1110

####################################################################################################

#Definindo o diretório e memória

setwd("C:/Users/leticia/Desktop/DSA/Projeto")
getwd()

memory.size() 
memory.limit() 
memory.limit(size=56000)

#Bibliotecas

library(psych)
library(ggplot2)
library(lmtest)

#Carregando os dataframes originais

tabela_cliente<- read.csv("cliente_tabla.csv")
tabela_produto<- read.csv("producto_tabla.csv")
amostra<- read.csv("sample_submission.csv")
teste_original<- read.csv("test.csv")
tabela_cidade<- read.csv("town_state.csv")
treino_original<- read.csv("train.csv")


head(tabela_cliente)
head(tabela_produto)
head(amostra)
head(tabela_cidade)
head(treino_original)
head(teste_original)

# Filtrando e sumarizando os data.frames de treino e teste 
# Produto = 2233 e Agência = 1110 Semana = 10
# A escolha do produto 2233 deve-se ao fato de que é o produto com maior frequência na loja 1110

LOJA1110 <- treino_original[treino_original$Agencia_ID == 1110,]

View(table(LOJA1110$Producto_ID)) #Através da função View eu consigo verificar a variável que possui maior frequência, o produto 2233
max(table(LOJA1110$Producto_ID))

# Sabendo dessas condições, vamos filtrar o dataset de treino e teste

treino <- treino_original[(treino_original$Agencia_ID == 1110 & treino_original$Producto_ID == 2233),]
head(treino)
str(treino)

boxplot(treino$Demanda_uni_equil)

teste <- teste_original[(teste_original$Agencia_ID == 1110 & teste_original$Producto_ID == 2233 & teste_original$Semana == 10),c("Canal_ID","Ruta_SAK","Cliente_ID")]
head(teste)
str(teste)

# Resumo

summary(treino[,c("Canal_ID","Ruta_SAK","Cliente_ID","Demanda_uni_equil")])
str(treino[,c("Canal_ID","Ruta_SAK","Cliente_ID","Demanda_uni_equil")])

#Histograma treino - Nesse histograma eu consigo ver que existem algumas demandas que são outliers e que podem comprometer minha análise
# de regressão pois não encontaremos um dist. normal. Por isso, escolhi trabalhar com o intervalo de demanda de 0 a 30. 

hist(treino$Demanda_uni_equil, main = 'Histograma', xlab = 'Demanda')

sort(table(treino$Demanda_uni_equil),decreasing = TRUE)

categorizada <- cut(treino$Demanda_uni_equil, breaks = c(0,30,60,90,120,150),labels = c("0-30","30-60","60-90","90-120","120-150"))
table(categorizada)# Nesse histograma eu consigo ver que a maior frequência está entre as demandas 0 e 30.

treino_ofc <- treino[treino$Demanda_uni_equil<=30,]
hist(treino_ofc$Demanda_uni_equil)

write.csv(treino, "treino_ofc.csv", row.names = FALSE)
write.csv(teste, "teste.csv", row.names = FALSE)

#Entendendo a correlação
  
cor(treino_ofc[,c("Demanda_uni_equil","Canal_ID","Ruta_SAK","Cliente_ID")]) # A correlação entre as variáveis preditoras e a variável target(demanda) não é muito boa

pairs.panels(treino2[,c("Demanda_uni_equil","Canal_ID","Ruta_SAK","Cliente_ID")])

#Treinando o modelo

regressão <- lm(data = treino_ofc,Demanda_uni_equil ~ Canal_ID + Ruta_SAK + Cliente_ID)
summary(regressão)

# As variáveis são significativas e temos relação linear (os coeficientes são diferentes de zero)
# R2 muito baixo

# Equação de regressão y = -7.198380e+01 - 1.515244e+00 * Canal_ID + 2.781020e-02 * Ruta_SAK - 7.270510e-07  * Cliente_ID 

coeficientes <- coefficients(regressão)
coeficientes 

#Avaliação do modelo com o dataset de treino


# Valores observados e previstos

plot(x = treino_ofc$Demanda_uni_equil, 
     y = regressão$fitted.values,
     xlab = "Valores observados",
     ylab = "Valores preditos")
abline(a = 0, b = 1, lty = 2, col = "red")

#Suposições do modelo de regressão linear

#1.0 A relação matemática entre x e y é linear;
# Temos uma relação linear comprovada pela significância, mas é uma relação liner fraca negativa

par(mfrow=c(2,2))

ggplot(treino2, aes(x=treino2$Canal_ID, y=treino2$Demanda_uni_equil)) + 
  geom_point()

ggplot(treino2, aes(x=treino2$Ruta_SAK, y=treino2$Demanda_uni_equil)) + 
  geom_point()

ggplot(treino2, aes(x=treino2$Cliente_ID, y=treino2$Demanda_uni_equil)) + 
  geom_point()


#2.0 Os valores de x são fixos;
#Sim, temos as variáveis preditoras.

#3.0 A média do erro é nula;
#Não é nula
 
mean(regressão$residuals)

#4.0 Para um dado valor de x, a variância de E (erro) é sempre constante (Heterocedasticidade)

bptest(regressão) # Não há heterocedaticidade

#Um valor p> 0,05 indica que a hipótese nula (a variância é imutável no residual) pode 
#ser rejeitada e, portanto, existe heterocedasticidade. 

#5.0 Os erros são não correlacionáveis e têm distribuição normal.
# Os erros são independentes
# Os erros não tem distribuição normal

hist(regressão$residuals,probability = TRUE)
lines(density(regressão$residuals))

shapiro.test(regressão$residuals) # p-valor <0,05 indica que os dados não apresentam normalidade.

Box.test(regressão$residuals, type =  "Ljung-Box") #p-valor <0,05 indica indepêndencia dos dados


#Previsão

previsão_semana10 <- round(data.frame(predict(regressão,teste)))
View(previsão_semana10)
str(previsão_semana10)

previsão_final<- cbind(teste,previsão_semana10)

write.csv(previsão_final, "previsão_final.csv", row.names = FALSE)


#Interpretação final 

#Apesar do modelo possuir coeficientes significativos, alguns pressupostos da regressão não foram atendidos
#e a média dos erros não tende a zero, o que significa que as previsão de demanda não foi tão eficiente. 
#Recomendo a extração de novas variáveis para uma posterior análise.



