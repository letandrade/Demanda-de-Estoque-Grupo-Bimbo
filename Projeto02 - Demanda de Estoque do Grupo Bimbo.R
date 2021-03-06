##################Projeto 2 - Prevendo Demanda de Estoque com Base em Vendas ########################

#OBS:Para esse c�digo vou prever a demanda da semana 10,do produto 2233 e ag�ncia = 1110

####################################################################################################

#Definindo o diret�rio e mem�ria

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
# Produto = 2233 e Ag�ncia = 1110 Semana = 10
# A escolha do produto 2233 deve-se ao fato de que � o produto com maior frequ�ncia na loja 1110

LOJA1110 <- treino_original[treino_original$Agencia_ID == 1110,]

View(table(LOJA1110$Producto_ID)) #Atrav�s da fun��o View eu consigo verificar a vari�vel que possui maior frequ�ncia, o produto 2233
max(table(LOJA1110$Producto_ID))

# Sabendo dessas condi��es, vamos filtrar o dataset de treino e teste

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

#Histograma treino - Nesse histograma eu consigo ver que existem algumas demandas que s�o outliers e que podem comprometer minha an�lise
# de regress�o pois n�o encontaremos um dist. normal. Por isso, escolhi trabalhar com o intervalo de demanda de 0 a 30. 

hist(treino$Demanda_uni_equil, main = 'Histograma', xlab = 'Demanda')

sort(table(treino$Demanda_uni_equil),decreasing = TRUE)

categorizada <- cut(treino$Demanda_uni_equil, breaks = c(0,30,60,90,120,150),labels = c("0-30","30-60","60-90","90-120","120-150"))
table(categorizada)# Nesse histograma eu consigo ver que a maior frequ�ncia est� entre as demandas 0 e 30.

treino_ofc <- treino[treino$Demanda_uni_equil<=30,]
hist(treino_ofc$Demanda_uni_equil)

write.csv(treino, "treino_ofc.csv", row.names = FALSE)
write.csv(teste, "teste.csv", row.names = FALSE)

#Entendendo a correla��o
  
cor(treino_ofc[,c("Demanda_uni_equil","Canal_ID","Ruta_SAK","Cliente_ID")]) # A correla��o entre as vari�veis preditoras e a vari�vel target(demanda) n�o � muito boa

pairs.panels(treino2[,c("Demanda_uni_equil","Canal_ID","Ruta_SAK","Cliente_ID")])

#Treinando o modelo

regress�o <- lm(data = treino_ofc,Demanda_uni_equil ~ Canal_ID + Ruta_SAK + Cliente_ID)
summary(regress�o)

# As vari�veis s�o significativas e temos rela��o linear (os coeficientes s�o diferentes de zero)
# R2 muito baixo

# Equa��o de regress�o y = -7.198380e+01 - 1.515244e+00 * Canal_ID + 2.781020e-02 * Ruta_SAK - 7.270510e-07  * Cliente_ID 

coeficientes <- coefficients(regress�o)
coeficientes 

#Avalia��o do modelo com o dataset de treino


# Valores observados e previstos

plot(x = treino_ofc$Demanda_uni_equil, 
     y = regress�o$fitted.values,
     xlab = "Valores observados",
     ylab = "Valores preditos")
abline(a = 0, b = 1, lty = 2, col = "red")

#Suposi��es do modelo de regress�o linear

#1.0 A rela��o matem�tica entre x e y � linear;
# Temos uma rela��o linear comprovada pela signific�ncia, mas � uma rela��o liner fraca negativa

par(mfrow=c(2,2))

ggplot(treino2, aes(x=treino2$Canal_ID, y=treino2$Demanda_uni_equil)) + 
  geom_point()

ggplot(treino2, aes(x=treino2$Ruta_SAK, y=treino2$Demanda_uni_equil)) + 
  geom_point()

ggplot(treino2, aes(x=treino2$Cliente_ID, y=treino2$Demanda_uni_equil)) + 
  geom_point()


#2.0 Os valores de x s�o fixos;
#Sim, temos as vari�veis preditoras.

#3.0 A m�dia do erro � nula;
#N�o � nula
 
mean(regress�o$residuals)

#4.0 Para um dado valor de x, a vari�ncia de E (erro) � sempre constante (Heterocedasticidade)

bptest(regress�o) # N�o h� heterocedaticidade

#Um valor p> 0,05 indica que a hip�tese nula (a vari�ncia � imut�vel no residual) pode 
#ser rejeitada e, portanto, existe heterocedasticidade. 

#5.0 Os erros s�o n�o correlacion�veis e t�m distribui��o normal.
# Os erros s�o independentes
# Os erros n�o tem distribui��o normal

hist(regress�o$residuals,probability = TRUE)
lines(density(regress�o$residuals))

shapiro.test(regress�o$residuals) # p-valor <0,05 indica que os dados n�o apresentam normalidade.

Box.test(regress�o$residuals, type =  "Ljung-Box") #p-valor <0,05 indica indep�ndencia dos dados


#Previs�o

previs�o_semana10 <- round(data.frame(predict(regress�o,teste)))
View(previs�o_semana10)
str(previs�o_semana10)

previs�o_final<- cbind(teste,previs�o_semana10)

write.csv(previs�o_final, "previs�o_final.csv", row.names = FALSE)


#Interpreta��o final 

#Apesar do modelo possuir coeficientes significativos, alguns pressupostos da regress�o n�o foram atendidos
#e a m�dia dos erros n�o tende a zero, o que significa que as previs�o de demanda n�o foi t�o eficiente. 
#Recomendo a extra��o de novas vari�veis para uma posterior an�lise.



