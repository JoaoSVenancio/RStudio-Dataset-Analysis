########################################--TRATAMENTO DE DADOS--##########################

dat<- data_set

dat$Sexo<-factor(dat$Sexo,label=c("Masculino","Feminino"),levels=c(1,2))  #--Recodificar a variavel Sexo

dat$Escolhi<-factor(dat$Escolhi,label=c("Sim","Nao"),levels=c(1,2))  #--Recodificar a variavel Escolhi 

dat$PMentoria<-factor(dat$PMentoria,label=c("Sim","Nao"),levels=c(1,2))  #--Recodificar a variavel PMentoria 

dat$`1ªOpção`<-factor(dat$`1ªOpção`,label=c("Sim","Nao"),levels=c(1,2))  #--Recodificar a variavel Opcao1

############--ANALISE DESCRITIVA UNIVARIADA--######################## 

############--IDADE--######################## 

ttable<-table(dat$Idade)
barplot(prop.table(table(dat$Idade))*100,
        main= "Gráfico barras para a variável 'Idade'", #--Grafico de barras da variavel Idade
        col = c("yellow","purple"),
        cex.main=1,
        xlab='Idade',
        ylab="Percentagem (%)",
        ylim = c(0,100))

boxplot(dat$Idade,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a idade",
        cex.main=1,
        ylab="Diferenca de idades",cex.lab=0.8,
        horizontal=TRUE)

install.packages("ggplot2")
library(ggplot2)

# Para os resultados aparecerem em forma tabela (library(pander))
if(!require("pander")) install.packages('pander')
library(pander)
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)

# ANALISE UNIVARIADA: IDADE

descritivas=dat %>%
  get_summary_stats(Idade, type = "full",
                    show=c("n","min", "max","q1","median","q3","mean","sd")) #--Summario das medidas de localizacao da variavel Idade
descritivas %>% pander(.,split.table=Inf)


############--SEXO--########################

library(plotrix)

relat<-round(prop.table(table(dat$Sexo)),2)
pie3D(relat, labels = paste0(relat*100, "%"),cex=0.8,
      main="Gráfico circular para a variável 'Sexo",cex.main=1,
      col=c("blue","deepink"))
legend(0.3,1, legend = c("Masculino", "Feminino"),
       fil = c("blue", "deepink"),cex=0.5)


############--CURSO--######################## 

ttable<-table(dat$Curso)
barplot(prop.table(table(dat$Curso))*100,
        main= "Grafico barras para a variavel 'Curso'", #--Grafico de barras da variavel Curso
        col = c("red","blue","green"),
        cex.main=1,
        xlab='Curso',
        ylab="Percentagem (%)",
        ylim = c(0,100))


relat<-round(prop.table(table(dat$Curso)),3) 
pie(relat, labels = paste0(relat*100, "%"),cex=1.2,
    main = "Grafico circular para a variavel 'Curso'", cex.main=1, #--Grafico circular da variavel Curso
    col = c("blue","deeppink","green"))
legend("bottomright", c("L.Biotecnologia","L.Bioinformatica","CTeSP TLQB"),
       fill = c("blue","deeppink","green"),
       cex=0.8)


############--ANO CURRICULAR--######################## 

ttable<-table(dat$`Ano Curricular`)
barplot(prop.table(table(dat$`Ano Curricular`))*100,
        main= "Grafico barras para a variavel 'Ano Curricular'", #--Grafico de barras da variavel Ano Curricular
        col = c("grey","grey","grey"),
        cex.main=1,
        xlab='Ano Curricular',
        ylab="Percentagem (%)",
        ylim = c(0,100))

boxplot(dat$`Ano Curricular`,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a idade",
        cex.main=1,
        ylab="Diferenca de Ano Curricular",cex.lab=0.8,
        horizontal=TRUE)

# ANALISE UNIVARIADA: ANO CURRICULAR

descritivas=dat %>%
  get_summary_stats(`Ano Curricular`, type = "full",
                    show=c("n","min", "max","q1","median","q3","mean","sd")) #--Sumario das medidas de localizacao da variavel Ano Curricular
descritivas %>% pander(.,split.table=Inf)


############--OPCAO1--######################## 

relat<-round(prop.table(table(dat$`1ªOpção`)),2) 
pie3D(relat, labels = paste0(relat*100, "%"),cex=1.2,
    main = "Grafico circular para a variável '1ª Opção'", cex.main=1, #--Grafico circular da variavel Opcao1
    col = c("green","red"))
legend("bottomright", c("Sim","Nao"),
       fill = c("green","red"),
       cex=0.9)


############--ESCOLHI CURSO--######################## 

ttable<-table(dat$Escolhi)
barplot(prop.table(table(dat$Escolhi))*100,
        main= "Grafico barras para a variavel 'Escolhi Curso'", #--Grafico de barras da variavel Escolhi Curso
        col = c("green","red"),
        cex.main=1,
        xlab='Escolhi Curso',
        ylab="Percentagem (%)",
        ylim = c(0,100))

relat<-round(prop.table(table(dat$Escolhi)),2) 
pie(relat, labels = paste0(relat*100, "%"),cex=1.2,
    main = "Grafico circular para a variavel 'EscolhiCurso'", cex.main=1, #--Grafico circular da variavel TempoDesloca
    col = c("green","deeppink"))
legend("bottomright", c("Sim","Nao"),
       fill = c("green","deeppink"),
       cex=0.8)


############--TEMPO DESLOCA--######################## 

boxplot(dat$TempoDesloca,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a variável 'Tempo Deslocação'",
        cex.main=1,
        ylab="Diferenca de Tempo de Deslocacao",cex.lab=0.8,
        horizontal=TRUE)

# ANALISE UNIVARIADA: TEMPO DE DESLOCACAO

descritivas=dat %>%
  get_summary_stats(TempoDesloca, type = "full",
                    show=c("n","min", "max","q1","median","q3","mean","sd")) #--Summario das medidas de localizacao da variavel Tempo de Deslocacao
descritivas %>% pander(.,split.table=Inf)

v<-round(var(dat$TempoDesloca),3) #--Variancia da variavel Tempo de Deslocacao
dp<-round(sqrt(v),3) #--Desvio padrao da variavel Tempo de Deslocacao
mean<-mean(dat$TempoDesloca) #--media da variavel Tempo de Deslocacao

cd<-dp/mean #--Coeficiente de Dispersao da variavel Tempo de Deslocacao


############--HORAS ESTUDO--######################## 

boxplot(dat$HorasEstudo,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a variável 'Horas de Estudo'",
        cex.main=1,
        ylim = c(0,70),
        ylab="Número de Horas de Estudo",cex.lab=0.8,
        horizontal = TRUE)

# ANALISE UNIVARIADA: HORAS DE ESTUDO

descritivas=dat %>%
  get_summary_stats(Idade, type = "full",
                    show=c("n","min", "max","q1","median","q3","mean","sd")) #--Summario das medidas de localizacao da variavel Horas Estudo
descritivas %>% pander(.,split.table=Inf)

v<-round(var(dat$HorasEstudo),3) #--Variancia da variavel Horas de Estudo
dp<-round(sqrt(v),3) #--Desvio padrao da variavel Horas de Estudo
mean<-mean(dat$HorasEstudo) #--media da variavel Horas de Estudo

cd<-dp/mean #--Coeficiente de Dispersao da variavel Horas de Estudo


############--HORAS REDES--######################## 

boxplot(dat$HorasRedes,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a variável 'Horas de Redes'",
        cex.main=1,
        ylab="Número de Horas de Redes",cex.lab=0.8,
        horizontal = TRUE)

# ANALISE UNIVARIADA: HORAS DE REDES

descritivas=dat %>%
  get_summary_stats(HorasRedes, type = "full",
                    show=c("n","min", "max","q1","median","q3","mean","sd")) #--Summario das medidas de localizacao da variavel Horas Redes
descritivas %>% pander(.,split.table=Inf)

v<-round(var(dat$HorasRedes),3) #--Variancia da variavel Horas de Redes
dp<-round(sqrt(v),3) #--Desvio padrao da variavel Horas de Redes
mean<-mean(dat$HorasRedes) #--media da variavel Horas de Redes

cd<-dp/mean #--Coeficiente de Dispersao da variavel Horas de Redes


############--HORAS TV--######################## 

boxplot(dat$HorasTV,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a variável Horas de TV",
        cex.main=1,
        ylab="Número de Horas de TV",cex.lab=0.8,
        horizontal = TRUE)

# ANALISE UNIVARIADA: HORAS TV

descritivas=dat %>%
  get_summary_stats(HorasTV, type = "full",
                    show=c("n","min", "max","q1","median","q3","mean","sd")) #--Summario das medidas de localizacao da variavel Horas TV
descritivas %>% pander(.,split.table=Inf)

v<-round(var(dat$HorasTV),3) #--Variancia da variavel Horas de TV
dp<-round(sqrt(v),3) #--Desvio padrao da variavel Horas de TV
mean<-mean(dat$HorasTV) #--media da variavel Horas de TV

cd<-dp/mean #--Coeficiente de Dispersao da variavel Horas de TV


############--HORAS SONO--######################## 

boxplot(dat$HorasSono,na.rm=TRUE,
        main="Diagrama de extremos e quartis para a variável 'Horas Sono'",
        cex.main=1,
        ylab="Número de Horas de Sono",cex.lab=0.8,
        horizontal = TRUE)

# ANALISE UNIVARIADA: Horas Sono

descritivas=dat %>%
  get_summary_stats(Idade, type = "full",
                    show=c("n","min", "max","q1","median","q3","mean","sd")) #--Summario das medidas de localizacao da variavel Horas Sono
descritivas %>% pander(.,split.table=Inf)


v<-round(var(dat$HorasSono),3) #--Variancia da variavel Horas de Sono
dp<-round(sqrt(v),3) #--Desvio padrao da variavel Horas de Sono
mean<-mean(dat$HorasSono) #--media da variavel Horas de Sono

cd<-dp/mean #--Coeficiente de Dispersao da variavel Horas de Sono


############--PMENTORIA--######################## 

relat<-round(prop.table(table(dat$PMentoria)),2) 
pie3D(relat, labels = paste0(relat*100, "%"),cex=1.2,
    main = "Gráfico circular para a variável 'Programa Mentoria'", cex.main=1, #--Grafico circular da variavel Programa de Mentoria
    col = c("green","red"))
legend("bottomright", c("Sim","Nao"),
       fill = c("green","red"),
       cex=0.8)

############--ANALISE DESCRITIVA BIVARIADA-######################## 
###################################################################
############--TABELA DE CONTIGENCIA-######################## 
install.packages("descr")
library(descr)

P6<-(dat$BurnoutP6)
P6<-factor(P6,label=c("Nunca","Quase nunca", "Algumas vezes","Regularmente","Muitas vezes","Quase sempre","Sempre"),levels=c(0,1,2,3,4,5,6))
table<-table(dat$Sexo,P6)
CrossTable(dat$Sexo,dat$BurnoutP6,prop.c = FALSE,prop.chisq = FALSE,prop.t = FALSE)

############--HORAS DE ESTUDO POR SEMANA VS HORAS DIARIAS QUE SE DEDICA AS REDES SOCIAIS(TESTE DE PERSON)--######################## 

install.packages(ggplot2)
library(ggplot2)
ggplot(dat, aes(y = `HorasEstudo` , x = `HorasRedes`)) + geom_point() #--Grafico de dispersao entre Horas de Estudo com Horas de Redes

cov(dat$HorasRedes,dat$HorasEstudo) #--Covarancia entre a Horas de Estudo com Horas de Redes

cor.test(dat$HorasRedes,dat$HorasEstudo, method = "pearson") #--Coeficiente de correlacao de Person

############--SEXO VS CURSO QUE FREQUENTA(TESTE DE CRAMER)--######################## 
install.packages("rcompanion")

tabela1<-table(dat$Sexo,dat$Curso) #--Tabela de contigencia o Sexo e o Curso
tabela<-table(dat$Sexo,dat$Curso)  #
t_linha<-margin.table(tabela,1)    #
tabela<-cbind(tabela,t_linha)      ##--Juncao dos totais marginais por linha e coluna numa tabela de contigencia
t_coluna<-margin.table(tabela,2)   #                        
tabela<-rbind(tabela,t_coluna)     #                       
library(rcompanion)
tabela1
cramerV(tabela1) #--Aplicacao do Cramer para variaveis qualitativas 

############--ESTUDO INFERENCIAL-######################## 
###################################################################
############--1ªQUESTAO-########################  
#alfa=0.05
#Este curso foi a 1ª opcao
#   vs
#Tenho vindo a desinteressar-me pelos estudos desde que entrei na escola

#amostra: qualitativa nominal vs qualitativa ordinal

#################################################################
####### Teste wilcoxon-Mann-Whitney (amostras independ.) ########
#################################################################

# Leitura ficheiro de dados
# load package "readxl"
if(!require(readxl)) install.packages("readxl")
library(readxl)

dat<-read_excel("dat/EscalaBurnoutGrupo5.xlsx")
View(dat)
# Checking the dimension of the data
names(dat)
dim(dat)

# Estatisticas descritivas por grupo
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)
if(!require(dplyr)) install.packages("dplyr") # Instalacao da funcao %>%
library(dplyr)
descritivas = dat %>%
  group_by(`1ªOpção`) %>%
  get_summary_stats(BurnoutP6, type = "full",
                    show=c("n","min", "max","q1","median","q3","mean","sd"))
descritivas

# Para os resultados aparecerem em forma tabela (library(pander))
if(!require("pander")) install.packages('pander')
library(pander)
descritivas %>% pander(.,split.table=Inf)

# Presenca de outliers por amostra (package: rstatistix)
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)
dat %>% group_by(`1ªOpção`) %>%
  identify_outliers(BurnoutP6) %>% pander(.,split.table=Inf)

# Normalidade: Shapiro por grupo (pacote RVAideMemoire)
if(!require(RVAideMemoire)) install.packages("RVAideMemoire") 
library(RVAideMemoire)    # Carregamento do pacote
byf.shapiro(BurnoutP6 ~ dat$`1ªOpção`,dat)

#Sim -> p-value = 1.289e-06 
#Não -> p-value = 2.019e-06 


# Teste de Wilcoxon-Mann-Whitney
wilcox.test(BurnoutP6 ~ `1ªOpção`, data = dat, mu=0,
            alternative="two.sided", correct="True")

#p-value=0.8901

# Obter Valor da estatistica Z do teste Wilcoxon-Mann-Whitney
if(!require(dplyr)) install.packages("dplyr") # Instalacao da funcao %>%
library(dplyr)
wilcox.test(BurnoutP6 ~ `1ªOpção`, data = dat, mu=0,
            alternative="two.sided", correct="True")%>%
  with(tibble(U=statistic,
              Z=qnorm(p.value/2),
              p=p.value))

# Dimensao do efeito: teste Wilcoxon-Mann-Whitney
if(!require(rcompanion)) install.packages("rcompanion") 
library(rcompanion)
wilcoxonR(dat$BurnoutP6, g= dat$`1ªOpção`, ci = T)

# Boxplot
boxplot(dat$BurnoutP6 ~ dat$`1ªOpção`,
        ylab="BurnoutP6",
        xlab= "Foi 1ª opcao")

#p-value maior q 0.05 logo sao independentes

#k=2

############################################################ 
############--2 QUESTAO-######################## 
############################################################  
#alfa=0.05
#Tempo total (em minutos) de deslocação entre Escola e Casa (ida e volta)
#   vs
#Sinto-me de "rastos" no final de um dia na escola

#amostra: quantitativa discreta vs qualitativa ordinal

# Coeficiente de correlação Spearman
cor(dat$BurnoutP2,dat$TempoDesloca,method="spearman")

############################################################ 
############--3 QUESTAO-######################## 
############################################################ 

#################################################################
########### Teste Kruskal-Wallis (amostras independ.) ###########
#################################################################
#ANO CURRICULAR VS BURNOUTP10
# Leitura ficheiro de dados
# load package "readxl"
library(readxl)

View(dat)
# Checking the dimension of the data
names(dat)
dim(dat)
### Recodificacao de variavel
dat$`Ano Curricular`<- factor(dat$`Ano Curricular`,
                              label=c("1","2","3"),
                              levels=c(1,2,3))

# Estatisticas descritivas por "ANO CURRICULAR"
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)
if(!require(dplyr)) install.packages("dplyr") # Instalacao da funcao %>%
library(dplyr)
descritivas=dat %>%
  group_by(`Ano Curricular`) %>%
  get_summary_stats(BurnoutP10, type = "full",
                    show=c("n","min", "max","q1","median","q3","mean","sd"))
descritivas
# Para os resultados aparecerem em forma tabela (library(pander))
if(!require("pander")) install.packages('pander')
library(pander)
descritivas %>% pander(.,split.table=Inf)

# Presenca de outliers por amostra (package: rstatistix)
if(!require(rstatix)) install.packages("rstatix")
library(rstatix)
dat %>% group_by(`Ano Curricular`) %>%
  identify_outliers(BurnoutP10) %>% pander(.,split.table=Inf)


# Verificacao dos Pressupostos:
# Normalidade: Shapiro por grupo (pacote RVAideMemoire)
if(!require(RVAideMemoire)) install.packages("RVAideMemoire") 
library(RVAideMemoire)    # Carregamento do pacote
byf.shapiro(BurnoutP10 ~ dat$`Ano Curricular`,dat)

# Teste de Levene
if(!require(car)) install.packages("car")
library(car)
leveneTest(BurnoutP10 ~ dat$`Ano Curricular`, dat,center=mean)

# Teste de Kruskal-Wallis
kruskal.test(BurnoutP10 ~ dat$`Ano Curricular`,dat)

# Dimensao do efeito: eta_quadrado ordinal
if(!require("pander")) install.packages('pander')
library(pander)
if(!require("rstatix")) install.packages('rstatix')
library(rstatix)
kruskal_effsize(BurnoutP10 ~ `Ano Curricular`, data = dat, ci =T) %>% 
  pander(.,split.table=Inf)

# Teste de Dunn
if(!require(FSA)) install.packages("FSA")
library(FSA)
dunnTest(BurnoutP10 ~ dat$`Ano Curricular`,dat,
         method="bonferroni")

# Boxplot
boxplot(BurnoutP10 ~ dat$`Ano Curricular`,dat,
        ylab="BurnoutP10",
        xlab= "Ano Curricular ")


#####################################################################
################### Regressao Linear Simples ########################
#####################################################################

#Variavel dependente: Nº horas diarios que se dedica as redes sociais

#Variavel indepente: Nº horas estudo por semana

## Verificacao de existencia relacao linear: Diagrama de Dispersao
plot(dat$HorasEstudo,dat$HorasRedes, 
     xlab="Nº Horas estudo", 
     ylab="Nºhoras diarios que se dedica as redes sociais",
     pch = 19)
corr=data.frame(correl=paste("Coeficiente de correlacao de Pearson: r = ", 
                             round(cor(dat$HorasEstudo,dat$HorasRedes),4),
                             sep=""))
legend("topleft", 
       legend=c(paste(corr)),
       bty="n", cex=0.8)

#### Construcao do modelo: ####
modelo = lm(HorasEstudo ~ HorasRedes,dat)
summary(modelo)

if(!require("pander")) install.packages('pander')
library(pander)
summary(modelo)  %>%   pander(.,split.table=Inf)

## Obter o valor estimado da VD a partir de uma VI
#6 horas nas redes
VI=6
new_VI = data.frame(HorasRedes=c(VI))
prev=predict(modelo, newdata = new_VI)
cat("O valor esperado para y quando x =",VI,"e de", prev)

## Obter o valor estimado da VD a partir de uma VI
#2 horas nas redes
VI=2
new_VI = data.frame(HorasRedes=c(VI))
prev=predict(modelo, newdata = new_VI)
cat("O valor esperado para y quando x =",VI,"e de", prev)

### Significancia pratica: 
R2 = summary(modelo)$r.squared #Coefiente de determinacao
R2_Ajust = summary(modelo)$adj.r.squared #Coefiente de determinacao
cat("O coefiente de determinacao tem o valor",R2,"e o coefiente de 
    determinacao ajustado o valor",R2_Ajust)

## Diagrama de dispersao com reta ajustada
plot(dat$HorasEstudo,dat$HorasRedes, 
     xlab="HorasEstudo (X)", 
     ylab="HorasRedes (Y)",pch = 19)
abline(modelo,col="red")
coeficientes=data.frame(modelo=paste("Y^=",round(coefficients(modelo)[1],4),
                                     " + (",round(coefficients(modelo)[2],4),
                                     " * X)", sep=""))
R_2=data.frame(paste("R^2 =",
                     round(summary(modelo)$r.squared,4)))
R_2_Ajust=data.frame(paste("R^2 adj =",
                           round(summary(modelo)$adj.r.squared,4)))
legend("topleft",
       legend=c(paste(coeficientes),paste(R_2),paste(R_2_Ajust)),
       bty="n", cex=0.6)

#### Verificacao pressupostos do modelo: 
## Representacao grafica:
par(mfrow=c(2,2))
plot(modelo,pch = 19)
par(mfrow=c(1,1))

## verificacao pressupostos: Usando Testes hipotese
# Normalidade dos residuos
shapiro.test(modelo$residuals)

# Outliers nos residuos
summary(rstandard(modelo))

# Independencia dos residuos (Durbin-Watson)
#Estatistica deve estar entre 1 e 3 para existir inependencia
if(!require(car)) install.packages("car")
library(car)
durbinWatsonTest(modelo)

# Homocedasticidade (Breusch-Pagan):
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)
bptest(modelo)

###################################################################
############--Regressao Linear Multipla-######################## 
###################################################################

# variavel dependente : Nº horas diários que se dedica às redes sociais --> quantitativa

#variavel independetes: Nº horas estudo por semana  --> quantitativa
#                       Sexo                        --> qualitativa


#####################################################################
######## Analise 1: Modelo de regressao com variavel dummy ##########
#####################################################################

# A introducao da variavel dummy, permite construir um unico modelo
# que comporta as 2 regressoes de uma so vez.
# variavel "dummy": fem = 1 se Feminino; fem = 0 caso contrario 
# Modelo: yi^ = b0 + b1 * x1 + b2 * fem  
# onde variavel dependente   = yi =  "HorasRedes"
#      variavel independente = xi = "HorasEstudo"
#                            = fem = 1 se feminino; 0 se masculino
#
# Criar a variavel dummy "Fem":
Fem = ifelse(dat$Sexo=="Feminino",1,0)

## Estimar modelo
modelo_fem <-lm(HorasRedes ~ HorasEstudo + Fem,data=dat)
summary(modelo_fem)
## Pressupostos do modelo de regressao (Representacao grafica):
par(mfrow=c(2,2))
plot(modelo_fem,pch = 19)
par(mfrow=c(1,1))

## Representacao grafica do modelo
plot(dat$HorasEstudo,dat$HorasSono,type='n')
points(dat$HorasEstudo[dat$Sexo=="Masculino"],
       dat$HorasSono[dat$Sexo=="Masculino"],col='blue',pch=19)
points(dat$HorasEstudo[dat$Sexo=="Feminino"],
       dat$HorasSono[dat$Sexo=="Feminino"],col='red',pch=19)
legend(4,115, legend=c("Masculino", "Feminino"), cex=0.6,
       fill = c("blue","red"))
abline(coef(modelo_fem)[1],coef(modelo_fem)[2], col='blue')
abline(coef(modelo_fem)[1]+coef(modelo_fem)[3],
       coef(modelo_fem)[2], col='red')


#################################################################
##################### Comparacao modelos ########################
#################################################################

## Criterios de AIC e BIC para comparar quaisquer modelos:
AIC(modelo, modelo_fem)
BIC(modelo, modelo_fem)

## Para comparar modelos aninhados
anova(modelo, modelo_fem)


#####################################################################
################### Analise Fatorial ################################
#####################################################################
# nº de variaveis = 15
# Matrix Correlação = 15*15 = 225
# Excluir Diagonal = 225 - 15 = 210
# nº de correlações a analisar = 210/2 = 105
# Vão ser estudadas 105 correlações 

# 105 - 41 = 64

#64/105 = 0,61

#################### Analise da matriz de correlacao #################

# Carregar os pacotes que serao usados
if(!require(psych)) install.packages("psych")
library(corrgram)
if(!require(Hmisc)) install.packages("Hmisc")
library(Hmisc)
if(!require(corrplot)) install.packages("corrplot")
library(corrplot)

dat<-dat[,14:28]
names(dat)

# Matriz correlacao Pearson com p-value
mat_corr <- rcorr(as.matrix(dat),type = c("spearman"))

# Matriz Triangular Inferior: Coef Correlacao
# Matriz Triangular Superior: p-values
lowerUpper(round(mat_corr$r,3),upper=round(mat_corr$P,3))


# representacoes graficas da matriz correlacao
corrplot(mat_corr$r,p.mat=mat_corr$P,
         sig.level=0.05,method="number",type="lower")



##################### Adequacao dos dados ############################

# Carregar os pacotes que serao usados
if(!require(psych)) install.packages("psych")
library(corrgram)

## Teste de esfericidade de Barlett
# Ho: A matriz correlacao nao difere da matriz identidade
# H1: A matriz correlacao difere da matriz identidade
cortest.bartlett(mat_corr$r,n=nrow(dat))

## Medida de adequabilidade de KMO
KMO(dat)

################# Obtencao da solucao inicial #######################
#Estimacao dos loadings a partir do Metodo das componentes Principais
dat<-dat[,14:28]
names(dat)

st_dat<-scale(dat)

fit<-princomp(st_dat,cor=TRUE)
fit
summary(fit)
#Obs.:
#  cor = TRUE: as componentes principais serao geradas a partir da
#              matriz de correlacao.
#  cor = FALSE: as componentes principais serao geradas a partir da
#              matriz de covariancia.

# Screeplot
screeplot(fit)
plot(fit,type="lines")

# Obtencao da solucao com o numero de fatores sugeridos
#Sem rotacao
fit4<-principal(st_dat, nfactors=4,
                n.obs=nrow(dat),rotate="none", scores=TRUE)
fit4
#Observacoes ao output anterior:
# na tabela vemos que itens carregam mais em seus fatores
# h2 - representa a comunalidade
# u2 - representa a singularidade dos itens

# Percentagem variancia acumulada
fit4$Vaccounted

# Valores das comunalidades
fit4$communality

# Representacao grafica
fit4_varimax<-principal(st_dat, nfactors=4,
                        n.obs=nrow(dat),rotate="varimax",scores=TRUE)
fit4_varimax

########### Interpretacao da solucao fatorial

# Representacao grafica

fa.diagram(fit4_varimax)

#Fiabilidade de fator 1

library(psych)

fator1<-dat[,1:5]
fator1

alpha(fator1)









