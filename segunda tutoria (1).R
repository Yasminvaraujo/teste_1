
#PACKAGES
install.packages("faraway")
install.packages("urca")
install.packages("vars")
install.packages("lmtest")
install.packages("faraway")
install.packages("readxl")


library(readxl)

precogas <- read_excel("~/Faculdade/Sexto per?odo/Econometria 3/dados.xls")

precogas$lpme_dist<-log(precogas$pme_dist)
precogas$lpme_rev<-log(precogas$pme_rev)
precogas$lcambio<-log(precogas$cambio)

serie <- ts(precogas,frequency = 12, start = 2002) #serie mensal: 12
plot(serie)

st_lpme_dist = serie[,4]
st_lpme_rev = serie[,5]
st_lcambio = serie[,6]

View(serie)

##############
###GRAFICOS###
##############

#Preco medio de distribuicao de gasolina
plot(st_lpme_dist,ylab='Preco medio de distribuicao de gasolina', xlab = 'Mensal')


decomp_lpme_dist = stl(st_lpme_dist, s.window = "periodic")

dev.new(width=20, height=20)
plot(decomp_lpme_dist)


#Preco medio de revenda de gasolina
plot(st_lpme_rev,ylab='Preco medio de revenda de gasolina', xlab = 'Mensal')

decomp_lpme_rev = stl(st_lpme_rev, s.window = "periodic")

plot(decomp_lpme_rev)

#Cambio
plot(st_lcambio,ylab='Ln do C?mbio', xlab = 'Mensal')

decomp_lcambio = stl(st_lcambio, s.window = "periodic")

dev.new(width=20, height=20)
plot(decomp_lcambio)

#Na presenca de quebra estrutural, os testes sao viesados na direcao da nao rejeicao da hipotese de raiz unitaria.


############################
###Teste de Raiz Unit?ria###
############################

#Primeiro iremos tirar a primeira diferenca
st_lpme_dist_diff <- diff(st_lpme_dist, differences=1)
st_lpme_rev_diff <- diff(st_lpme_rev, differences=1)
st_lcambio_diff <- diff(st_lcambio, differences=1)

#vamos testar a raiz unitaria para cada serie em n?vel e em primeira diferenca
#usaremos o teste Dickey-Fuller Aumentado (ADF)
#O teste ADF testa a hip?tese nula de se uma raiz unit?ria est? presente em um modelo autorregressivo.
#A hip?tese alternativa ? diferente dependendo de qual vers?o do teste ? usada:
#geralmente ? estacion?rio, estacion?rio com intercepto ("drift") ou estacion?rio com tend?ncia ("trend").

### Preco medio de distribuicao de gasolina

### Preco m?dio de distribuicao em n?vel
library(urca)

summary(ur.df(st_lpme_dist, type='none', selectlags = "AIC")) #usa-se o criterio AIC para defasagem
#Value of test-statistic is: 1.1773 > -1.95 = Critical value at 5% (Nao rejeitamos H0 a 5%), logo a serie  nao e estacionaria I(1)

summary(ur.df(st_lpme_dist, type='drift', selectlags = "AIC")) #teste com intercepto
#Value of test-statistic is: -2.2972 4.4893
#-2.2972>-2.88, logo nao rejeita H0 a 5% de ter raiz unitaria I(1)


summary(ur.df(st_lpme_dist, type='trend', selectlags = "AIC")) #teste com intercepto e tendencia
#Value of test-statistic is:  -5 9.9893 12.9709
#-5 < -3.43, logo rejeita H0 a 5% de ter raiz unitaria mas para os demais eh I(1)


#Zivot & Andrews Unit Root Test
#a hip?tese nula ? que a s?rie tem raiz unit?ria com quebra estrutural
#hip?tese alternativa de que s?o estacion?rias com quebras.
#a hip?tese alternativa ? um processo estacion?rio de tend?ncia que permite uma quebra ?nica no n?vel, na tend?ncia ou em ambos
summary(ur.za(st_lpme_dist, model="both"))
#Teststatistic: -4.6>-5,08, logo n?o rejeita H0 a 5% de raiz unit?ria com quebra estrutural


### Preco m?dio de distribuicao em diferenca
summary(ur.df(st_lpme_dist_diff, type='none', selectlags = "AIC")) #I(0)
summary(ur.df(st_lpme_dist_diff, type='drift', selectlags = "AIC"))
summary(ur.df(st_lpme_dist_diff, type='trend', selectlags = "AIC"))

summary(ur.za(st_lpme_dist_diff, model="both")) #Zivot & Andrews Unit Root Test


### Preco medio de revenda de gasolina em nivel
summary(ur.df(st_lpme_rev, type='none', selectlags = "AIC"))
summary(ur.df(st_lpme_rev, type='drift', selectlags = "AIC"))
summary(ur.df(st_lpme_rev, type='trend', selectlags = "AIC"))

summary(ur.za(st_lpme_rev, model="both")) #Zivot & Andrews Unit Root Test


### Preco medio de revenda de gasolina em diferenca
summary(ur.df(st_lpme_rev_diff, type='none', selectlags = "AIC"))
summary(ur.df(st_lpme_rev_diff, type='drift', selectlags = "AIC"))
summary(ur.df(st_lpme_rev_diff, type='trend', selectlags = "AIC"))

summary(ur.za(st_lpme_rev_diff, model="both")) #Zivot & Andrews Unit Root Test


### Cambio em nivel
summary(ur.df(st_lcambio, type='none', selectlags = "AIC"))
summary(ur.df(st_lcambio, type='drift', selectlags = "AIC"))
summary(ur.df(st_lcambio, type='trend', selectlags = "AIC"))

summary(ur.za(st_lcambio, model="both")) #Zivot & Andrews Unit Root Test

summary(ur.df(st_lcambio, type='none', selectlags = "BIC"))
summary(ur.df(st_lcambio, type='drift', selectlags = "BIC"))
summary(ur.df(st_lcambio, type='trend', selectlags = "BIC"))


### Cambio em diferenca
summary(ur.df(st_lcambio_diff, type='none', selectlags = "AIC"))
summary(ur.df(st_lcambio_diff, type='drift', selectlags = "AIC"))
summary(ur.df(st_lcambio_diff, type='trend', selectlags = "AIC"))

summary(ur.za(st_lcambio_diff, model="both", lag=2)) #Zivot & Andrews Unit Root Test


# Quando aplicada a 1 diferenca as tres series se tornam estacionarias

#############################
### As s?ries cointegram? ###
#############################

#Metodo Engle e Granger
#Passo 1: vimos anteriormente que as series em nivel n?o sao estacionarias
#Passo 2: Vamos estimar a equacao lpme_rev = lpme_dist + lcambio + et, para ver se os res?duos s?o estacionarios

attach(precogas)
mod <- lm(st_lpme_rev ~ st_lpme_dist+st_lcambio,precogas)

summary(mod)

erro=resid(mod)
plot(erro)

attach(precogas33333)
mod333 <- lm(st_lpme_rev ~ st_lpme_dist+st_lcambio,precogas)

summary(mod333)

erro333=resid(mod333)
plot(erro333)

#teste Engle-Granger

#teste ADF para saber se os residuos sao estacionarios
#N?o rejeitar H0 indica que os residuos tem raiz unitaria e assim as variaveis nao cointegram
# Considerando que as 3 variaveis sao I(1), basta testar se o erro do modelo ? I(0) com a tabela apropriada.

library(urca)
eg=ur.df(erro) #teste de raiz unitaria no erro
summary(eg)
# H0: Nao ha cointegracao
#Value of test-statistic is: -4.5738 < -3,368 (tabela do teste Engle-Granger) (a confirmar)-> rejeita H0 a 5% I(0)
#Valor tabelado:
#Ao testar em uma tabela apropriada com valores cr?ticos para o teste de cointegracao de Engle e Granger rejeitamos H0
#Como a estatistica do teste e menor que o valor critico,rejeitamos H0, indicando que h? cointegracao


#Passo 3: como as variaveis sao cointegradas, podemos estimar o VECM

#Passo 4: Vericar se o modelo VECM e adequado, para isso vamos testar a heterocedasticidade e a autocorrelacao

#teste de heterocedasticidade Breusch-Pagan
#H0: n?o h? heterocedasticidade
library(lmtest)
bptest(mod)
#p-value = 0.4658>0,05: n?o rejeita H0


# Teste para ver se h? aucorrelacao dos residuos

Box.test(erro,lag=2,type="Ljung-Box")
# p-value < 2.2e-16 < 0.05 -> rejeitamos a hipotese nula que os residuos sao conjuntamente nao
# correlacionados ao longo do tempo, isto e, ha presenca de autocorrelacao.


### Testar autocorrelacao usando o Durbin-Watson test
#H0 = nenhuma autocorrela??o de primeira ordem
dwtest(mod) #p-value < 2.2e-16 -> rejeita H0, logo ha autocorrelacao


###multicolinearidade
explicativas<-data.frame(precogas$lpme_rev,precogas$lcambio)
View(explicativas)

library(faraway)
vif(explicativas) #Variance Inflation Factors - VIF #http://www.est.ufmg.br/portal/arquivos/rts/RT-SE-2009.pdf (pg.49)

#precogas.lpme_rev   precogas.lcambio
#     1.265846          1.265846

#Draper e Smith (1998) recomendam que valores de VIF maiores do que 10 podem causar s?rios problemas na estima??o dos coeficientes de regress?o
#Observa-se valores muito elevados para VIF1 e VIF2, confirmando a presen?a da multicolinearidade.


###########
### VAR ###
###########

#Supondo que as s?ries n?o cointegram, vamos usar os crit?rios de informa??o para escolher a ordem do VAR

serie=cbind(st_lpme_rev_diff,st_lpme_dist_diff, st_lcambio_diff) # variaveis

library(vars)

VARselect(serie,lag.max = 6) # total de lags necessarios para minimizar os 4 criterios
#

# (AKaike, Hannan-Quinn, Schwarz, e Final Prediction Error)

# AIC(n)  HQ(n)  SC(n)  FPE(n)
#  2       1      1      2

# Neste caso, utilizamos 2 lags

var.1=VAR(serie,type="none",p=2)
summary(var.1)
estcoefs = coef(var.1)
estcoefs = rbind(estcoefs[[1]][, 1], estcoefs[[2]][, 1])
round(estcoefs, 2)

#AIC e preferida a outros criterios, devido as suas caracteristicas favoraveis de previsao de pequenas amostras.
var.aic = VAR(serie, type = "none", lag.max = 5, ic = "AIC")
summary(var.aic)


estcoefs.aic = coef(var.aic)
round(estcoefs, 2)

#teste de verificacao
serial.test(var.1, lags.pt = 5, type = "PT.adjusted") #p-value = 0.5945
# A hip?tese nula ? de n?o autocorrela??o #H0: nao autocorrelacao
#p-value = 0.5945: indica que n?o h? autocorrelacao-> modelo bem especificado


# Resposta Impulso do Modelo

ir.1 = irf(var.1, impulse = "st_lpme_dist_diff", response = "st_lpme_rev_diff", n.ahead = 20, ortho = FALSE)
#queremos saber como a st_lpme_rev_diff se comporta apos um choque de st_lpme_dist_diff
plot(ir.1)

ir.1lp = irf(var.1, impulse = "st_lpme_dist_diff", response = "st_lpme_rev_diff", n.ahead = 20, ortho = FALSE, cumulative = TRUE)
plot(ir.1lp)#efeito culmulativo/longo prazo

ir.2 = irf(var.1, impulse = "st_lcambio_diff", response = "st_lpme_rev_diff", n.ahead = 20, ortho = FALSE)
plot(ir.2)

ir.2lp = irf(var.1, impulse = "st_lcambio_diff", response = "st_lpme_rev_diff", n.ahead = 20, ortho = FALSE, cumulative = TRUE)
plot(ir.2lp)#efeito culmulativo/longo prazo



############################################################################################################

###EXTRA###

#Teste de Granger-Causalidade (Pergunta-se se uma variavel e capaz de prever outra e em que condicoes)
#st_lpme_rev_diff ajuda a prever st_lpme_dist_diff st_lcambio_diff?
#H0: st_lpme_rev_diff do not Granger-cause st_lpme_dist_diff e st_lcambio_diff
causality(var.1,cause='st_lpme_rev_diff')
causality(var.aic,cause='st_lpme_rev_diff')

#projecao
var.1proj=predict(var.1,n.ahead=12,ci=0.95)
dev.new(width=20, height=20)
fanchart(var.1proj)

var.1proj=predict(var.aic,n.ahead=12,ci=0.95)
dev.new(width=20, height=20)
fanchart(var.1proj)


##############################################################################################################



##################
### Modelo VECM ##
##################

# Como as series cointegram, especificaremos um modelo VECM

# Longo Prazo: yt = a + b*xt + e -> lm com series em nivel

# Curto Prazo ? dado com series em diferencas considerando o erro do modelo de longo prazo (VECM)

#modelo curto prazo simples seria um MQO dif_y=alpha+beta1 dif_x1+beta_2 dif_x2+beta_3 et-1

erro<- ts(erro)
vecm=VAR(serie,type="none",p=2,exogen=erro[2:224]) #igual ao var mas devemos acrescentar o erro como ex?geno
vecm


serial.test(vecm, lags.pt = 8, type = "PT.adjusted") #p-value = 0.1124>0,05 -> bem especificado
#obs: s? ficou bem especificado a partir de lags.pt=7


########################################################################################################################################################

###EXTRA###

#######################
###Teste De Johansen###
#######################

# H0: somente os r+1  autovalores s?o diferentes de zero, isto e,
# existe r s?ries temporais cointegradas, isto e, nao cointegracao (para r=0).

rev_var=cbind(st_lpme_dist,st_lpme_rev, st_lcambio)


jotest=ca.jo(rev_var, type="trace", K=2, ecdet="none", spec="longrun")

summary(jotest)

#          test 10pct  5pct  1pct
#r <= 2 |  0.00  6.50  8.18 11.65
#r <= 1 | 10.02 15.66 17.95 23.52
#r = 0  | 29.89 28.71 31.52 37.22


# r=0 -> t=29.89 < 31.52 (5%) a 5% de significancia, podemos nao rejeitar a hipotese de nao cointegracao, (teria cointegracao a 10%)

# r<=1 -> t = 10.02 < 17.95 -> nao rejeito  a existencia de 1 s?rie cointegrada 5% de significancia.

# r<=2 -> t = 0.00 < 8.18 -> n?o rejeito H0 a 5% a existencia de 2 s?ries cointegradas

# como nao rejeitou r<=1 e rejeitou r=0, logo existe pelo menos uma serie cointegrada..
###############################################################################################################

#agora vou fazer algo diferente

erro<- ts(erro)
vecm=VAR(serie,type="none",p=2,exogen=erro[2:224]) #igual ao var mas devemos acrescentar o erro como ex?geno
vecm


serial.test(vecm, lags.pt = 8, type = "PT.adjusted")

#agora vou fazer algo mais diferente ainda

erro<- ts(erro)
vecm=VAR(serie,type="none",p=2,exogen=erro[2:224]) #igual ao var mas devemos acrescentar o erro como ex?geno
vecm


serial.test(vecm, lags.pt = 8, type = "PT.adjusted")
