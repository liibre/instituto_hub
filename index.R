## ----vetores-------------------------------------------
populacao <- c(1500, 2000) 
estados <- c("Acre", "Alagoas", "Amapá", "Amazonas", "Tocantins")
class(populacao)
class(estados)


## ----vetores2------------------------------------------
verdadeiro_falso <- c(FALSE, TRUE)
verdadeiro_falso
class(verdadeiro_falso)
posicao <- c(1L, 2L)
posicao
class(posicao)


## ----str_length----------------------------------------
str(estados) #estrutura
length(estados)


## ----subsetting, results = "hide"----------------------
estados <- c("Acre", "Alagoas", "Amapá", "Amazonas", "Tocantins")
estados[1]
estados[c(TRUE, FALSE, TRUE, FALSE)]
estados[1:2]
estados[1:4]
#estados[ 1,3,5] #NAO! a vírgula marca dimensões
estados[c(1,3,5)] #tem que ser um vetor


## ----logical_clauses, results="hide"-------------------
casos <- c(150, 200, 400, 500, 500, 600)
casos > 150
casos[casos > 150]
casos >= 150 # superior ou igual
casos < 300
casos == 200
casos != 200


## ------------------------------------------------------
casos[casos > 150]


## ----outras, results="hide"----------------------------
1:10
seq(1, 10, 2)
rep(1:4, 2)
rep(1:4, each = 2)
unique(casos)


## ----pacotes, eval = F---------------------------------
## # Para instalar pacotes desde CRAN
## install.packages("remotes")
## 
## # Para instalar coronabr desde GitHub
## remotes::install_github("liibre/coronabr")
## 
## # Para carregar pacotes
## library(coronabr)
## 
## # Para buscar ajuda
## ?coronabr


## ----coronabr, echo = TRUE-----------------------------
library(coronabr)


## ----get_corona_br, eval = FALSE-----------------------
## caminho <- "dados/brutos"
## get_corona_br(dir = caminho, filename = "01-amapa", uf = "AP")


## ----read_to_tranform_to_df----------------------------
amapa <- read.csv("dados/brutos/01-amapa.csv", stringsAsFactors = FALSE)
class(amapa)


## ----try, echo = TRUE, eval = FALSE--------------------
## names(amapa)
## dim(amapa)
## nrow(amapa)
## ncol(amapa)
## head(amapa) # 6 linhas por padrão
## tail(amapa)
## rownames(amapa)
## length(amapa) # numero de colunas
## summary(amapa) # quantis e a média


## ------------------------------------------------------
amapa[, 1:3] #três primeiras colunas
amapa[4, 13] #quarto elemento da coluna 13



## ----echo = FALSE--------------------------------------
include_graphics("https://www.measureevaluation.org/resources/newsroom/news-images/tidy-data/image")


## ---- echo = FALSE, out.width=300----------------------
include_graphics("figs/data_feminism.png")


## ------------------------------------------------------
amapa <- read.csv("dados/brutos/01-amapa.csv")
head(amapa[c(3, 4, 9:18)])


## ------------------------------------------------------
unique(amapa$city)



## ------------------------------------------------------
table(amapa$place_type)

unique(amapa$place_type[is.na(amapa$city)])


## ------------------------------------------------------
class(amapa$date)
class(amapa$last_available_date)


## ------------------------------------------------------
amapa$date <- as.Date(amapa$date)
amapa$last_available_date <- as.Date(amapa$last_available_date)

class(amapa$date)

range(amapa$date)


## ------------------------------------------------------
acu <- aggregate(last_available_confirmed ~ city, FUN = max, data = amapa)
acu



## ----fig.align='center'--------------------------------
barplot(acu[, 2], names.arg = acu[, 1])


## ------------------------------------------------------
acu.or <- acu[order(acu$last_available_confirmed), ]
acu.or


## ----fig.align='center'--------------------------------
barplot(acu.or[, 2], names.arg = acu.or[, 1], las = 2)


## ------------------------------------------------------
estado <- amapa[amapa$place_type == "state", c(-15)]
dim(estado)


## ------------------------------------------------------
# outra forma: onde a coluna city é NA
estado2 <- amapa[is.na(amapa$city), c(-15)]
## as duas formas tem o mesmo resultado:
all.equal(estado2, estado)


## ------------------------------------------------------
# dados apenas de Macapa por meio do geocode do IBGE
macapa <- amapa[amapa$city_ibge_code == "1600303", c(-15)]


## ------------------------------------------------------
if (!dir.exists("dados/processados/")) {
  dir.create("dados/processados/")
}

# Escrevendo os dados
write.csv(estado, 
          "dados/processados/02-estado_AP.csv",
          row.names = FALSE)

write.csv(macapa, 
          "dados/processados/02-municipio_Macapa.csv", 
          row.names = FALSE)


## ---- eval = FALSE-------------------------------------
## plot(last_available_confirmed ~ date, data = macapa)
## lines(last_available_confirmed ~ date, data = macapa)


## ---- echo = FALSE, fig.align='center'-----------------
plot(last_available_confirmed ~ date, data = macapa)
lines(last_available_confirmed ~ date, data = macapa)


## ---- fig.align='center', out.width=400, echo = FALSE----
include_graphics("./figs/AP_design_ativista.png")


## ----eval = FALSE--------------------------------------
## # para o estado todo
## plot(last_available_confirmed ~ date, data = estado,
##      xlab = "Data de notificação", ylab = "Casos acumulados")
## lines(last_available_confirmed ~ date, data = estado)


## ----echo = FALSE, fig.align='center'------------------
# para o estado todo
plot(last_available_confirmed ~ date, data = estado,
     xlab = "Data de notificação", ylab = "Casos acumulados")
lines(last_available_confirmed ~ date, data = estado)


## ----eval = FALSE--------------------------------------
## # para o estado todo
## plot(last_available_confirmed ~ date, data = estado,
##      xlab = "Data de notificação", ylab = "Casos acumulados")
## lines(last_available_confirmed ~ date, data = estado)
## # linhas e pontos do municipio Macapa
## lines(last_available_confirmed ~ date, data = macapa, col = "red")
## points(last_available_confirmed ~ date, data = macapa, col = "red")
## # legenda
## legend("topleft", c("estado AP", "município Macapá"),
##        pch = 1, col = c("black", "red"))
## 


## ----echo = FALSE, fig.align='center'------------------
# para o estado todo
plot(last_available_confirmed ~ date, data = estado,
     xlab = "Data de notificação", ylab = "Casos acumulados")
lines(last_available_confirmed ~ date, data = estado)
lines(last_available_confirmed ~ date, data = macapa, col = "red")
points(last_available_confirmed ~ date, data = macapa, col = "red")
legend("topleft", c("estado AP", "município Macapá"), 
       pch = 1, col = c("black", "red"))



## ----eval = FALSE--------------------------------------
## plot(x = amapa$estimated_population,
##      y = amapa$last_available_confirmed)


## ----echo = FALSE, fig.align='center'------------------
plot(x = amapa$estimated_population, 
     y = amapa$last_available_confirmed)


## ----eval = FALSE--------------------------------------
## boxplot(last_available_death_rate ~ city,
##         data = amapa)


## ----echo = FALSE, fig.align='center'------------------
boxplot(last_available_death_rate ~ city, 
        data = amapa)


## ---- echo=FALSE, fig.show='asis', fig.align='center', fig.width=8, fig.height=7----
set.seed(2)
par(bty = "n")
bp <- rnorm(1000, 0, 0.1)
boxplot(bp, yaxt = "n", xlim = c(0, 3), ylim = c(-0.3, 0.3))
text(x = 1.82, y = min(bp), "último ponto (-1,5 x IIQ)", cex = .9)
text(x = 1.72, y = quantile(bp)[2], "primeiro quartil", cex = .9)
text(x = 1.72, y = median(bp), "mediana", cex = .9)
text(x = 1.72, y = quantile(bp)[4], "terceiro quartil", cex = .9)
text(x = 1.82, y = bp[203], "último ponto (+1,5 x IIQ)", cex = .9)
arrows(x0 = 0.68, x1 = 0.68, y0 = quantile(bp)[2], 
       y1 = quantile(bp)[4], code = 3, length = 0.05)
text(x = 0.54, y = median(bp), "IIQ", cex = .9)


## ------------------------------------------------------
# Macapa
macapa <- read.csv("dados/processados/02-municipio_Macapa.csv")
# convertendo para classe data
macapa$date <- as.Date(macapa$date)


## ----eval=FALSE----------------------------------------
## plot(new_confirmed ~ date, data = macapa)
## lines(new_confirmed ~ date, data = macapa)


## ----echo=FALSE, fig.align='center'--------------------
plot(new_confirmed ~ date, data = macapa)
lines(new_confirmed ~ date, data = macapa)


## ----eval = FALSE--------------------------------------
## # usando a funcao par para controlar parametros
## par(bty = "l", las = 1)
## # adicionando nome dos eixos
## plot(new_confirmed ~ date, data = macapa,
##      xlab = "Data",
##      ylab = "Casos diários")
## lines(new_confirmed ~ date, data = macapa)


## ----echo = FALSE, fig.align='center'------------------
# usando a funcao par para controlar parametros
par(bty = "l", las = 1)
# adicionando nome dos eixos
plot(new_confirmed ~ date, data = macapa,
     xlab = "Data",
     ylab = "Casos diários")
lines(new_confirmed ~ date, data = macapa)


## ----eval = FALSE--------------------------------------
## png("figs/casos_diarios_Macapa.png", res = 300,
##     width = 1200, height = 1200)
## par(bty = "l", las = 1)
## # adicionando nome dos eixos
## plot(new_confirmed ~ date, data = macapa,
##      xlab = "Data",
##      ylab = "Casos diários")
## lines(new_confirmed ~ date, data = macapa)
## dev.off()


## ----gif, echo = FALSE, out.width=500, fig.align='center'----
include_graphics("https://raw.githubusercontent.com/covid19br/covid19br.github.io/master/fig/gif_BE_05_junho.gif")


## ---- echo = FALSE, fig.align='center'-----------------
include_graphics("./figs/observatorio.png")


## ----obs-diarios,echo = FALSE, fig.align='center', out.width=700----
include_graphics("./figs/diarios_AP.png")

