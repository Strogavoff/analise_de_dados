#Usamos a função Import Dataset para importar a planilha com os dados da arvore 2

View(Galhas_arvore_2)

galhas=Galhas_arvore_2

View(galhas)

#Instalamos o pacote usando a função install.packages("vcd")
install.packages("vcd")

#Calculo da media para gerar distribuicao esperada
meangalhas <- sum(galhas$numgalhas*galhas$totalfolhas)/sum(galhas$totalfolhas)

#Calculo de frequencias esperadas
galhaesper <- dpois(0:30, lambda = meangalhas)
View(galhaesper)

#Calculo da soma das probabilidades de 1 a 30
prob1a30 <- sum (galhaesper)

#resultado deu 1 ou seja todas as freqs contempladas
folhastotal <- sum(galhas$totalfolhas)
View(folhastotal)

#Calculo do numero esperado de galhas em folhas
galhasnumesp <- galhaesper * folhastotal
View(galhasnumesp)

#Juntar no mesmo dataframe os valores observados e esperados
galhasobsesp <- data.frame(galhas,galhasnumesp)
options("scipen"=100, "digits"=4)
View(galhasobsesp)

#Plot dos valores observados e esperados
plot(galhasobsesp$numgalhas,galhasobsesp$totalfolhas, col="blue", ylab = "Número de Folhas", xlab = "Número de Galhas")
points(galhasobsesp$numgalhas,galhasobsesp$galhasnumesp, col="red")

#Juntar as classes com frequencia menor do que 5 para poder fazer um qui-quadrado
galhasobsesp$grupo <- cut(galhasobsesp$numgalhas, breaks = c(0,1,2,3,31), right = FALSE, labels = c("0","1","2","3 ou mais"))
galhasobsesp
View(galhasobsesp)

#Criacao dos novos grupos com a soma das classes
galhaobsgrupo <- tapply(galhasobsesp$totalfolhas, galhasobsesp$grupo, sum)
galhaobsgrupo
galhaespgrupo <- tapply(galhasobsesp$galhasnumesp, galhasobsesp$grupo, sum)
galhaespgrupo

#Montar a tabela do qui-quadrado
tblobsesp = table(galhaobsgrupo, galhaespgrupo)
View(tblobsesp)

#Fazer o qui-quadrado
chisq.test(tblobsesp)


galhasnum <- unlist(galhas$numgalhas)
galhasfolh <- unlist(galhas$totalfolhas)
galhasunlist <- data.frame(galhasnum, galhasfolh)
galhasunlist
View(galhasunlist)
result = goodfit(galhasunlist, type="nbinomial")
summary(result)

View(Arvore_2)
mean(Arvore_2$`Nº de galhas por folha`)/var(Arvore_2$`Nº de galhas por folha`)
