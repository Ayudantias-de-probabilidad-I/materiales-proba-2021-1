#Tirar dados

tiradas <- sample(1:6, 300, replace = TRUE)


prop_mayoresa4 <- vector()
mayor4 <- tiradas>4
head(mayor4)
head(tiradas)
for (i in 1:300) { 
  dados_m4 <- mayor4[c(1:i)]
  prop_mayoresa4 <- c(prop_mayoresa4, sum(dados_m4)/i)
}

head(prop_mayoresa4)
tail(prop_mayoresa4)
summary(prop_mayoresa4)
plot(1:300, prop_mayoresa4)



#Repetir el proceso en 10000 tiros
tiradas<- vector()
tiradas<- sample(1:6, 10000, replace = TRUE)


prop_mayoresa4 <- vector()
mayor4 <- tiradas>4
for (i in 1:10000) {
  dados_m4 <- mayor4[c(1:i)]
  prop_mayoresa4 <- c(prop_mayoresa4, sum(dados_m4)/i)
}

summary(prop_mayoresa4)
plot(1:10000, prop_mayoresa4)
tail(prop_mayoresa4)

#Problema de las cartas
cartas_sombrero <- c("n1", "n2", "b1", "b2","n3", "b3")
sacar_carta <- function(num_rep){
  carta_boca_arriba <- sample(cartas_sombrero, 
                        num_rep, replace = TRUE)
    n1_a <- carta_boca_arriba == "n1" 
    n2_a <- carta_boca_arriba == "n2" 
    n3_a <- carta_boca_arriba == "n3"
    negra_arriba <- as.logical( n1_a+ n2_a+ n3_a)
  veces_blanco_abajo <- sum(carta_boca_arriba[negra_arriba] == "n3")
  veces_blanco_abajo/length(carta_boca_arriba[negra_arriba])
}


