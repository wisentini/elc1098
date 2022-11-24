## autores:
##    Marcos Visentini    <mvisentini@inf.ufsm.br>
##    Henrique Fochesatto <hfochesatto@inf.ufsm.br>


library(tools)
library(arules)


achar_nomes_unicos <- function(df, distancia_minima_aceitacao) {
  nomes <- unique(toTitleCase(trimws(unlist(strsplit(df$Jogadore.a.s, ", ")))))
  nomes_unicos <- c()
  
  for (i in 1:length(nomes)) {
    distancias <- adist(nomes[i], nomes_unicos, ignore.case = TRUE)
    distancias <- distancias[distancias != 0]
    
    if (length(distancias) == 0 || min(distancias) >= distancia_minima_aceitacao) {
      nomes_unicos <- append(nomes_unicos, nomes[i])
    }
  }
  
  return(sort(nomes_unicos))
}

achar_nome_correspondente <- function(nome, nomes) {
  distancias <- adist(nome, nomes, ignore.case = TRUE)

  return(nomes[match(min(distancias), distancias)])
}

one_hot_encode <- function(df) {
  nomes_unicos <- achar_nomes_unicos(df, 4)
  
  z <- data.frame(matrix(0, nrow = nrow(df), ncol = length(nomes_unicos) + 1))
  
  header <- append(nomes_unicos, "Vitoria")
  
  names(z) <- header
  
  for (i in 1:nrow(df)) {
    nomes_celula <- toTitleCase(unlist(strsplit(df$Jogadore.a.s[i], ", ")))
    
    for (j in 1:length(nomes_celula)) {
      nome_processado <- achar_nome_correspondente(nomes_celula[j], nomes_unicos)
      indice_coluna <- match(nome_processado, nomes_unicos)
      z[i, indice_coluna] <- 1
    }
    
    if (df$Amigos[i] > df$Oponentes[i]) {
      z$Vitoria[i] <- 1
    }
  }
  
  z[] <- lapply(z, factor)
  
  return(z)
}


csv_1 <- read.csv("http://www-usr.inf.ufsm.br/~joaquim/UFSM/DM/ds/_ASSOC_BGFriends_01.csv")

csv_2 <- read.csv("http://www-usr.inf.ufsm.br/~joaquim/UFSM/DM/ds/_ASSOC_BGFriends_02.csv", encoding = "latin1")

df <- rbind(csv_1, csv_2)

df <- df[df$Oponentes != df$Amigos,]

df <- one_hot_encode(df)


regras_simples <- apriori(df, parameter = list(target = "rules", supp = 0.1, conf = 0.5))

regras_melhor_jogador <- subset(regras_simples, size(lhs) == 1 & lhs %pin% "=1" & rhs %in% "Vitoria=1")


regras_duplas <- apriori(df, parameter = list(target = "rules", supp = 0.1, conf = 0.7))

regras_melhor_dupla <-  subset(regras_duplas, size(lhs) == 2 & !(lhs %pin% "=0") & rhs %in% "Vitoria=1")

regras_pior_dupla <- subset(regras_duplas, size(lhs) == 2 & !(lhs %pin% "=0") & rhs %in% "Vitoria=0")


inspect(sort(regras_melhor_jogador, decreasing = TRUE, by = "confidence"), linebreak = FALSE)

inspect(sort(regras_melhor_dupla, decreasing = TRUE, by = "confidence"), linebreak = FALSE)

inspect(sort(regras_pior_dupla, decreasing = TRUE, by = "confidence"), linebreak = FALSE)
