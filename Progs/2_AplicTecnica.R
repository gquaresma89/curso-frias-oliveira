
####################################
#####                         ######
#####     FRIAS E OLIVEIRA    ######
#####                         ######
####################################

# Código atualizado em: 11-09-2020 
# Versão do R: 4.0.2

## Carregando as bibliotecas necessárias
pkg <- c('openxlsx', 'data.table', 'dplyr')

lapply(pkg, library, character.only = TRUE)

## Definição inicial
setwd('C:/Users/Guilherme Quaresma/OneDrive/Guilherme/Cursos/# Elaborados/TransFec')

## Carregando o arquivo necessário
load("Dados/PreProcessamento.RData")

## Criando a função Frias e Carvalho
FO_TEF <- function(Base, Regiao, Age, Mi, FT, FS) {
  
  ## Vetores para a aplicação da técnica (padrão)
  
  # Vetor Ai para a Parturição - Tabela A1
  Ai <- c(0.1219156510, 0.1370351947, 0.1507705101, 0.1634522358, 
          0.1752967022, 0.1864547046, 0.1970363731) 
  
  # Vetor k de Ai
  k <- 0.86668444
  
  # Vetores para a Fecundidade acumulada - Tabela A2
  F_20 <- c(0.301590, 1.250309, -0.800978, 0.220274)
  F_25 <- c(0.144403, 1.256802, -0.356139)
  F_30 <- c(-0.018386, 0.494253, 0.517650)
  F_35 <- c(0.028261, -0.428986, 1.410370)
  F_40 <- c(-0.025018, -0.710893, 1.800768)
  F_45 <- c(0.140871, -0.762433, 1.631598)
  F_50 <- c(-0.204015, 1.211073)
  
  # Vetores para a TEF - Tabela A3
  f1 <- c(0.204207, -0.006589)
  f2 <- c(0.259045, -0.009918, 0.007858, -0.205244)
  f3 <- c(0.207653, -0.000780, 0.000739, -0.196565)
  f4 <- c(0.167995, -0.000333, 0.000340, -0.171871, -0.158297, 0.161950)
  f5 <- c(0.166491, -0.000894, 0.000892, -0.166074, -0.155667, 0.155277)
  f6 <- c(0.182392, -0.002541, 0.002548, -0.182851, -0.259493, 0.260145)
  f7 <- c(0.199712, -0.008107, 0.008128, -0.200213, -1.525749, 1.529572)

  # Variável referente ao ponto médio. Isso será usado para o spline
  PM = as.numeric(ifelse(Age=="15-19", 17.5, 
                  ifelse(Age=="20-24", 22.5, 
                  ifelse(Age=="25-29", 27.5, 
                  ifelse(Age=="30-34", 32.5, 
                  ifelse(Age=="35-39", 37.5,
                  ifelse(Age=="40-44", 42.5, 
                  ifelse(Age=="45-49", 47.5, NA))))))))
  
  # Crianda a proporção de filhos mortos
  si = (FT - FS)/Mi
  
  # Criando SJ
  Base = data.frame(Base)
  
  id = seq(1:length(Base[,1])) # Isso só funciona para data.frame
  
  Sj = si
  
  for (i in id) {
    
    Sj[i] = ifelse(PM[i]==17.5, si[i],si[i] + Sj[i-1])
    
  }
  
  # Filhos nascidos mortos estimados
  Mult = Ai * (Sj ^ k)
  
  FNM_Est = Mi * Mult
  
  for (i in id) {
    
    FNM_Est[i] = ifelse(PM[i]==17.5, 
                        FNM_Est[i], 
                        Mi[i] * (Mult[i] - Mult[i-1]))
    
  }
  
  # Fihos tidos nascidos vivos estimados
  FTNV_Est = FT - FNM_Est 
  
  # Parturição
  Pi <- FTNV_Est / Mi
  
  # Fecundidade Acumulada (Fi)
  Fi <- as.numeric(ifelse(Age=="15-19", 1.200118, 
                   ifelse(Age=="20-24", 0.895876, 
                   ifelse(Age=="25-29", 1.012988, 
                   ifelse(Age=="30-34", 0.984019, 
                   ifelse(Age=="35-39", 0.868603,
                   ifelse(Age=="40-44", 0.998407, 
                   ifelse(Age=="45-49", 0.994680, NA))))))))
  
  for (i in id) {
    
    Fi[i] = ifelse(PM[i]==17.5, Fi[i] * (Pi[i] ^ F_20[1]) * (Pi[i+1] ^ F_20[2]) * (Pi[i+2] ^ F_20[3]) * (Pi[i+3] ^ F_20[4]), 
            ifelse(PM[i]==22.5, Fi[i] * (Pi[i] ^ F_25[1]) * (Pi[i+1] ^ F_25[2]) * (Pi[i+2] ^ F_25[3]),
            ifelse(PM[i]==27.5, Fi[i] * (Pi[i-1] ^ F_30[1]) * (Pi[i] ^ F_30[2]) * (Pi[i+1] ^ F_30[3]),
            ifelse(PM[i]==32.5, Fi[i] * (Pi[i-2] ^ F_35[1]) * (Pi[i-1] ^ F_35[2]) * (Pi[i] ^ F_35[3]),
            ifelse(PM[i]==37.5, Fi[i] * (Pi[i-4] ^ F_40[1]) * (Pi[i-2] ^ F_40[2]) * (Pi[i-1] ^ F_40[3]),
            ifelse(PM[i]==42.5, Fi[i] * (Fi[i-3] ^ F_45[1]) * (Fi[i-2] ^ F_45[2]) * (Fi[i-1] ^ F_45[3]),
            ifelse(PM[i]==47.5, Fi[i] * (Fi[i-2] ^ F_50[1]) * (Fi[i-1] ^ F_50[2]),NA)))))))
  }
  
  # Taxa Específica de Fecundidade (TEF)
  fi <- as.numeric(NA)
  
  for (i in id) {
    fi[i] = as.numeric(ifelse(PM[i]==17.5, Fi[i] * (f1[1] + f1[2]*Fi[i+6]),
                       ifelse(PM[i]==22.5, Fi[i] * (f2[1] + f2[2] * Fi[i+5]) + Fi[i-1] * (f2[3] * Fi[i+5] + f2[4]),
                       ifelse(PM[i]==27.5, Fi[i] * (f3[1] + f3[2] * Fi[i+4]) + Fi[i-1] * (f3[3] * Fi[i+4] + f3[4]),
                       ifelse(PM[i]==32.5, (Fi[i] * (f4[1] + f4[2] * Fi[i+3]) + Fi[i-1] * (f4[3] * Fi[i+3] + f4[4])) / (1 + f4[5] * Fi[i] + f4[6] * Fi[i-1]),
                       ifelse(PM[i]==37.5, (Fi[i] * (f5[1] + f5[2] * Fi[i+2]) + Fi[i-1] * (f5[3] * Fi[i+2] + f5[4])) / (1 + f5[5] * Fi[i] + f5[6] * Fi[i-1]),
                       ifelse(PM[i]==42.5, (Fi[i] * (f6[1] + f6[2] * Fi[i+1]) + Fi[i-1] * (f6[3] * Fi[i+1] + f6[4])) / (1 + f6[5] * Fi[i] + f6[6] * Fi[i-1]),
                       ifelse(PM[i]==47.5, (Fi[i] * (f7[1] + f7[2] * Fi[i]) + Fi[i-1] * (f7[3] * Fi[i] + f7[4])) / (1 + f7[5] * Fi[i] + f7[6] * Fi[i-1]), NA))))))))
  }
  
  # Base de dados final (estado - idade)
  Base = data.frame(PM = PM, Mulheres = Mi, FTNV_Est = FTNV_Est, 
                    Pi = Pi, Fi = Fi, fi = fi, FT = FT, FS = FS, 
                    Regiao_Rot = Regiao)
  
}


## Aplicando o modelos de Frias e Carvalho aos censos
RL <- list()

for (i in 1:8) {
  
  TEMP <- FO_TEF(DL[[i]], 
                DL[[i]]$Regiao_Rot,
                DL[[i]]$G_Idade, 
                DL[[i]]$Mulheres, 
                DL[[i]]$FT, 
                DL[[i]]$FS)
  
  L = length(RL) + 1
  
  RL[[L]] <- TEMP
 
}


## Renomeando os objetos na lista
NAMES <- names(DL)
names(RL) <- NAMES


## Criando a variável ano
RL[[1]]$Ano = 1933
RL[[2]]$Ano = 1943
RL[[8]]$Ano = 1953
RL[[3]]$Ano = 1963
RL[[4]]$Ano = 1973
RL[[5]]$Ano = 1983
RL[[6]]$Ano = 1993
RL[[7]]$Ano = 2003

## Aplicar a interpolação quinquenal
df <- do.call("rbind", RL)

## Preparando os dados para a interpolação
df <- df %>% arrange(Regiao_Rot, PM, Ano)

# Pegando os valores únicos de cada variável
uniq_reg = as.character(unique(df$Regiao_Rot))
uniq_age = unique(df$PM)

fi_Q <- data.frame(Regiao_Rot=NA, PM=NA, Ano=NA, fi=NA)

for(i in uniq_reg) {
  
  for(j in uniq_age) {
    
    Temp = subset(df, Regiao_Rot == i & PM == j)
    
    Temp2 = as.data.frame(rbind(Temp[, c("Regiao_Rot", "PM")], 
                                data.frame(Regiao_Rot = i, PM = j),
                                data.frame(Regiao_Rot = i, PM = j),
                                data.frame(Regiao_Rot = i, PM = j),
                                data.frame(Regiao_Rot = i, PM = j),
                                data.frame(Regiao_Rot = i, PM = j),
                                data.frame(Regiao_Rot = i, PM = j),
                                data.frame(Regiao_Rot = i, PM = j)))
    
    Temp3 = as.data.frame(spline(Temp$Ano, 
                                 Temp$fi, 
                                 xout = seq(1933, 2003, 5), 
                                 method = "natural"))
    colnames(Temp3) <- c('Ano', 'fi')
    
    fi_Q <- as.data.frame(rbind(fi_Q, 
                                cbind(Temp2, 
                                           Temp3)))
  }
}

fi_Q <- fi_Q[-1, ]

fi_Q <- fi_Q %>% arrange(Regiao_Rot, Ano, PM)


## Criando a TFT e a idade média
TFT = fi_Q %>%  
  group_by(Regiao_Rot, Ano) %>%
  summarise(TFT = 5 * sum(fi))

M = fi_Q %>%  
  group_by(Regiao_Rot, Ano) %>%
  summarise(M = (sum(fi * PM) / sum(fi)))

TFT_Final = merge(TFT, M, 
                  by = c("Regiao_Rot", "Ano"))

rm(list = setdiff(ls(), c('DL', 'RL', 'fi_Q', 'TFT_Final')))


###  Salvando a versão final
save.image("Dados/Dados_VF.RData")

write.xlsx(list(fi = fi_Q,
                TFT = TFT_Final), 'Tabelas/Tabela_F.xlsx',
           colNames=T,rowNames=F,overwrite =T)


