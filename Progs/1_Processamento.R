
####################################
#####                         ######
##### PROCESSAMENTO DOS DADOS ######
#####                         ######
####################################

# Código atualizado em: 11-09-2020 
# Versão do R: 4.0.2

## Carregando as bibliotecas necessárias

#install.packages('openxlsx')

pkg <- c('openxlsx', 'data.table', 'dplyr')
lapply(pkg, library, character.only = TRUE)



####    FUNÇÃO PARA O PROCESSAMENTO    ####
PREP_C_40_50 <- function(Dados) {
  
  Dados$G_Idade <- as.character(ifelse(Dados$Idade>=15 & Dados$Idade<=19, "15-19", 
                                ifelse(Dados$Idade>=20 & Dados$Idade<=24, "20-24", 
                                ifelse(Dados$Idade>=25 & Dados$Idade<=29, "25-29", 
                                ifelse(Dados$Idade>=30 & Dados$Idade<=34, "30-34", 
                                ifelse(Dados$Idade>=35 & Dados$Idade<=39, "35-39", 
                                ifelse(Dados$Idade>=40 & Dados$Idade<=44, "40-44", 
                                ifelse(Dados$Idade>=45 & Dados$Idade<=49, "45-49", NA))))))))
  
  Dados$Regiao_Rot <- as.character(ifelse(Dados$Regiao==1, "Amazônia", 
                                   ifelse(Dados$Regiao==2, "Nordeste Setentrional", 
                                   ifelse(Dados$Regiao==3, "Nordeste Central", 
                                   ifelse(Dados$Regiao==4, "Nordeste Meridional", 
                                   ifelse(Dados$Regiao==5, "Leste", 
                                   ifelse(Dados$Regiao==6, "Rio de Janeiro",
                                   ifelse(Dados$Regiao==7, "São Paulo", 
                                   ifelse(Dados$Regiao==8, "Paraná", 
                                   ifelse(Dados$Regiao==9, "Extremo Sul", 
                                   ifelse(Dados$Regiao==10, "Centro-Oeste", "?????")))))))))))
  
  Agrega <- Dados %>% 
    group_by(G_Idade) %>%
    summarise(Mulheres = sum(Mulheres), 
              FT = sum(FT), 
              FS = sum(FS), 
              Regiao_Rot="Brasil")
  
  var <- c("Regiao_Rot", "G_Idade", "Mulheres", "FT", "FS")
  
  Agrega <- Agrega[, var]
  
  Agrega2 <- Dados %>% 
    group_by(Regiao_Rot, G_Idade) %>%
    summarise(Mulheres = sum(Mulheres), 
              FT = sum(FT), 
              FS = sum(FS))
  
  Base = data.table(rbind(data.table(Agrega), data.table(Agrega2)))
  
}  

PREP_C <- function(Regiao, Idade, Peso, FT, FS) {
  
  G_Idade <- as.character(ifelse(Idade>=15 & Idade<=19, "15-19", 
                          ifelse(Idade>=20 & Idade<=24, "20-24", 
                          ifelse(Idade>=25 & Idade<=29, "25-29", 
                          ifelse(Idade>=30 & Idade<=34, "30-34", 
                          ifelse(Idade>=35 & Idade<=39, "35-39", 
                          ifelse(Idade>=40 & Idade<=44, "40-44", 
                          ifelse(Idade>=45 & Idade<=49, "45-49", NA))))))))
  
  Regiao_Rot <- as.character(ifelse(Regiao>=11 & Regiao<=16, "Amazônia", 
                             ifelse(Regiao==21 | Regiao==22, "Nordeste Setentrional", 
                             ifelse((Regiao>=23 & Regiao<=27) | Regiao==20, "Nordeste Central", 
                             ifelse(Regiao==28 | Regiao==29, "Nordeste Meridional", 
                             ifelse(Regiao==31 | Regiao==32, "Leste", 
                             ifelse(Regiao==33 | Regiao==34, "Rio de Janeiro",
                             ifelse(Regiao==35, "São Paulo", 
                             ifelse(Regiao==41, "Paraná", 
                             ifelse(Regiao==42 | Regiao==43, "Extremo Sul", 
                             ifelse((Regiao>=50 & Regiao<=53) | Regiao==17, "Centro-Oeste", "?????")))))))))))
  
  FS_Corr = ifelse(FS<99, FS, NA)
  
  Dados = data.table(Regiao_Rot, G_Idade, Peso, FT, FS_Corr)
  
  Dados <- Dados[!is.na(Dados$FT) & !is.na(Dados$FS_Corr), ]
  
  Agrega <- Dados %>% 
    group_by(G_Idade) %>%
    summarise(Mulheres = sum(Peso),
              FT = sum(FT * Peso), 
              FS = sum(FS_Corr * Peso), 
              Regiao_Rot="Brasil")
  
  var <- c("Regiao_Rot", "G_Idade", "Mulheres", "FT", "FS")
  
  Agrega <- Agrega[, var]
  
  Agrega2 <- Dados %>% 
    group_by(Regiao_Rot, G_Idade) %>%
    summarise(Mulheres = sum(Peso), 
              FT = sum(FT * Peso), 
              FS = sum(FS_Corr * Peso))
  
  Base = data.table(rbind(data.table(Agrega), data.table(Agrega2)))
  
}  


#####     1940 & 1950     ######

# Base de dados para os anos de 1940 e 1950
FilePath <- "C:/Users/Guilherme Quaresma/OneDrive/Guilherme/Doutorado/Tese/Dados/Dados.xlsx"

# Aplicando a função
Data_1940 <- read.xlsx(xlsxFile = FilePath, sheet = 1) 
Data_1950 <- read.xlsx(xlsxFile = FilePath, sheet = 2) 

# Insumos para a técnica
Insumos40 <- PREP_C_40_50(Data_1940)
Insumos50 <- PREP_C_40_50(Data_1950)

# Valor do ano
Insumos40$Ano_C = 1940
Insumos50$Ano_C = 1950

# Mantendo somente os objetos de interesse
DL <- list('1940'=Insumos40, '1950'=Insumos50) 
summary(DL)

rm(list = setdiff(ls(), c('DL', 'PREP_C')))


#####     1970     ######

# Abrindo a base de dados
myvar <- c("UF", "V004", "V023", "V025", "V026", "V027", "V050", "V051", "V053", "V054")

setwd("C:/Users/Guilherme Quaresma/OneDrive/Dados")

Data_1970 <- as.data.frame(fread("Censos/1970/Dados/Censo1970_PessDom.csv", 
                                 select = myvar))

# Mantendo somente mulheres em idade reprodutiva
Data_1970 <- Data_1970[Data_1970$V023==1 & (Data_1970$V027>=15 & Data_1970$V027<=49), ]

# Ajsute de algumas variáveis
Data_1970$FTNV <- ifelse(Data_1970$V050<99, Data_1970$V050, NA)

Data_1970$FTNM <- ifelse(Data_1970$V051<9, Data_1970$V051, NA)

Data_1970$FT = Data_1970$FTNV + Data_1970$FTNM

# Insumos para a técnica
Insumos70 <- PREP_C(Data_1970$UF, Data_1970$V027, Data_1970$V054, Data_1970$FT, Data_1970$V053)

# Valor do ano
Insumos70$Ano_C = 1970

# Mantendo somente os objetos de interesse
DL$'1970' <- Insumos70
summary(DL)

rm(list = setdiff(ls(), c('DL', 'PREP_C')))


#####     1980     ######

# Abrindo a base de dados
myvar <- c("V002", "V598", "V501", "V606", "V550", "V551", "V552", "V553", "V554", "V555", "V604")

Data_1980 <- as.data.frame(fread("Censos/1980/Dados/Censo1980_PessDom.csv", select = myvar))

# Mantendo somente mulheres em idade reprodutiva
Data_1980 <- Data_1980[Data_1980$V501==3 & (Data_1980$V606>=15 & Data_1980$V606<=49), ]

# Ajsute de algumas variáveis
attach(Data_1980)
Data_1980$FT <- as.numeric(ifelse(V550!=99 & V551!=99 & V552!=99 & V553!=99, (V550 + V551 + V552 + V553), NA))
Data_1980$FS <- as.numeric(ifelse(V554!=99 & V555!=99, (V554 + V555), NA))
detach(Data_1980)

# Insumos para a técnica
Insumos80 <- PREP_C(Data_1980$V002, Data_1980$V606, Data_1980$V604, Data_1980$FT, Data_1980$FS)

# Valor do ano
Insumos80$Ano_C = 1980

# Mantendo somente os objetos de interesse
DL$'1980' <- Insumos80
summary(DL)

rm(list = setdiff(ls(), c('DL', 'PREP_C')))


#####     1991     ######

# Abrindo a base de dados
myvar <- c("V1101", "V1061", "V3072", "V0301", "V3354", "V3357", "V3360", "V7301a", "V3351")

Data_1991 <- as.data.frame(fread("Censos/1991/Dados/Censo1991_PessDom.csv", select = myvar))

# Mantendo somente mulheres em idade reprodutiva
Data_1991 <- Data_1991[Data_1991$V0301==2 & (Data_1991$V3072>=15 & Data_1991$V3072<=49), ]

# Insumos para a técnica
Insumos91 <- PREP_C(Data_1991$V1101, Data_1991$V3072, Data_1991$V7301a, Data_1991$V3351, Data_1991$V3360)

# Valor do ano
Insumos91$Ano_C = 1991

# Mantendo somente os objetos de interesse
DL$'1991' <- Insumos91
summary(DL)

rm(list = setdiff(ls(), c('DL', 'PREP_C')))


#####     2000     ######

# Abrindo a base de dados
myvar <- c("V1006", "V4752", "V0102", "V0401", "V4690", "V0463", "PES_PESSOA")

Data_2000 <- as.data.frame(fread("Censos/2000/Dados/censo2000_BRpes.csv", select = myvar))

# Mantendo somente mulheres em idade reprodutiva
Data_2000 <- Data_2000[Data_2000$V0401==2 & (Data_2000$V4752>=15 & Data_2000$V4752<=49), ]

# Ajsute de algumas variáveis
Data_2000$FS <- ifelse(Data_2000$V4690>0, Data_2000$V0463, 0)

# Insumos para a técnica
Insumos00 <- PREP_C(Data_2000$V0102 ,Data_2000$V4752, Data_2000$PES_PESSOA, Data_2000$V4690, Data_2000$FS)

# Valor do ano
Insumos00$Ano_C = 2000

# Mantendo somente os objetos de interesse
DL$'2000' <- Insumos00
summary(DL)

rm(list = setdiff(ls(), c('DL', 'PREP_C')))


#####     2010     ######

# Abrindo a base de dados
myvar <- c("V1006", "V6033", "V0001", "V0601", "V6800", "V6643", "V0010")

Data_2010 <- as.data.frame(fread("Censos/2010/Dados/censo2010_BRpes.csv", select = myvar))

# Mantendo somente mulheres em idade reprodutiva
Data_2010 <- Data_2010[Data_2010$V0601==2 & (Data_2010$V6033>=15 & Data_2010$V6033<=49), ]

# Ajsute de algumas variáveis
Data_2010$FS <- ifelse(is.na(Data_2010$V6643), 0,  Data_2010$V6643)

# Insumos para a técnica
Insumos10 <- PREP_C(Data_2010$V0001, Data_2010$V6033, Data_2010$V0010, Data_2010$V6800, Data_2010$FS)

# Valor do ano
Insumos10$Ano_C = 2010

# Mantendo somente os objetos de interesse
DL$'2010' <- Insumos10
summary(DL)

rm(list = setdiff(ls(), c('DL', 'PREP_C')))


#####     1960     ######
df <- do.call("rbind", DL)

## Preparando os dados para a interpolação
df <- df %>% 
  filter(Ano_C>=1940 & Ano_C<=1980) %>% 
  arrange(Regiao_Rot, G_Idade, Ano_C)


## Interpolação dos dados

# Pegando os valores únicos de cada variável
uniq_reg = unique(df$Regiao_Rot) 
uniq_age = unique(df$G_Idade)

# Gereando a interpolação para os Filhos Sobreviventes
Insumos60 <- data.frame(Regiao_Rot=NA, G_Idade=NA, Ano=NA,
                        FS=NA, FT=NA, Mulheres=NA)

for(i in uniq_reg) {
  
  for(j in uniq_age) {
    
    Temp = subset(df, Regiao_Rot == i & G_Idade == j)
    
    Temp2 = as.data.frame(rbind(Temp[, c("Regiao_Rot", "G_Idade")], 
                                data.frame(Regiao_Rot = i, 
                                           G_Idade = j)))
    
    Temp3 = as.data.frame(spline(Temp$Ano_C, 
                                 Temp$FS, 
                                 xout = seq(1933, 1973, 10), 
                                 method = "natural"))
    colnames(Temp3) <- c('Ano', 'FS')
    
    Temp4 = as.data.frame(spline(Temp$Ano_C, 
                                 Temp$FT, 
                                 xout = seq(1933, 1973, 10), 
                                 method = "natural"))
    colnames(Temp4) <- c('Ano', 'FT')
    
    Temp5 = as.data.frame(spline(Temp$Ano_C, 
                                 Temp$Mulheres, 
                                 xout = seq(1933, 1973, 10), 
                                 method = "natural"))
    colnames(Temp5) <- c('Ano', 'Mulheres')
    
    Temp3 <- Reduce(function(x, y) merge(x, y, by = c('Ano'), all=TRUE), 
                    list(Temp3, Temp4, Temp5))
    
    Insumos60 <- as.data.frame(rbind(Insumos60, 
                                     cbind(Temp2, 
                                           Temp3)))
  }
}

Insumos60 <- Insumos60 %>% filter(!is.na(Regiao_Rot) & Ano==1953)

# Valor do ano
Insumos60$Ano_C = 1960
Insumos60 <- Insumos60[,-3]

# Mantendo somente os objetos de interesse
DL$'1960' <- Insumos60
summary(DL)

rm(list = setdiff(ls(), c('DL', 'PREP_C')))


###  Salvando a versão final
setwd('C:/Users/Guilherme Quaresma/OneDrive/Guilherme/Cursos/# Elaborados/TransFec/')

save.image("Dados/PreProcessamento.RData")

write.xlsx(DL, 'Dados/PreProcessamento.xlsx',
           colNames=T,rowNames=F,overwrite =T)



