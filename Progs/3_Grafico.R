
####################################
#####                         ######
#####     ANÁLISE GRÁFICA     ######
#####                         ######
####################################

# Código atualizado em: 11-09-2020 
# Versão do R: 4.0.2 

## Carregando as bibliotecas necessárias
pkg <- c('openxlsx', 'ggplot2', 'plyr')

lapply(pkg, library, character.only = TRUE)

## Definição inicial
setwd('C:/Users/Guilherme Quaresma/OneDrive/Guilherme/Cursos/# Elaborados/TransFec')

## Carregando o arquivo necessário
load("Dados/Dados_VF.RData")


## TFT

# Geral
p <- ggplot(data = TFT_Final, aes(x=Ano, y=TFT, colour=Regiao_Rot)) +
  scale_y_continuous(limits = c(0, 9), 
                     breaks = seq(0, 9, 1)) +
  scale_x_continuous(limits = c(1930, 2005), 
                     breaks = seq(1930, 2005, 5)) +
  geom_line(aes(linetype=Regiao_Rot), size=0.8) +
  geom_point(aes(color=Regiao_Rot))+
  labs(x="Ano",
       y="TFT") +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"))

p

ggsave(p, file=paste0("Grafs/TFT.png"), width = 14, height = 10, units = "cm")

# Brasil
p <- ggplot(data = TFT_Final[TFT_Final$Regiao_Rot=='Brasil',], aes(x=Ano, y=TFT, colour=Regiao_Rot)) +
  scale_y_continuous(limits = c(0, 9), 
                     breaks = seq(0, 9, 1)) +
  scale_x_continuous(limits = c(1930, 2005), 
                     breaks = seq(1930, 2005, 5)) +
  geom_line(aes(linetype=Regiao_Rot), size=0.8) +
  geom_point(aes(color=Regiao_Rot))+
  labs(x="Ano",
       y="TFT") +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"))

p

ggsave(p, file=paste0("Grafs/TFT_Brasil.png"), width = 14, height = 10, units = "cm")

# Brasil - SP - RJ
p <- ggplot(data = TFT_Final[TFT_Final$Regiao_Rot=='Brasil' |
                               TFT_Final$Regiao_Rot=='Rio de Janeiro' | 
                               TFT_Final$Regiao_Rot=='São Paulo',], aes(x=Ano, y=TFT, colour=Regiao_Rot)) +
  scale_y_continuous(limits = c(0, 9), 
                     breaks = seq(0, 9, 1)) +
  scale_x_continuous(limits = c(1930, 2005), 
                     breaks = seq(1930, 2005, 5)) +
  geom_line(aes(linetype=Regiao_Rot), size=0.8) +
  geom_point(aes(color=Regiao_Rot))+
  labs(x="Ano",
       y="TFT") +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"))

p

ggsave(p, file=paste0("Grafs/TFT_Selecionado.png"), width = 14, height = 10, units = "cm")


## Idade Média

# Brasil - SP - RJ
p <- ggplot(data = TFT_Final[TFT_Final$Regiao_Rot=='Brasil' |
                               TFT_Final$Regiao_Rot=='Rio de Janeiro' | 
                               TFT_Final$Regiao_Rot=='São Paulo',], aes(x=Ano, y=M, colour=Regiao_Rot)) +
  scale_y_continuous(limits = c(25, 32), 
                     breaks = seq(24, 32, 2)) +
  scale_x_continuous(limits = c(1930, 2005), 
                     breaks = seq(1930, 2005, 5)) +
  geom_line(aes(linetype=Regiao_Rot), size=0.8) +
  geom_point(aes(color=Regiao_Rot))+
  labs(x="Ano",
       y="Idade Média") +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"))

p

ggsave(p, file=paste0("Grafs/M_Selecionado.png"), width = 14, height = 10, units = "cm")


## TFT x M

# Brasil - SP - RJ
p <- ggplot(data = TFT_Final[TFT_Final$Regiao_Rot=='Brasil' |
                               TFT_Final$Regiao_Rot=='Rio de Janeiro' | 
                               TFT_Final$Regiao_Rot=='São Paulo',], aes(x=M, y=TFT, colour=Regiao_Rot)) +
  scale_y_continuous(limits = c(0, 9), 
                     breaks = seq(0, 9, 1)) +
  scale_x_continuous(limits = c(25, 32), 
                     breaks = seq(24, 32, 2)) +
  geom_line(aes(linetype=Regiao_Rot), size=0.8) +
  geom_point(aes(color=Regiao_Rot))+
  labs(x="Idade Média",
       y="TFT") +
  theme(plot.title = element_text(face = "bold", size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks = element_line(colour = "grey70", size = 0.2),
        panel.grid.major = element_line(colour = "grey90", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "grey50"))

p

ggsave(p, file=paste0("Grafs/TFT_M_Selecionado.png"), width = 14, height = 10, units = "cm")

