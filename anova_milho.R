#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#
# ANOVA1: Análise de Variância a 1 FATOR ANOVA
# Exemplo 1: Dados de Milho
# Autor: Prof. Evaldo Martins
#
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

Tr = chickwts %>% group_by(feed) %>% summarise(n=length(weight), 
																							 med=mean(weight), 
																							 dp=sd(weight), 
																							 s2= var(weight),  
																							 se=sd(weight)/length(weight))



#    Ajustando o ambiente, carregaendo principais pacotes, --------------
# setwd("C:/Users/odlav/Dropbox/Scripts/Aulas/EP_Geral/ANOVA")

#    Carrregando dados e resumo os cálculo de estatísticas por grupos 

#  Ajustando o ambiente: 
rm(list = ls())     # Remove todos os objetos do ambiente
dev.off()           # Limpa todos os gráficos
cat("\014")         # Limpa o console

library(readxl)     # Leitura de dados do excel
library(agricolae)  # realiza teste estatísticos e delinemanetos de experimentos
library(ggplot2)    # Faz gráficos de ótima qualidade e por camadas
library(dplyr)      # manipula planilha de dados
library(readr)      # leitura de dados de forma eficiente para tibble
library(psych)

#   Lendo os dados a partir de planilha do excel ou arquivo csv
#   Recomendação: salvar seu arquivo de dados como csv - UTF-8 via MS - Excel
    milho = read_csv2("dados/milho_var.csv", col_types = 'fd')
    glimpse(milho)
    psych::headTail(milho, 6, 6)

# Calculando várias estatísticas dos dados gerais
SUMARIO1 = milho %>% group_by() %>% summarise(N=length(Prod), 
                                              MED=mean(Prod), 
                                              DP=sd(Prod), 
                                              S2= var(Prod),  
                                              SE=sd(Prod)/length(Prod))
SUMARIO1

# Calculando as estatísticas por tratamento (Por variedade de milho: A, B, C e D) 
sumario = milho |> group_by (Var) |> summarise(n= length(Prod))

sumario1 = milho %>% group_by(Var) %>% summarise(n=length(Prod), 
                                                 med=mean(Prod), 
                                                 dp=sd(Prod), 
                                                 s2= var(Prod),  
                                                 se=sd(Prod)/length(Prod))
sumario1

## Colocando em ordem crescente médias e variâncias: 
sort(sumario1$med)
sort(sumario1$s2)


# Construçãoo dos gráficos para visualizaçãoo a respeito dos dados ----------------

# Localização das médias de tratamentos e relação e media geral: 
par(mai=c(0.5, 1, .2, .2))
plot.design(data = milho, Prod ~ Var, xlab="", ylab="", 
            las=1, bty='l', col='blue')
mtext('Produçãoo média de milho (ton))', side=2, line=3.5)

 # Gráfico de Caixas e bigodes: 
par(mai=c(1., 1.1, .2, .2)) #, mfrow=c(1,2))
require(car)
Boxplot(data = milho, Prod ~ Var, names=LETTERS[1:4], ylab="Produção (ton)",
        xlab="Variedades de Milho", las=1, col='LightGreen', bty = "l")
points(sumario1$med, pch="+", col=2, cex=1.5)

# stripchart é uma alternativa ao boxplot para amostras de tamanho pequeno.

stripchart(data = milho, Prod ~ Var, 
           method="jitter", vertical = TRUE, las=1, pch=1, col='blue', 
           
           group.names=LETTERS[1:4],
           xlab='Variedades de Milho', 
           ylab='Produções de Milho (ton)')

points(sumario1$med, pch = 19, cex = 1.3, col='red') # Adiciona pontos

arrows(1:4, sumario1$med + 2*sumario1$se, 1:4, sumario1$med - 2*sumario1$se, angle = 90,
       code = 3, length = 0.1, col='red')

# 6.3) Procedemos ? an?lise de variância, Avaliação das Premissas da ANOVA : ----------
#  Normalidade dos residuos, homoscedasticidade

# Realizando a análise de variância com o comando aov
mod1 = aov(data = milho, Prod ~ Var)
anova(mod1)  # Mostra a tabela anova

# Para verificarmos se os ERROS tem distribuição normal: 
# Gráfico Normal QQ
par(mai = c(1, 1, .3, .2))
plot(mod1, which=c(2:2), pch=19, col='red', las=1)

# Teste de Shapiro e teste de Lilliefors (Kolmogorov-Smirnov)
# Teste de Shapiro-Wilk para normalidade dos erros
shapiro.test(mod1$residuals)
# Teste de Kolmogov-Smirnov para normalidade dos erros
require(nortest)
lillie.test(mod1$res)

# Para verificar se as vari?ncias s?o homog?neas 
# aplicamos o TESTE DE BARTLETT           
bartlett.test(mod1$res ~ Var)

# SE os erros N?O SEGUEM uma distribuição normal 
# uma alternativa ao teste de Bartlett é o teste de Levene.           -

# É preciso carregar o pacote car
require(car)
leveneTest(Prod ~ Var, center=mean)
leveneTest(Prod ~ Var, center=median)

# 6.4) Comparando as M?DIAS -----------------------------

#  Teste de HSD
comp1.HSD =  HSD.test(mod1,"Var", alpha=0.05, group=TRUE)
comp1.HSD$statistics; comp1.HSD$groups 
comp1.HSD

#  Teste de Tukey
comp1.tukey = HSD.test(mod1, 'Var', main='Produção de Milho (ton)', console=T)
comp1.tukey$statistics; comp1.tukey$groups
comp1.tukey
names(comp1.tukey)

#  Fazendo um comparação visual (gráfica)

plot(comp1.HSD, ylim=c(25,40), las=1, ylab="Produção de milho (ton)", xlab="Variedades de milho", cex.lab= 1.3,
     cex.axis=1.3, font.lab=2, font.axis=2)

par(mai=c(1, 2.3, .7, .2))
plot(comp1.tukey, las=1, col='blue')

# 6.5 Comparação ssando o pacote agricolae----------------------------      

library(agricolae)
# Teste Tukey 
teste.HSD1 = HSD.test(mod1, 'Var', main='Produção de Milho', console=T)
names(teste.HSD1)

par(mai=c(1, 1.1, .7, .2))
bar.group(teste.HSD1$groups, ylim=c(0,40), density=10, border="blue",
          las=1, angle=45, col='red', main='Teste de Tukey',
          xlab='Tipos de variedade', ylab='Produção (ton)')
abline(h=0, col='black', lwd=1.9)

# 6.6) Grafico de barras, com barra de erro padr?o (para publicar) ----------------

### Teste de comparação de m?dia
HSD_A = HSD.test(y = Prod,
                 trt = Var,
                 DFerror = mod1$df.residual,
                 MSerror = deviance(mod1)/mod1$df.residual,
                 alpha = 0.05,
                 group = TRUE,
                 console = TRUE)

# Ordem crescente de grupos para letras no gr?fico de barras
## Ordem crescente do HSD$group para o fator A
ascend_A = HSD_A$groups %>%
  group_by(rownames(HSD_A$groups)) %>%
  arrange(rownames(HSD_A$groups))
  print(ascend_A)

# Visualizando efeitos ------------------------------------------

# Obtendo a m?dia e SE como dataframe

## Efeitos principais
### M?dia e SE para o primeiro fator (Fator A)
MeanSE_A = milho %>%
  group_by(Var) %>%
  summarise(avg_A = mean(Prod),
            se = sd(Prod)/sqrt(length(Prod)))
attach(MeanSE_A)
print(MeanSE_A)

# Plotando os efeitos principais -------------------------------------

## Para o primeiro fator (Fator A = Var) -----------------------
p1 = ggplot(MeanSE_A, aes(x = Var, 
                          y = avg_A))
print(p1)

p1 = ggplot(MeanSE_A, aes(x = Var,
                          y = avg_A,
                          fill = Var))
print(p1)
# Adicionando camadas ao objeto p1
pA = p1 + 
  
  # Plotando barras
  geom_bar(stat = "identity",
           color = "black",
           position = position_dodge(width=0.9),
           width = 0.8) +
  # Mudando a escala de cor de preenchimento
  scale_fill_manual(values=gray(1:4/4))  +
  
  # Adicionando barras de erros      
  geom_errorbar(aes(ymax = avg_A + se,
                    ymin = avg_A - se), 
                position = position_dodge(width=0.9), 
                width = 0.25) +
  # Alterar t?tulo principal, r?tulos X e Y
  labs(title = "Efeito da Variedade sobre a Produção",
       x = "Variedade",
       y = "Produção(ton)") +
  
  # Adicionando letras do teste aplicado (HSD$group)
  geom_text(aes(x = Var,
                y = avg_A + se,
                label = as.matrix(ascend_A$groups)),
            position = position_dodge(width = 0.9),
            vjust = -(0.5))  +        # vjust = 1 (top) 
  theme_classic()

print(pA)


