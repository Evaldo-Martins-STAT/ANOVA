# Carregamento de pacotes

# Carregando os pacotes
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)
library(fdrtool)
library(emmeans)
library(dunn.test)
library(rstatix)
library(DescTools) 
library(readxl)
library(plotrix) # Para cálculo do stadard error
library(Rmisc)

std_mean <- function(x) sd(x)/sqrt(length(x))
ci.pos <- function(x) {
  a = Rmisc::CI(x)
  ci.pos = a[2]-a[3]
  print(ci.pos)
  }

# Leitura e visualização dos dados

NaCl_DNA_Concentração <- read_excel("dados/NaCl DNA Concentração.xlsx")

View(NaCl_DNA_Concentração)
NaCl_DNA_Concentração=NaCl_DNA_Concentração[-20,]

str(NaCl_DNA_Concentração)

# Geração de um simples plot para visualização de dados
qplot(x = Tissue, y = DNA_Concentration, geom = "point", data = NaCl_DNA_Concentração) +
  facet_grid(.~Storage, labeller = label_both)

# Criando a variável como fator para ANOVA

NaCl_DNA_Concentração$Storage <- as.factor(NaCl_DNA_Concentração$Storage)
NaCl_DNA_Concentração$Tissue_Factor <- as.factor(NaCl_DNA_Concentração$Tissue)
str(NaCl_DNA_Concentração)

# Análise de variância

anova <- aov(DNA_Concentration ~ Storage*Tissue_Factor, data = NaCl_DNA_Concentração)
summary(anova)

# Teste de normalidade para os resíduos
shapiro.test(anova$residuals)

# Tabela com fatores, médias e desvio padrão
data_summary <- group_by(NaCl_DNA_Concentração, Tissue_Factor, Storage) %>%
  summarise(mean=round(mean(DNA_Concentration), 1), min=min(DNA_Concentration), 
            max=max(DNA_Concentration), sd= round(sd(DNA_Concentration),1), 
            se = std_mean(DNA_Concentration), ci = round(ci.pos(DNA_Concentration),1)) %>% arrange(desc(mean))
print(data_summary)


# Teste de Tukey
tukey <- TukeyHSD(anova)
print(tukey)

# Teste FDR
#PostHocTest(aov(DNA_Concentration ~ Storage*Tissue, data = NaCl_DNA_Concentração), method = "hsd",
#            conf.level=0.95)

# Post hoc - Teste de Sidak

NaCl_DNA_Concentração %>% group_by(Storage) %>% 
  emmeans_test(DNA_Concentration ~ Tissue, p.adjust.method = "sidak")

# Criando uma apresentação com letras compactas
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

# Adicionando letras compactas na tabela com médias e desvio padrão
cld <- as.data.frame.list(tukey.cld$`Storage:Tissue_Factor`)
data_summary$Tukey <- cld$Letters

# Adicionando letras compactas a partir de Sidak
data_summary$Sidak <- cld$Letters
print(data_summary)

# Exportar o sumário como planilha csv
write.csv("NaCl_DNA_Concentração_summary.csv")

# scatterplot
ggplot(data_summary, aes(Tissue_Factor, mean)) + 
  geom_point()

# scatterplot com cores
ggplot(data_summary, aes(Tissue_Factor, mean, color = Storage)) + 
  geom_point(aes(shape = Storage))

# scatterplot com cores e linhas
ggplot(data_summary, aes(Tissue_Factor, mean, color = Storage)) + 
  geom_point(aes(shape = Storage)) +
  geom_line(aes(linetype = Storage, group = Storage))

# Configurando a largura do scatterplot
ggplot(data_summary, aes(Tissue_Factor, mean, color = Storage)) + 
  geom_point(aes(shape = Storage), position=position_dodge(width=0.5)) +
  geom_line(aes(linetype = Storage, group = Storage), position=position_dodge(width=0.5))

# Adicionando barras de desvio padrão
ggplot(data_summary, aes(Tissue_Factor, mean, color = Storage)) + 
  geom_point(aes(shape = Storage), position=position_dodge(width=1)) +
  geom_line(aes(linetype = Storage, group = Storage), position=position_dodge(width=1)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=1))

# Configurando as barras dos erros 
ggplot(data_summary, aes(Tissue_Factor, mean, color = Storage)) + 
  geom_point(aes(shape = Storage), position=position_dodge(width=1)) +
  geom_line(aes(linetype = Storage, group = Storage), position=position_dodge(width=1)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=1), 
                width = 1, show.legend = FALSE)

# Configurando os títulos dos eixos
ggplot(data_summary, aes(Tissue_Factor, mean, color = Storage)) + 
  geom_point(aes(shape = Storage), position=position_dodge(width=1)) +
  geom_line(aes(linetype = Storage, group = Storage), position=position_dodge(width=1)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=1), 
                width = 1, show.legend = FALSE) +
  labs(x="Tissue", y="DNA Concentration (ng/µl)")

# Retirando a grade e mudando paleta de cores
ggplot(data_summary, aes(Tissue_Factor, mean, color = Storage)) + 
  geom_point(aes(shape = Storage), position=position_dodge(width=1)) +
  geom_line(aes(linetype = Storage, group = Storage), position=position_dodge(width=1)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=1), 
                width = 1, show.legend = FALSE) +
  labs(x="Tissue", y="DNA Concentration (ng/µl)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.1, 0.7))

# Adicionando letras compactas 
ggplot(data_summary, aes(Tissue_Factor, mean, color = Storage)) + 
  geom_point(aes(shape = Storage), position=position_dodge(width=1)) +
  geom_line(aes(linetype = Storage, group = Storage), position=position_dodge(width=1)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=1), 
                width = 1, show.legend = FALSE) +
  labs(x= "Tissue", y="DNA Concentration (ng/µl)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.9, 0.8)) +
  geom_text(aes(label=Tukey), size = 4, 
            position = position_dodge(width=1), vjust=1.5, show.legend = FALSE)

# Configurando os limites dos eixos
ggplot(data_summary, aes(Tissue_Factor, mean, color = Storage)) + 
  geom_point(aes(shape = Storage), position=position_dodge(width=1)) +
  geom_line(aes(linetype = Storage, group = Storage), position=position_dodge(width=1)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=1), 
                width = 1, show.legend = FALSE) +
  labs(x="Tissue", y="DNA Concentration (ng/µl)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.9, 0.8)) +
  geom_text(aes(label=Tukey), size = 4, 
            position = position_dodge(width=1), vjust=1.5, show.legend = FALSE) +
  scale_y_continuous(limits=c(-55, 300), breaks=seq(0,300,50)) 

# Modificando o tema do gráfico
ggplot(data_summary, aes(Tissue_Factor, mean, fill = Storage)) + 
  geom_col(aes(shape = Storage), position=position_dodge(width=1), alpha=0.4, size = 1)+
  # geom_point(aes(shape = Storage), position=position_dodge(width=1), alpha=0.4, size = 4) +
  # geom_line(aes(linetype = Storage, group = Storage), position=position_dodge(width=1)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position=position_dodge(width=1), 
                width = 0.5, show.legend = TRUE) +
  labs(x="", y="DNA Concentration (ng/µl)", size = 14) +
  theme_classic() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  
  
  theme(legend.position = c(0.10, 0.85)) +
  geom_text(aes(label=Sidak, y=mean+se), size = 4, 
            position = position_dodge(width=1), vjust=-1.9, hjust= -1.5, show.legend = FALSE) +
  geom_text(aes(label=mean, y=mean+se), size = 4, 
            position = position_dodge(width=1), vjust=-0.5, hjust= 1.3,  show.legend = FALSE) +
  geom_text(aes(label= "±", y=mean+se), size = 4, 
            position = position_dodge(width=1), vjust=-0.5, show.legend = FALSE) +
  geom_text(aes(label=sd, y=mean+se), size = 4, 
            position = position_dodge(width=1), vjust=-0.5, hjust= -0.3,  show.legend = FALSE) +
   
  # scale_x_continuous(breaks=c(100,125,150)) +
  scale_y_continuous(limits=c(-1, 250), breaks=seq(0,300,50)) +
  #scale_color_brewer(palette = "Set1") +
  # scale_fill_manual(values=c("#D02090", "#1E90FF")) +
  #scale_fill_manual(values=c("gray10", "grey60")) +
  theme(axis.text.x = element_text(size = 14, colour = "black", family = "sans")) +
  theme(axis.text.y = element_text(size = 14, colour = "black", family = "sans")) +
  theme(axis.title.y = element_text(size = 14, colour = "black")) +
  theme(axis.title.y = element_text(vjust = 0.9))
  
# Salvando a figura final
ggsave("Saline_NaCl_DNA_Concentration.tiff", width = 6, height = 3.8, dpi = 1000)
