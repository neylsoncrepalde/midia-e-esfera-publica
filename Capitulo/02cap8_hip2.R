#### Dados Cap8
#### Hipótese 2
#### Script 2 - Dados completos
#### Script: Neylson Crepalde
#############################

library(readxl)
library(dplyr)
library(descr)
dados = read_excel("Artigo_Sitema_Banco_dedos_Completo.xlsx")
names(dados)

#----------------------------------------------
### HIPÓTESE 2 - Haverá maior ocorrência de justificação onde tem maior 
### incidência de desacordo
#----------------------------

# Duas abordagens:
# 1) Analisar os dados como estão. Y = Complexa
# 2) Criar uma variável numérica Justificação - 
#    Complexa = 2, simples = 1, opinião = 0

#----------------------------
# Abordagem 1 - Dados como estão. Y = Just. Complexa [20]

# Testando apenas a hipótese

jus_des_total = dados %>% select(1:7, 12:14, 20:22) %>%
  mutate(desacordo = case_when(
    `D01-Bold` == 1 ~ 'bold',
    `D02-Soft` == 1 ~ 'soft',
    `D03-Absence` == 1 ~ 'absence'
  )) %>%
  mutate(justificacao = case_when(
    `J01-Complexa` == 1 ~ 'complexa',
    `J02-Simples` == 1 ~ 'simples',
    `J03-Opiniao` == 1 ~ 'opiniao'
  )) %>% 
  mutate(desac_num = case_when(
    `D01-Bold` == 1 ~ 2,
    `D02-Soft` == 1 ~ 1,
    `D03-Absence` == 1 ~ 0
  )) %>%
  mutate(just_num = case_when(
    `J01-Complexa` == 1 ~ 2,
    `J02-Simples` == 1 ~ 1,
    `J03-Opiniao` == 1 ~ 0
  ))

freq(jus_des_total$desacordo, plot=F) # Divergência de 4 casos em absence
freq(jus_des_total$`D03-Absence`, plot = F)

freq(jus_des_total$justificacao, plot = F) # Divergência de 1 caso em opiniao
freq(jus_des_total$`J03-Opiniao`, plot = F)

# Testando a relação entre a justificacao e o desacordo
table(jus_des_total$justificacao, jus_des_total$desacordo)
summary(table(jus_des_total$justificacao, jus_des_total$desacordo)) # São dependentes

# Fazendo um teste de correlação
cor.test(jus_des_total$just_num, jus_des_total$desac_num) # Não sig. Corr baixíssima.


