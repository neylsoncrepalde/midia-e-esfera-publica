#### Dados Cap8
#### Hipótese 2
#### Script 2 - Dados completos
#### Script: Neylson Crepalde
#############################

library(readxl)
library(dplyr)
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

jus_des_total = 



