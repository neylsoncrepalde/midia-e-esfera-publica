##############################
# Mídia e Esfera Pública
# Rousiley, Gabriella Hauber
# Script: Neylson Crepalde
##############################

setwd('~/Documentos/Rousiley')
list.files()

library(xlsx)

dados = read.xlsx("Nova_codificacao.xlsx",1)
View(dados)

nomes = names(dados)
nomes
nomes2 = gsub("X", "", nomes)
nomes2

names(dados) = nomes2
###########################################

library(reshape2)
library(dplyr)
dados$ocasiao = ifelse(dados$PH == 1, "Public Hearing", "Meeting")
names(dados)
emocoes = dados %>% select(1:6)
argumentos = dados %>% select(c(1, 14:37))
objetos = dados %>% select(c(1, 7:13))
names(argumentos)

emocoes_molten = melt(emocoes, id=1)
names(emocoes_molten)[2] = "emocao"
emocoes_molten = emocoes_molten %>% filter(value==1) %>% select(-value)

argumentos_molten = melt(argumentos, id=1)
names(argumentos_molten)[2] = "argumento"
argumentos_molten = argumentos_molten %>% filter(value==1) %>% select(-value)

objetos_molten = melt(objetos, id=1)
names(objetos_molten)[2] = "objeto"
objetos_molten = objetos_molten %>% filter(value==1) %>% select(-value)

molten = full_join(emocoes_molten, argumentos_molten, by='Argumentos')
emo_obj = full_join(emocoes_molten, objetos_molten, by="Argumentos")
obj_ocasiao = full_join(emo_obj, dados[c(1,40)], by="Argumentos")
molten_ocasiao = full_join(molten, dados[c(1,40)], by="Argumentos")
head(molten)
head(emo_obj)
head(molten_ocasiao)

# CRUZA EMOÇÕES E ARGUMENTOS
table(molten$emocao, molten$argumento)
xtable::xtable(table(molten$emocao, molten$argumento))
# testa

fisher.test(table(molten$emocao, molten$argumento), simulate.p.value = T, B=5000)
chisq.test(table(molten$emocao, molten$argumento))


# PREPARANDO
index = grep("C", molten$argumento)
molten$argumento = as.character(molten$argumento)
molten$posicionamento[index] = "Contra"
molten$posicionamento[-index] = "A Favor"
contra = molten[index,]
afavor = molten[-index,]
head(contra)
head(afavor)

# CRUZA EMOÇÕES E TIPO DE ARGUMENTO
table(molten$posicionamento, molten$emocao)
xtable::xtable(table(molten$posicionamento, molten$emocao))
chisq.test(table(molten$posicionamento, molten$emocao))
fisher.test(table(molten$posicionamento, molten$emocao))

# CRUZA EMOÇÕES E ARGUMENTOS
xtable::xtable(table(contra$emocao, contra$argumento))
table(contra$emocao, contra$argumento)
chisq.test(table(contra$emocao, contra$argumento))
fisher.test(table(contra$emocao, contra$argumento), simulate.p.value = T, B=5000)


xtable::xtable(table(afavor$emocao, afavor$argumento))
table(afavor$emocao, afavor$argumento)
fisher.test(table(afavor$emocao, afavor$argumento), simulate.p.value = T, B=5000)

# CRUZA EMOÇÕES E OBJETOS
table(emo_obj$emocao, emo_obj$objeto)
xtable::xtable(table(emo_obj$emocao, emo_obj$objeto))
fisher.test(table(emo_obj$emocao, emo_obj$objeto), simulate.p.value = T, B=5000)


# CRUZA EMOÇÕES E OCASIAO
table(molten_ocasiao$ocasiao, molten_ocasiao$emocao)
chisq.test(table(molten_ocasiao$ocasiao, molten_ocasiao$emocao))


# CRUZA POSICIONAMENTO E OCASIAO
table(molten_ocasiao$posicionamento, molten_ocasiao$ocasiao)
chisq.test(table(molten_ocasiao$posicionamento, molten_ocasiao$ocasiao))

# CRUZA OBJETOS E OCASIÃO
table(obj_ocasiao$ocasiao, obj_ocasiao$objeto)
xtable::xtable(table(obj_ocasiao$objeto, obj_ocasiao$ocasiao))
chisq.test(table(obj_ocasiao$ocasiao, obj_ocasiao$objeto))
fisher.test(table(obj_ocasiao$ocasiao, obj_ocasiao$objeto), simulate.p.value = T, B=5000)


######################################
