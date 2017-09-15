##############################
# Análises dos argumentos
# Dados: Gabriella e Rousiley
# Script: Neylson Crepalde
##############################

# Define o diretório de trabalho
setwd('~/Documentos/Rousiley')
# Verifica os arquivos no diretório
list.files()

# Carrega bibliotecas
library(magrittr)
library(xlsx)

# Lê os dados
geral = read.xlsx("emocao_tipo_argumento.xlsx", sheetIndex = 1)

#Separando as tabelas contra e a favor
contra = geral[2:7,2:13]
contra = sapply(contra, as.character)
contra = apply(contra, 2, as.numeric)
contra = as.matrix(contra)

colnames(contra) = c("Arg1","Arg2","Arg3","Arg4","Arg5","Arg6","Arg7",
                  "Arg8","Arg9","Arg10","Arg11","Arg12")
rownames(contra) = c("compaixão","temor", "indignação","cólera", "outro","não se aplica")


afavor = geral[10:15,2:13]
afavor = sapply(afavor, as.character)
afavor = apply(afavor, 2, as.numeric)
afavor = as.matrix(afavor)

colnames(afavor) = c("Arg1","Arg2","Arg3","Arg4","Arg5","Arg6","Arg7",
                  "Arg8","Arg9","Arg10","Arg11","Arg12")
rownames(afavor) = c("compaixão","temor", "indignação","cólera", "outro","não se aplica")
afavor


#-------------------------------------------------
# invertendo as matrizes
bdcontra = t(contra) %>% as.data.frame
bdafavor = t(afavor) %>% as.data.frame


# Rodando teste qui-quadrado com as somas das emoções encontradas em cada arg
sum_contra = sapply(bdcontra, sum)
sum_afavor = sapply(bdafavor, sum)

sum_geral = rbind(sum_contra, sum_afavor)
sum_geral = sum_geral[,-5]

# Roda o qui-quadrado
chisq.test(sum_geral)

#Apresenta a tabela de proporções
sum_geral_prop = prop.table(as.table(sum_geral))


#Rodando teste de Fisher para associação argumentos e posicionamento
arg_contra = apply(bdcontra, 1, sum)
arg_afavor = apply(bdafavor, 1, sum)

arg_geral = rbind(arg_contra, arg_afavor)
arg_geral_prop = prop.table(as.table(arg_geral))

# Apresenta as tabelas de frequência e proporção
arg_geral
arg_geral_prop

# Roda o teste de Fisher com 100000 simulações
fisher.test(arg_geral, simulate.p.value = T, B=100000)
