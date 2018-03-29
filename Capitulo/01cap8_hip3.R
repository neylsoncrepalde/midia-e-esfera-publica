#### Dados Capítulo 8
#### Hipótese 3
#### Script: Neylson Crepalde
#############################

# Corrigir para a pasta onde o banco de dados está alocado
setwd('~/Documentos/Rousiley/Capitulo')
dir()

library(data.table)

hip3 = fread('cap8_dados_hip3.csv')

names(hip3)

# Testando a hipótese 3 - Os mesmos argumentos aparecem nas plataformas e nos
# comentários? Para isso, vamos reduzir os dados desagregados em Audiências, 
# Facebook e Notícias a PLATAFORMA e COMENTARIOS somando as linhas.

plataformas = apply(hip3[,2:4], 1, sum)
comentarios = apply(hip3[,6:8], 1, sum)

chisq.test(as.table(cbind(plataformas, comentarios)))
fisher.test(as.table(cbind(plataformas, comentarios)), simulate.p.value = T)

# O teste de Fisher mostra que há dependência entre o argumento utilizado e o
# local (plataforma ou comentário). Isso mostra que os argumentos usados não
# são os mesmos. Os argumentos mobilizados nas plataformas tem uma tendência
# diferente daqueles mobilizados nos comentários.

#----------------------------------------------------------
# Agora devemos testar isso de forma desagregada por cada plataforma
chisq.test(as.table(as.matrix(hip3[ ,c(2,6)]))) # Audiencias
fisher.test(as.table(as.matrix(hip3[ ,c(2,6)])), simulate.p.value = T) # Audiencias

chisq.test(as.table(as.matrix(hip3[ ,c(3,7)]))) # Facebook
fisher.test(as.table(as.matrix(hip3[ ,c(3,7)])), simulate.p.value = T) # Facebook

chisq.test(as.table(as.matrix(hip3[ ,c(4,8)]))) # Noticias
fisher.test(as.table(as.matrix(hip3[ ,c(4,8)])), simulate.p.value = T) # Noticias

# Desagregado por cada plataforma, os resultados são os mesmos:
# Há associação entre o tipo de argumento mobilizado e se é 
# plataforma ou comentário

#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
# Agora vamos testar se há associação entre o TIPO de argumento mobilizado
# e se ele aparece em plataforma ou comentário

tipos = data.frame(tipos = c('Pragmático', 'Moral, ético, político', 'Legal',
                             'Pragmático', 'Moral, ético, político', 'Legal'),
                   favoravel = c(1,1,1,0,0,0),
                   audiencias_p = c(18,64,3,5,26,9),
                   facebook_p = c(3,6,3,0,0,0),
                   noticias_p = c(19,132,3,20,77,73),
                   audiencias_c = c(12,100,5,102,66,28),
                   facebook_c = c(24,92,7,158,115,63),
                   noticias_c = c(4,13,0,193,60,47))

tipos_p = apply(tipos[,2:4], 1, sum)
tipos_c = apply(tipos[,5:7], 1, sum)

chisq.test(as.table(cbind(tipos_p, tipos_c)))
fisher.test(as.table(cbind(tipos_p, tipos_c)), simulate.p.value = T)

# Ambos os testes mostram que há associação entre o tipo de argumento
# mobilizado e se ele aparece em plataformas ou comentários

#------------------------------------------------------------
# Agora vamos testar a mesma coisa separando por plataforma

chisq.test(as.table(as.matrix(tipos[ ,c(3,6)]))) # Audiencias
fisher.test(as.table(as.matrix(tipos[ ,c(3,6)])), simulate.p.value = T) # Audiencias

chisq.test(as.table(as.matrix(tipos[ ,c(4,7)]))) # Facebook
fisher.test(as.table(as.matrix(tipos[ ,c(4,7)]))) # Facebook

chisq.test(as.table(as.matrix(tipos[ ,c(5,8)]))) # Noticias
fisher.test(as.table(as.matrix(tipos[ ,c(5,8)])), simulate.p.value = T) # Noticias

# Todos os testes mostraram associação entre as variáveis.

# CONCLUSÃO: Hipótese 3 refutada. Não há correspondência nem dos argumentos
# utilizados nem dos tipos de argumentos entre as plataformas e os
# comentários.
