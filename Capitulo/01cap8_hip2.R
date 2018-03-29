#### Dados Cap8
#### Hipótese 2
#### Script: Neylson Crepalde
#############################

plataformas = data.frame(tipo = c('desacordo','ausência','acordo'),
                         justificacao = c(115,183,13),
                         opiniao = c(47,142,15))

audiencias = data.frame(tipo = c('desacordo','ausência','acordo'),
                        justificacao = c(64,278,13),
                        opiniao = c(16,57,5))

facebook = data.frame(tipo = c('desacordo','ausência','acordo'),
                      justificacao = c(152,160,24),
                      opiniao = c(87,67,14))

# Hipótese 2:
# Haverá maior ocorrência de justificação onde tem maior incidência de desacordo
# Comparar entre plataformas

lista = list(plataformas, audiencias, facebook)
obtemTabela = function(x) {
  # Calcular taxa de desacordo e taxa de justificacao
  res = matrix(ncol = 2, nrow=1)
  tx_desacordo = (x[1,2] + x[1,3]) / sum(x[,-1])
  res[1,1] = tx_desacordo
  
  tx_justificacao = sum(x[,2]) / sum(x[,-1])
  res[1,2] = tx_justificacao
  
  return(res)
}

# Montando a tabela Taxa de Desacordo e Taxa de justificacao
tabela = t(sapply(lista, obtemTabela))
row.names(tabela) = c('Plataformas','Audiências', 'Facebook')
colnames(tabela) = c('Taxa Desacordo', 'Taxa Justificação')
tabela

# Não dá teste estatístico
# cor.test(tabela[,1], tabela[,2])

