#### Dados Cap8
#### Hipótese 1
#### Script: Neylson Crepalde
#############################

bd = data.frame(local = c('Audiências', 'Notícias', 'Facebook'),
                plat_contra = c(50,121,18),
                plat_afavor = c(24,126,0),
                plat_NI = c(4,20,0),
                coment_contra = c(116,22,92),
                coment_afavor = c(218,453,219),
                coment_NI = c(72,27,66),
                desac_bold = c(50, 126, 171),
                desac_soft = c(30,36,68),
                desac_absence = c(335, 340, 227))
bd

# Hipótese:
# Haverá maior ocorrência de desacordo em plataformas com maior incidência de 
# informação contrária à posição majoritária de usuários. Em outras palavras:
# desacordo > when plataforma != usuarios
bd$posicao_plat = c('contra','inconclusivo','contra')
bd$posicao_coment = c('afavor', 'afavor', 'afavor')
bd$desacordo = bd$desac_bold + bd$desac_soft

# Não é possível testar com maior acurácia pois os dados estão agregados demais

#-------------------------------
# Tentar um teste de proporções
teste = bd[,c(13,10)]

prop.test(as.matrix(teste), correct = F)
# As proporções são diferentes. 

# Testando a hipótese para Audiencias e noticias:
# Espera-se que desacordo seja maior em 1 do que em 2
prop.test(as.matrix(teste[1:2,]), correct = F, alternative = 'g') 
prop.test(as.matrix(teste[1:2,]), correct = F, alternative = 'l') 
# No caso da comparação entre Audiencias e notícias, a hipótese foi refutada. 
# Embora as audiências tenham mais divergência entre a
# plataforma e os comentários, ela possui menos desacordo. O teste
# para MENOS desacordo foi estatisticamente significativo.

# Testando a hipotese para noticias e Facebook
# Espera-se que o desacordo seja maior em 2 do que em 1
prop.test(as.matrix(teste[2:3,]), correct = F, alternative = 'l')

# No caso da comparação entre Notícias e Facebook, a hipótese foi confirmada!
# Havendo mais informação contrária à posição dominante dos usuários no Facebook,
# este possui mais desacordo em comparação às Noticias

# Testando agora entre Audiencias e Facebook
#Espera-se maior desacordo em 2 do que em 1
prop.test(as.matrix(teste[c(1,3),]), correct = F, alternative = 'l') 
# No caso da comparação entrer Audiencias e Facebook, a hipótese foi confirmada!
