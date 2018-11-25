# 1) Pré-Processamento

### Carregando dados
library(stats)
teens <- read.csv('./snsdata.csv', header = TRUE)
nrow(teens)
###

### Frequência de Gênero
table(teens$gender, useNA = 'always')
###

### Dummies para identificar gênero
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender),1,0)
teens$no_gender <- ifelse(is.na(teens$gender),1,0)
###

### Distribuição de Idades (Existem "estudantes" de 3.1 a 106.9 anos)
summary(teens$age)
### Conclusão: é necessário filtrar pela idade correta

### Graus do High School e idades de entrada e de saída:
### 9th grade => 13-14
### 10th grade => 14-15
### 11th grade => 15-16
### 12th grade => 16-17
### O estudante conclui o High School com idade entre 17 e 18 anos
### Foram desconsiderados estudantes atrasados ou adiantados

### Idades inválidas foram substituídas por NAs
teens$age <- ifelse(
  teens$age >= 13 & teens$age < 18,
  teens$age, 
  NA
)
summary(teens$age)
###

### Média de Idade Total
ave_age <- mean(teens$age, na.rm = TRUE)
ave_age
###

### Média de Idade por Ano de Graduação
ag <- aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
ag
###

### Pode apagar isso aqui
### Substituindo NAs de Age pela idade média respectiva ao ano de graduação
### Essa parte está errada no tutorial (retorna só a média de 2006)
### Então fiz uma função própria mas ela também não funciona no R (talvez eu tenha errado)
### averageAgePerGradyear <- function(gradyear) {
###  return(ag[ag$gradyear == gradyear,]$age)
### }
### teens$age <- ifelse(
###  is.na(teens$age),
###  averageAgePerGradyear(teens$gradyear),
###  teens$age
###)
### summary(teens$age)
###

### Pode apagar isso aqui
### Substituindo NAs pela Média Total => Não é uma abordagem legal
### teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
### summary(teens$age)
###

### Removendo exemplos com valores inadequados de idade
nrow(teens)
teens <- teens[!is.na(teens['age']),]
rownames(teens) <- 1:nrow(teens)
nrow(teens)
###



# 2) Construção do Modelo

### Normalização aplicada para eliminar bias das preferências dos estudantes
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))
summary(interests_z[5:10])
###

### Aplicando e plotando o COTOVELO
set.seed(123)
models <- sapply(
  1:15,
  function(k){kmeans(interests_z, k, nstart=1)$tot.withinss}
)
plot(1:15, models, type='b', pch = 19, frame = FALSE, 
     xlab='Number of clusters K',
     ylab='Total within-clusters sum of squares')
abline(v = 0, lty =0)
###

### Gerando modelo K-means para K = 5
teen_clusters <- kmeans(interests_z, 5)
###

### Visualizando os clusters
library(factoextra)
fviz_cluster(
  teen_clusters,
  interests_z,
  geom = 'point',
  show.clust.cent = TRUE
)
###



# 3) Analisando resultados do modelo

### Tamanho dos clusters
teen_clusters$size
###

### Identificando em quais clusters estão os estudantes
teens$cluster <- teen_clusters$cluster
teens[1:5, c('cluster', 'gender', 'age', 'friends')]
###

### Idade por cluster
aggregate(data = teens, age ~ cluster, mean)
boxplot(split(teens$age,teens$cluster),main='Idade média por cluster')
###

### Número de amigos por cluster
aggregate(data = teens, friends ~ cluster, mean)
boxplot(split(teens$friends,teens$cluster), outline=FALSE,main='Número médio de amigos por cluster')
###

### Proporção de mulheres por cluster
aux <- aggregate(data = teens, female ~ cluster, mean)
barplot(aux$female, names.arg = c('1','2','3','4','5'), main = 'Proporção de mulheres por cluster')
###

####