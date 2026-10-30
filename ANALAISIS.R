## 
lista_imputaciones <- mice::complete(imputacion_mice, action = "all")
lista_imputaciones$`1`

caso_1 <- lista_imputaciones[[1]]
caso_1_scale <- caso_1 %>% 
  select(-year, -iso3c)
caso_1_scale <- scale(caso_1_scale, center = TRUE, scale = TRUE)
PCA <- prcomp(caso_1_scale)
PCA$rotation
summary(PCA)


for (i in 1:30) {
  caso_n <-  lista_imputaciones[[i]]
  caso_n_scale <- caso_n %>% 
    select(-year, -iso3c)
  caso_n_scale <- scale(caso_1_scale, center = TRUE, scale = TRUE)
  PCA_n <- prcomp(caso_1_scale)
  print(paste0("Este es el caso:    ", i))
  print(summary(PCA_n))
  print(PCA_n$rotation[,1:6])
}


## con bayesian norm 

tictoc::tic()
imputacion_mice <- mice::mice(
  data            = datos_para_mice,
  m               = 30,            # Cambiar a 30 para la corrida final de publicación
  maxit           = 10,
  method          = "norm",
  donors          = 5,
  visitSequence   = "monotone",   # Ordena de menor a mayor % de NAs (Sec. 5.5.1)
  predictorMatrix = matriz_predictores,
  ridge           = 0.001,
  printFlag       = TRUE
)
tictoc::toc()

mice::densityplot(
  imputacion_mice, 
  ~ tech_id_pib + tech_investigad + tech_exp_alta + tech_exp_serv_dig + 
    tech_patentes_tic + fin_mkt_cap_pib + fin_regalias_rec
)

i = 1
caso_n <-  lista_imputaciones[[i]]
caso_n_scale <- caso_n %>% 
  select(-year, -iso3c)
caso_n_scale <- scale(caso_1_scale, center = TRUE, scale = TRUE)
PCA_n <- prcomp(caso_1_scale)
paste0("Este es el caso:    ", i)
summary(PCA_n)
PCA_n$rotation[,1:6]

biplot(PCA)


library(factoextra)
library(FactoMineR)

facto_pca <- PCA(caso_n_scale)
facto_pca$ind
summary(facto_pca)
factoextra::fvis_pca(PCA_n)


### 

summary(PCA)

P

library(plotly)

PCA$x

score <- as.data.frame(PCA$x)

score <- score %>% 
  mutate(iso3c = caso_1$iso3c)


fig <- plot_ly(score, x = ~PC1, y = ~PC2, z = ~PC3, color = ~iso3c)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                                   yaxis = list(title = 'Gross horsepower'),
                                   zaxis = list(title = '1/4 mile time')))

fig

