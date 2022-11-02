```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
saveRDS(dadao,"dados.rds")

```{r dadosf,cache=T}
library(radiant)
#radiant_window()
## filter and sort the dataset
## Load commands
dados <- readr::read_rds("dados.rds") # %>%
# fix_names()  # o comando fix_names altera os nomes retirando _ e espaços etc
dadosf<-dados %>%
  filter(`Mato Grosso do Sul`!=0) %>%   # apenas onde Mato Grosso do Sul não é zerado
  select(codSubclasse:`Mato Grosso do Sul`) #%>%
#dtab(dec = 2, nr = 100) %>% render()
dadosz<-dados %>%
  filter(`Mato Grosso do Sul`==0) %>%
  select(codSubclasse:`Mato Grosso do Sul`) #%>%
#dtab(dec = 2, nr = 100) %>% render()
```

matf<-  t(dadosf[,c(3:81)]) # somente numeros e transpor
source('code.R')
QLf<-QL(matf)
PRf<-PR(matf) 
IHHf<-IHH(matf) 




```
```{r icn, cache=T}
nomecidades<-rownames(matf)
ICN_all <- data.frame(nomecidades)
#ICNf<-ICN(nomecidades,QLf,IHHf,PRf)
# ICN - fiz um loop inves de funcao
for (k in 1:ncol(QLf)) {
  print(c("coluna ",k))
  componentes<-data.frame(cbind(QL=QLf[,k],IHH=IHHf[,k],PR=PRf[,k]))
  
  # padronizar variaveis
  x <- scale(componentes[,1:3])
  r <- cor(x)  # matriz de correlação de x
  print(psych::cortest.bartlett(r, n=length(x[,1])))  # teste de Bartlett
  print(psych::KMO(r))  # teste de KMO
  # realizar componentes principais em x
  pca_psych_rotated <- psych::principal(x, 
                                        rotate="varimax", 
                                        nfactors=3, scores=F,
                                        oblique.scores=T)
  print(summary(pca_psych_rotated))
  ### Extração dos loadings e Proportion_Var
  pca_psych_rotated$values  # loadings = eigenvalues
  pca_psych_rotated$loadings  # rotated component matrix
  #
  # extract loadings as a data.frame 
  # (https://stackoverflow.com/questions/17371266/extracting-output-from-principal-function-in-psych-package-as-a-data-frame)
  loadings<-as.data.frame(unclass(pca_psych_rotated$loadings))
  loadings
  #
  # normalizar loadings
  matriz_normal<-t(t(loadings)/rowSums(t(loadings)))
  matriz_normal
  #
  # Proportion Var
  Vaccounted<-as.data.frame(unclass(pca_psych_rotated$Vaccounted))
  Proportion_Var<-t(Vaccounted[2,])  # matriz de autovalores
  Proportion_Var
  
  #### obter theta = matriz_normal%\*%varexpl
  #varexpl  é o mesmo que Proportion_Var transposto
  varexpl<-matrix(pca_psych_rotated$Vaccounted[4,],nrow = 1,ncol = 3)
  varexpl  
  theta=matriz_normal%*%Proportion_Var
  colnames(theta)<-c("theta")
  theta
  ### Cálculo de ICN
  dados_originais<-componentes[,]
  ICN=as.matrix.data.frame(dados_originais,
                           ncol = 3,nrow = length(mat), 
                           nrow = rows, 
                           ncol = cols) %*% matrix(theta,nrow = 3,ncol = 1)
  
  # saida dos resultados
  ICN_all <- cbind(ICN_all, round(ICN, digits = 3))
  
}
colnames(ICN_all)=c("Cidades",t(dadosf[,1]))
ICN_all<-rbind(c("Cidades",t(dadosf[,2])),ICN_all)
writexl::write_xlsx(as.data.frame(ICN_all),path = "ICN_all.xlsx")
```

Fazer teste com dataset com zeros e rotinas do code.

```{r matfull}
dados <- readr::read_rds("dados.rds") 
library(tidyverse)
mat<-  t(dados[,c(3:81)]) # somente numeros e transpor
source('code.R')
QL<-QL(mat)
PR<-PR(mat) 
IHH<-IHH(mat) 
writexl::write_xlsx(as.data.frame(QL),path = "QL.xlsx")
writexl::write_xlsx(as.data.frame(PR),path = "PR.xlsx")
writexl::write_xlsx(as.data.frame(IHH),path = "IHH.xlsx")
nomecidades<-rownames(mat)

```

Não roda com as colunas zeradas!
  
  ```{r}
nomecidades<-rownames(matf)
source("code.r")
ICN<-ICN(nomecidades,QLf,PRf,IHHf)
```
