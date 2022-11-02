# novo script a partir de EconGeo::RCA
# QL = quociente locacional
QL <-function (mat) 
{
  mat <- as.matrix(mat)
  share_tech_city <- mat/rowSums(mat)
  share_tech_total <- colSums(mat)/sum(mat)
  QL <- t(t(share_tech_city)/share_tech_total)
  QL[is.na(QL)] <- 0
  #QL = round(QL, digits = 4)
  return(QL)
}
# PR = participacao relativa
PR<-function (mat) 
  {
  PR<-as.data.frame(mat) %>%
    mutate_if(is.numeric, funs(./sum(.))) 
  #PR = round(PR, digits = 4)
  return(PR)
}

# IHH = Indice Herfindahl-Hirschman modificado
IHH <-function (mat) 
{
  #mat <- as.matrix(mat)
  e_i<-as.matrix(rowSums(mat))
  e<-as.numeric(sum(mat))
  s_ki <- as.data.frame(mat) %>%
    mutate_if(is.numeric, funs(./sum(.))) #PR
  s_ki <- as.matrix(s_ki) # matriz 27 x 1327
  s_i <- as.matrix(e_i/e) # matriz 27 x 1
  s_i = s_i[,rep(1,times=ncol(mat))] # repetir colunas até length(matf)
  IHH<-s_ki-s_i
  IHH[is.na(IHH)] <- 0
  #IHH = round(IHH, digits = 4)
  return(IHH)
}
# ICN - componentes principais
# a instrução deve conter os dados de QL, depois IHH e depois PR
# obtidos das rotinas acima
ICN <-function (Nomemun,QL,IHH,PR) {
  ICN_all <- data.frame(Nomemun)
  for (k in 1:ncol(QL)) {
    print(c("coluna ",k))
    componentes<-data.frame(cbind(QL=QL[,k],IHH=IHH[,k],PR=PR[,k]))
    
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
  colnames(ICN_all)=c("Nomemun",seq(1:length(QL)))
  writexl::write_xlsx(ICN_all,path = "ICN_all.xlsx")
  ICN_all
} 

