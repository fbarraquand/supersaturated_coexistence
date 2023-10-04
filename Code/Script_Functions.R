##########################Functions :
{
  
  makeparameters<-function(r, D, m, S, K, C){#function that create the parameter array used in dXt
    
    parameters<-array(0, dim = c(dim(K)[1], dim(K)[2], 4))#Setting the size of the array.
    
    parameters[, , 1]<-K #Matrix of half saturation constants
    parameters[, , 2]<-C #Matrix of the content of each resources in each species
    parameters[1, , 3]<-r#Maximum specific growth rate
    parameters[2, , 3]<-m#specific mortality rates 
    parameters[, 1, 4]<-S#supply concentration of resources
    parameters[1, 2, 4]<-D#the system's turnover rate
    
    parameters
  } 
  
  mu<- function(R, r, K){ 
    apply( t( R /(K+R) ) * r, 1, min)#specific growth rate ; Equation (3)
  }
  
  dXt <- function(t, X, parameters ){ #function that gives the values of the derivatives of N and R
    
    #extracting the parameters from the parameters array : 
    
    K<-parameters[, , 1]#Matrix of half saturation constants
    C<-parameters[, , 2]#Matrix of the content of each resources in each species
    r<-parameters[1, , 3]#Maximum specific growth rate
    m<-parameters[2, , 3]#specific mortality rates 
    S<-parameters[, 1, 4]#supply concentration of resources
    D<-parameters[1, 2, 4]#the system's turnover rate
    
    l = length(X)
    
    N<-X[1:dim(K)[2]]#The population abundance of species 
    R<-X[(dim(K)[2]+1):l]#The availability of resources 
    
    dN<-N*(mu(R, r, K)-m)#Equation (1)
    dR<-D*(S-R)-C%*%(N*mu(R, r, K))#Equation (2)
    dX<-c(dN, dR)
    dX<-list(ifelse(X<0, 0, dX))#Filtering out negative outliers
  }
}
