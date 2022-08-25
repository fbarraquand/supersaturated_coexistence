##########################Used libraries and functions : 
{
  library(deSolve) 
  library(truncnorm)

  source("Script_Functions.R")
}

##########################experiment 1 : 
{
  
  #size of the test sample : 
  n_exp1 <- 500
  
  #array that will receive the results : 
  stats_exp1 <- data.frame()
  
  #parameters that will not change : (mostly reusing the parameters used for Fig 4)
  {  
    D_exp1<- 0.25
    m_exp1<-rep(D_exp1, 12)
    S_exp1<-c(6, 10, 14, 4, 9)
    
    K_exp1<- matrix(scan("./Matrix_K/K_4.txt"), 5, 12, T)
    C_exp1<- matrix(scan("./Matrix_C/C_4.txt"), 5, 12, T)
    
    parameters_exp1<-makeparameters(rep(0, 12), D_exp1, m_exp1, S_exp1, K_exp1, C_exp1)
    
    #randomly choosing the parameter r, but assuring that the firsts r will be the "normal" configuration (only ones)
    set.seed(1473)
    matr_exp1 <- matrix(c(rep(1, 12), rtruncnorm(n_exp1*12, 0.7, 1.3, 1, 0.1)), n_exp1+1, 12, T)
    
    
  }
  
  #Then it is exactly how we calculated the values to plot Fig4 
  #except that in order to minimize the calculus time, we extracted as much as we could of the calculus outside of the loop  : 
  #(In order to avoid making useless calculus each time the loop is done)
  {
    #initial state : 
    R_j_1_0_exp1<-S_exp1
    names(R_j_1_0_exp1) <- c(paste("R", 1:5, sep = ""))
    N_j_1_0_exp1<- c(0.1+(1:5)/100, rep(0, 7))
    names(N_j_1_0_exp1) <- c(paste("N", 1:12, sep = ""))
    X_j_1_0_exp1<- c(N_j_1_0_exp1, R_j_1_0_exp1)
    
    
    #times sequences for the output:
    times_1_exp1<-seq(0, 1000, by = D_exp1)
    times_2_exp1<-seq(1000, 3000, by = D_exp1)
    times_3_exp1<-seq(3000, 5000, by = D_exp1)
    times_4_exp1<-seq(5000, 10000, by = D_exp1)
  }
  
  #running the experiment : 
  for (j in (1:(n_exp1+1))){

    #parameters that will change : 
    {
      parameters_exp1[1, , 3] <- matr_exp1[j, ]#changing r
    }
    
    #AS for _1c and _1d :
    #In order to add new species mid-time, the resolution is split every time new species are added : 
    #The ode function is used on a certain period of time between two addition. 
    #When new species must be added, the calculus is stopped. 
    #The final values of the last resolution will be used as the new initial values of the next, with also the newly added species.
    {
      #First period (Species 1-2-3-4-5)
      {
        #use of the ode function from the deSolve library
        results_1_exp1<-round(ode(X_j_1_0_exp1, times_1_exp1, dXt, parameters_exp1), digits = 4)
      }
      
      #Second period (Species 1-2-3-4-5-6-7-8)
      {   
        #initial state : 
        X_j_2_0_exp1<-results_1_exp1[dim(results_1_exp1)[1], 2:18]
        X_j_2_0_exp1[6:8]<- c(0.1, 0.1, 0.1)

        #use of the ode function from the deSolve library
        results_2_exp1<-round(ode(X_j_2_0_exp1, times_2_exp1, dXt, parameters_exp1), digits = 4)
      }
      
      #Third period (Species 1-2-3-4-5-6-7-8-9-10)
      {
        #initial state : 
        X_j_3_0_exp1<-results_2_exp1[dim(results_2_exp1)[1], 2:18]
        X_j_3_0_exp1[9:10]<- c(0.1, 0.1)
        
        #use of the ode function from the deSolve library
        results_3_exp1<-round(ode(X_j_3_0_exp1, times_3_exp1, dXt, parameters_exp1), digits = 4)
      }
      
      #Fourth period (Species 1-2-3-4-5-6-7-8-9-10-11-12)
      {
        #initial state : 
        X_j_4_0_exp1<-results_3_exp1[dim(results_3_exp1)[1], 2:18]
        X_j_4_0_exp1[11:12]<- c(0.1, 0.1)
   
        #use of the ode function from the deSolve library
        results_4_exp1<-round(ode(X_j_4_0_exp1, times_4_exp1, dXt, parameters_exp1), digits = 4)
      }
    }  
    
    #transforming the data to plot it
    {
      #concatenating all the results together : 
      results_exp1<-rbind(results_1_exp1[, 1:13][-dim(results_1_exp1)[1], ], 
                         results_2_exp1[, 1:13][-dim(results_2_exp1)[1], ], 
                         results_3_exp1[, 1:13][-dim(results_3_exp1)[1], ], 
                         results_4_exp1[, 1:13])
      
      #sometimes the simulation can generate error such as negative or NA values.
      #Before saving the data we check if it's the case or not.
      if((!is.na(results_exp1[dim(results_exp1)[1], 2]))&(setequal(((results_exp1[dim(results_exp1)[1], 2:13])> = 0),rep(T,12)))){

        nametxtj <- paste('./DataExp/Exp1/dataresults_exp1_', as.character(j), '.txt',  sep = "")
        
        write.table(as.data.frame(results_exp1), nametxtj)
    
        coex_species_1 <- (results_exp1[dim(results_exp1)[1], 2:13]>0.001)
    
        stats_exp1 <- rbind(stats_exp1, c(j, matr_exp1[j, ], sum(coex_species_1)))
      }
    }
    
  }
  
  #saving the statistics 
  names(stats_exp1) <- c("N°", paste("r", 1:12, sep = ""), "Number of coexisting species")
  write.table(stats_exp1, "./DataExp/Exp1/stats_exp1.txt")

  #displaying it 
  results_exp1 <- data.frame(Probability = c(colSums(outer(stats_exp1[-1, 14], 0:12, " = = ")), sum(stats_exp1[-1, 14]>5))*(100/dim(stats_exp1[-1, ])[1]))
  row.names(results_exp1) <- c(paste(0:12, "species", sep = " "), "Supersaturated")
  write.table(results_exp1, "./DataExp/Exp1/results_exp1.txt")
  
  #Saving the results as LaTex tables : 
  results_exp1_1 <- as.data.frame(t(results_exp1))[, 1:7]
  results_exp1_2 <- as.data.frame(t(results_exp1))[, 8:14]
  
  print(xtable(results_exp1_1, type = "latex"), file = "./Figures/results_exp1_1.tex")
  print(xtable(results_exp1_2, type = "latex"), file = "./Figures/results_exp1_2.tex")
  
}

##########################experiment 2 : 
{
  #size of the test sample : 
  n_exp2 <- 500
  
  #array that will receive the results : 
  stats_exp2 <- data.frame()
  
  #parameters that will not change : (mostly reusing the parameters used for Fig 4)
  {
  D_exp2<- 0.25
  m_exp2<-rep(D_exp2, 12)
  S_exp2<-c(6, 10, 14, 4, 9)
  
  K_exp2<- matrix(scan("./Matrix_K/K_4.txt"), 5, 12, T)
  C_exp2<- matrix(scan("./Matrix_C/C_4.txt"), 5, 12, T)
  
  #randomly choosing the parameter r, but assuring that the firsts r will be the "normal" configuration (only ones) 
  set.seed(1151)
  matr_exp2 <- matrix(c(rep(1, 12), rtruncnorm(n_exp2*12, 0.7, 1.3, 1, 0.1)), n_exp2+1, 12, T)
  }
  
  #initial state : 
  { 
    R_exp20<-S_exp2
    names(R_exp20) <- c(paste("R", 1:5, sep = ""))
    N_exp20<- c(0.1+(1:12)/100)
    names(N_exp20) <- c(paste("N", 1:12, sep = ""))
    X_exp20<- c(N_exp20, R_exp20)
  }
  
  #times sequence for the output:
  times_exp2<-seq(0, 10000, by = D_exp2)
  
  parameters_exp2j<-makeparameters(rep(0, 12), D_exp2, m_exp2, S_exp2, K_exp2, C_exp2)#parameter matrix
  
  for (j in (1:(n_exp2+1))){
    #parameters that will change : 
    {
      parameters_exp2j[1, , 3] <- matr_exp2[j, ]#changing the r parameter
      
    }
  
    #use of the ode function from the deSolve library
    results_exp2j<-round(ode(X_exp20, times_exp2, dXt, parameters_exp2j), digits = 4)
  
    nametxtj <- paste('./DataExp/Exp2/dataresults_exp2_', as.character(j), '.txt',  sep = "")
    write.table(as.data.frame(results_exp2j), nametxtj)
    
    coex_species_2 <- (results_exp2j[, 2:13][dim(results_exp2j)[1], ]>0.001)
    
    stats_exp2 <- rbind(stats_exp2, c(j, matr_exp2[j, ], sum(coex_species_2)))
      
  }
  
  names(stats_exp2) <- c("N°", paste("r", 1:12, sep = ""), "Number of coexisting species")
  write.table(stats_exp2, "./DataExp/Exp2/stats_exp2.txt")
  
  results_exp2 <- data.frame(Probability = c(colSums(outer(stats_exp2[-1, 14], 0:12, " = = ")), sum(stats_exp2[-1, 14]>5))*(100/dim(stats_exp2[-1, ])[1]))
  row.names(results_exp2) <- c(paste(0:12, "species", sep = " "), "Supersaturated")
  
  write.table(results_exp2, "./DataExp/Exp2/results_exp2.txt")
  
  
  #Saving the results as LaTex tables : 
  results_exp2_1 <- as.data.frame(t(results_exp2))[, 1:7]
  results_exp2_2 <- as.data.frame(t(results_exp2))[, 8:14]
  
  print(xtable(results_exp2_1, type = "latex"), file = "./Figures/results_exp2_1.tex")
  print(xtable(results_exp2_2, type = "latex"), file = "./Figures/results_exp2_2.tex")
  
}




