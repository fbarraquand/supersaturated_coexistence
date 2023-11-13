##########################Used libraries and functions : 
library(deSolve) 

source("Script_Functions.R")


##########################To obtain Fig_1a and 1b : 
{
  #parameters : (according to the article, in "Methods")
  {
    r_1ab<-rep(1,3)
    D_1ab<- 0.25
    m_1ab<-rep(D_1ab,3)
    S_1ab<-rep(10,3)

    K_1ab <- as.matrix(read.table("./Matrix_K/K_1ab.table", sep = " ", numerals = "no.loss"))
    C_1ab <- as.matrix(read.table("./Matrix_C/C_1ab.table", sep = " ", numerals = "no.loss"))
    
    parameters_1ab<-makeparameters(r_1ab, D_1ab, m_1ab, S_1ab, K_1ab, C_1ab)#parameters array for Fig1a and Fig1b
  }
  
  #initial state : 
  {
    R_0_1ab<-S_1ab
    names(R_0_1ab) <- c("R1", "R2", "R3")
    N_0_1ab<-0.1+(1:3)/100 #as specified in the article, "Ni = 0.1 + i/100" 
    names(N_0_1ab)<-c("N1", "N2", "N3")
    X_0_1ab<-c(N_0_1ab, R_0_1ab)
  }
  
  #time sequence for the output
  times_1ab<-seq(0, 200, by = D_1ab)
  
  #use of the ode function from the deSolve library to solve the differential equation : 
  results_1ab<-round(ode(y = X_0_1ab, times = times_1ab, func = dXt, parms = parameters_1ab), digits = 4)
  
  #saving the data to plot it later
  write.table(as.data.frame(results_1ab[, -(5:7)]), "./DataHW1999/dataresults_1ab.txt")

}

##########################To obtain Fig_1c : 
{
  #parameters : (according to the article, in "Methods")
  {
    r_1c<-rep(1, 6)
    D_1c<- 0.25
    m_1c<-rep(D_1c, 6)
    S_1c<-c(6, 10, 14)
    
    
    K_1c <- as.matrix(read.table("./Matrix_K/K_1c.table", sep = " ", numerals = "no.loss"))
    C_1c <- as.matrix(read.table("./Matrix_C/C_1c.table", sep = " ", numerals = "no.loss"))

    parameters_1c <- makeparameters(r_1c, D_1c, m_1c, S_1c, K_1c, C_1c)
      
  }
  
  #In order to add new species mid-time, the resolution is split every time new species are added : 
  #The ode function is used on a certain period of time between two addition. 
  #When new species must be added, the calculus is stopped. 
  #The final values of the last resolution will be used as the new initial values of the next, with also the newly added species.
  
  #First period (Species 1-2-3)
  {
    #initial state : 
    R_1_0_1c<-S_1c
    names(R_1_0_1c) <- c("R1", "R2", "R3")
    N_1_0_1c<-c(0.1+(1:3)/100, 0, 0, 0)#notice that the species 4-5-6 are absent for the moment
    names(N_1_0_1c) <- c("N1", "N2", "N3", "N4", "N5", "N6")
    X_1_0_1c<-c(N_1_0_1c, R_1_0_1c)

    
    #times sequence for the output :
    times_1_1c<-seq(0, 1000, D_1c) #because species 4 start at t = 1,000
    
    #use of the ode function from the deSolve library
    results_1_1c<-round(ode(X_1_0_1c, times_1_1c, dXt, parameters_1c), digits = 4)
  }

  #Second period (Species 1-2-3-4)
  {
    #initial state : 
    X_2_0_1c <- results_1_1c[dim(results_1_1c)[1], 2:10]#using the final values of the previous period as initial values
    X_2_0_1c[4] <- 0.1 #adding the new species (species 4)
    
    #times sequence for the output :
    times_2_1c<-seq(1000, 2000, D_1c) #because species 5 start at t = 2,000
    
    #use of the ode function from the deSolve library
    results_2_1c<-round(ode(X_2_0_1c, times_2_1c, dXt, parameters_1c), digits = 4)
  }
  
  #Third period (Species 1-2-3-4-5)
  {
    #initial state : 
    X_3_0_1c <- results_2_1c[dim(results_2_1c)[1], 2:10]
    X_3_0_1c[5] <- 0.1 #adding the new species (species 5)
    
    #times sequence for the output :
    times_3_1c<-seq(2000, 5000, D_1c)#because species 6 start at t = 5,000
    
    #use of the ode function from the deSolve library
    results_3_1c<-round(ode(X_3_0_1c, times_3_1c, dXt, parameters_1c), digits = 4)
  }
  
  #Fourth period (Species 1-2-3-4-5-6)
  {
    #initial state : 
    X_4_0_1c <- results_3_1c[dim(results_3_1c)[1], 2:10]
    X_4_0_1c[6] <- 0.1#adding the new species (species 6)
    
    #times sequence for the output :
    times_4_1c<-seq(5000, 15000, D_1c)#until the end of the simulation
    
    #use of the ode function from the deSolve library
    results_4_1c<-round(ode(X_4_0_1c, times_4_1c, dXt, parameters_1c), digits = 4)
  }
  
  #transforming and saving the data to plot it 
  {
    #concatenating every period into one
    results_1c<-rbind(results_1_1c[, 1:7][-dim(results_1_1c)[1], ], 
                      results_2_1c[, 1:7][-dim(results_2_1c)[1], ], 
                      results_3_1c[, 1:7][-dim(results_3_1c)[1], ], 
                      results_4_1c[, 1:7])
      
    #saving the data to plot it later
    write.table(as.data.frame(results_1c[, 1:7]), "./DataHW1999/dataresults_1c.txt")
  }
  
}

##########################To obtain Fig_1d : 
{
  #parameters : (according to the article, in "Methods")
  {
    r_1d<-rep(1, 9)
    D_1d<- 0.25
    m_1d<-rep(D_1d, 9)
    S_1d<-c(10, 10, 10)
    
    K_1d <- as.matrix(read.table("./Matrix_K/K_1d.table", sep = " ", numerals = "no.loss"))
    C_1d <- as.matrix(read.table("./Matrix_C/C_1d.table", sep = " ", numerals = "no.loss"))
    
    
    parameters_1d <- makeparameters(r_1d, D_1d, m_1d, S_1d, K_1d, C_1d)
    
  }
  
  #AS for _1c :
  #In order to add new species mid-time, the resolution is split every time new species are added : 
  #The ode function is used on a certain period of time between two addition. 
  #When new species must be added, the calculus is stopped. 
  #The final values of the last resolution will be used as the new initial values of the next, with also the newly added species.
  {
    #First period (Species 1-2-3)
    {
      #initial state :
      {    
        R_1_0_1d<-S_1d
        names(R_1_0_1d)<-c("R1", "R2", "R3")
        N_1_0_1d<-c(0.1+(1:3)/100, rep(0, 6))
        names(N_1_0_1d) <- c("N1", "N2", "N3", "N4", "N5", "N6", "N7", "N8", "N9")
        X_1_0_1d<-c(N_1_0_1d, R_1_0_1d)
      }
  
      #times sequence for the output :
      times_1_1d<-seq(0, 250, D_1d)
      
      #use of the ode function from the deSolve library
      results_1_1d<-round(ode(X_1_0_1d, times_1_1d, dXt, parameters_1d), digits = 4)
  }
    
    #Second period (Species 1-2-3-4)
    {
      #initial state : 
      X_2_0_1d<-results_1_1d[dim(results_1_1d)[1], 2:13]
      X_2_0_1d[4]<- 0.1
      
      #times sequence for the output :
      times_2_1d<-seq(250, 500, D_1d)
      
      #use of the ode function from the deSolve library
      results_2_1d<-round(ode(X_2_0_1d, times_2_1d, dXt, parameters_1d), digits = 4)
  }
    
    #Third period (Species 1-2-3-4-5)
    {
      #initial state : 
      X_3_0_1d<-results_2_1d[dim(results_2_1d)[1], 2:13]
      X_3_0_1d[5]<- 0.1
      
      #times sequence for the output :
      times_3_1d<-seq(500, 750, D_1d)
      
      #use of the ode function from the deSolve library
      results_3_1d<-round(ode(X_3_0_1d, times_3_1d, dXt, parameters_1d), digits = 4)
  }
    
    #Fourth period (Species 1-2-3-4-5-6)
    {
      #initial state : 
      X_4_0_1d<-results_3_1d[dim(results_3_1d)[1], 2:13]
      X_4_0_1d[6]<- 0.1
      
      #times sequence for the output :
      times_4_1d<-seq(750, 1000, D_1d)
      
      #use of the ode function from the deSolve library
      results_4_1d<-round(ode(X_4_0_1d, times_4_1d, dXt, parameters_1d), digits = 4)
  }
    
    #Fifth period (Species 1-2-3-4-5-6-7)
    {
      #initial state : 
      X_5_0_1d<-results_4_1d[dim(results_4_1d)[1], 2:13]
      X_5_0_1d[7]<- 0.1
      
      #times sequence for the output :
      times_5_1d<-seq(1000, 1250, D_1d)
      
      #use of the ode function from the deSolve library
      results_5_1d<-round(ode(X_5_0_1d, times_5_1d, dXt, parameters_1d), digits = 4)
  }
    
    #Sixth period (Species 1-2-3-4-5-6-8)
    {
      #initial state : 
      X_6_0_1d<-results_5_1d[dim(results_5_1d)[1], 2:13]
      X_6_0_1d[8]<- 0.1
      
      #times sequence for the output :
      times_6_1d<-seq(1250, 1500, D_1d)
      
      #use of the ode function from the deSolve library
      results_6_1d<-round(ode(X_6_0_1d, times_6_1d, dXt, parameters_1d), digits = 4)
  }
      
    #Seventh period (Species 1-2-3-4-5-6-8-9)
    {
      #initial state : 
      X_7_0_1d<-results_6_1d[dim(results_6_1d)[1], 2:13]
      X_7_0_1d[9]<- 0.1
      
      #times sequence for the output :
      times_7_1d<-seq(1500, 3000, D_1d)
      
      #use of the ode function from the deSolve library
      results_7_1d<-round(ode(X_7_0_1d, times_7_1d, dXt, parameters_1d), digits = 4)
  }
    
  }
  
  #transforming and saving the data to plot it
  {
    results_1d<-rbind(results_1_1d[, 1:10][-dim(results_1_1d)[1], ], 
                    results_2_1d[, 1:10][-dim(results_2_1d)[1], ], 
                    results_3_1d[, 1:10][-dim(results_3_1d)[1], ], 
                    results_4_1d[, 1:10][-dim(results_4_1d)[1], ], 
                    results_5_1d[, 1:10][-dim(results_5_1d)[1], ], 
                    results_6_1d[, 1:10][-dim(results_6_1d)[1], ], 
                    results_7_1d[, 1:10])
    
    write.table(as.data.frame(results_1d[, 1:10]), "./DataHW1999/dataresults_1d.txt")
  }
  
}

##########################To obtain Fig_2a, 2b and 2c : 
{
  #parameters : (according to the article, in "Methods") 
  {
    r_2abc<-rep(1, 5)
    D_2abc<- 0.25
    m_2abc<-rep(D_2abc, 5)
    S_2abc<-c(6, 10, 14, 4, 9) 
    
    K_2abc <- as.matrix(read.table("./Matrix_K/K_23.table", sep = " ", numerals = "no.loss"))
    C_2abc <- as.matrix(read.table("./Matrix_C/C_23.table", sep = " ", numerals = "no.loss"))
    
    parameters_2abc<-makeparameters(r_2abc, D_2abc, m_2abc, S_2abc, K_2abc, C_2abc)#parameters matrix for Fig_2a, Fig_2b and Fig_2c
  }
  
  #initial state : 
  {
    R_0_2abc<-S_2abc
    names(R_0_2abc) <- c("R1", "R2", "R3", "R4", "R5")
    N_0_2abc<- 0.1+(1:5)/100
    names(N_0_2abc) <- c("N1", "N2", "N3", "N4", "N5")
    X_0_2abc<- c(N_0_2abc, R_0_2abc)
  }
  
  #times sequence for the output:
  times_2abc<-seq(0, 2000, by = D_2abc)
  
  #use of the ode function from the deSolve library
  results_2abc<-round(ode(X_0_2abc, times_2abc, dXt, parameters_2abc), digits = 4)
  
  
  #transforming the data to plot it
  {
    dataresults_2a<-as.data.frame(results_2abc[0:(300/D_2abc), -(7:11)])
    dataresults_2b<-as.data.frame(results_2abc[(1000/D_2abc):(2000/D_2abc), ])
    dataresults_2c<-data.frame(results_2abc[0:(300/D_2abc), 1], rowSums(results_2abc[0:(300/D_2abc), 2:6]))
    names(dataresults_2c)<-c("time", "Tot")
    
    write.table(dataresults_2a, "./DataHW1999/dataresults_2a.txt")
    write.table(dataresults_2b, "./DataHW1999/dataresults_2b.txt")
    write.table(dataresults_2c, "./DataHW1999/dataresults_2c.txt")
  
  }

}

##########################To obtain Fig 3a and 3b :
{
  #parameters : (according to the article, in "Methods") 
  {
    r_3ab = rep(1, 5)
    D_3ab = 0.25
    m_3ab = rep(D_3ab, 5)
    S_3ab = c(6, 10, 14, 4, 9)
    
    K_3ab <- as.matrix(read.table("./Matrix_K/K_23.table", sep = " ", numerals = "no.loss"))
    C_3ab <- as.matrix(read.table("./Matrix_C/C_23.table", sep = " ", numerals = "no.loss"))
    
    parameters_3ab <- makeparameters(r_3ab, D_3ab, m_3ab, S_3ab, K_3ab, C_3ab) 
  }  
  
  N_3ab0 <- 0.1+(1:5)/100
  R_3ab0 <- S_3ab
  X_3ab0 <- c(N_3ab0, R_3ab0)
  
  times_3ab = seq(0, 4000, D_3ab)
  
  #matrix the will recieve the results during each loop :
  results_3ab <- c() 
  results_3cd <- c()
  
  #loop for different values of K41
  for(K_41_j_3ab in seq(0.1, 0.5, 0.001)){#possibility to reduce the step in order to accelerate the calculus
    
    parameters_3ab[4, 1, 1] <- K_41_j_3ab#allocating the new value of K
    
    results_3abj<-round(ode(X_3ab0, times_3ab, dXt, parameters_3ab), digits = 4)
    
    #saving the values between t = 2,000 and t = 4,000 :
    results_3abj_u <- unique(results_3abj[(2000/D_3ab):(4000/D_3ab), 2])
    
    results_3ab <- rbind(results_3ab,cbind(rep(K_41_j_3ab,length(results_3abj_u)),results_3abj_u))
    
    
    
    #And the extremums : 
    Maxj <- max(results_3abj[(2000/D_3ab):(4000/D_3ab), 2])
    Minj <- min(results_3abj[(2000/D_3ab):(4000/D_3ab), 2])
    
    results_3cd <- rbind(results_3cd, c(K_41_j_3ab, Maxj, Minj))
  }
  
  #transformating the data to plot it
  {
    dataresults_3ab <- as.data.frame(results_3ab)
    dataresults_3cd <- as.data.frame(results_3cd)
    
    write.table(dataresults_3ab, "./DataHW1999/dataresults_3ab.txt", row.names = F, col.names = F)
    
    write.table(dataresults_3cd, "./DataHW1999/dataresults_3cd.txt", row.names = F, col.names = F)
    }
  
  }

##########################To obtain Fig 4a and 4b :
{
#parameters : (according to the article, in "Methods") 
  {
    r_4ab<-rep(1, 12)
    D_4ab<- 0.25
    m_4ab<-rep(D_4ab, 12)
    S_4ab<-c(6, 10, 14, 4, 9)
    
    K_4ab <- as.matrix(read.table("./Matrix_K/K_4.table", sep = " ", numerals = "no.loss"))
    C_4ab <- as.matrix(read.table("./Matrix_C/C_4.table", sep = " ", numerals = "no.loss"))
    
    parameters_4ab<-makeparameters(r_4ab, D_4ab, m_4ab, S_4ab, K_4ab, C_4ab)#parameters matrix for Fig_2a, Fig_2b and Fig_2c
  }
  
  #AS for _1c and _1d :
  #In order to add new species mid-time, the resolution is split every time new species are added : 
  #The ode function is used on a certain period of time between two addition. 
  #When new species must be added, the calculus is stopped. 
  #The final values of the last resolution will be used as the new initial values of the next, with also the newly added species.
  
  {
    #First period (Species 1-2-3-4-5)
    {
      #initial state : 
      {     
        R_4ab10<-S_4ab
        names(R_4ab10) <- c(paste("R", 1:5, sep = ""))
        N_4ab10<- c(0.1+(1:5)/100, rep(0, 7))
        names(N_4ab10) <- c(paste("N", 1:12, sep = ""))
        X_4ab10<- c(N_4ab10, R_4ab10)
      }
    
      #times sequence for the output:
      times_4ab1<-seq(0, 1000, by = D_4ab)
      
      #use of the ode function from the deSolve library
      results_4ab1<-round(ode(X_4ab10, times_4ab1, dXt, parameters_4ab), digits = 4)
    }
      
    #Second period (Species 1-2-3-4-5-6-7-8)
    {
      #initial state : 
      X_4ab20<-results_4ab1[dim(results_4ab1)[1], 2:18]
      X_4ab20[6:8]<- c(0.1, 0.1, 0.1)
      
      #times sequence for the output:
      times_4ab2<-seq(1000, 3000, by = D_4ab)
      
      #use of the ode function from the deSolve library
      results_4ab2<-round(ode(X_4ab20, times_4ab2, dXt, parameters_4ab), digits = 4)
    }
      
    #Third period (Species 1-2-3-4-5-6-7-8-9-10)
    {
      #initial state : 
      X_4ab30<-results_4ab2[dim(results_4ab2)[1], 2:18]
      X_4ab30[9:10]<- c(0.1, 0.1)
      
      #times sequence for the output:
      times_4ab3<-seq(3000, 5000, by = D_4ab)
      
      #use of the ode function from the deSolve library
      results_4ab3<-round(ode(X_4ab30, times_4ab3, dXt, parameters_4ab), digits = 4)
    }
      
    #Fourth period (Species 1-2-3-4-5-6-7-8-9-10-11-12)
    {
      #initial state : 
      X_4ab40<-results_4ab3[dim(results_4ab3)[1], 2:18]
      X_4ab40[11:12]<- c(0.1, 0.1)
      
      #times sequence for the output:
      times_4ab4<-seq(5000, 10000, by = D_4ab)
      
      #use of the ode function from the deSolve library
      results_4ab4<-round(ode(X_4ab40, times_4ab4, dXt, parameters_4ab), digits = 4)
    }    
      
  }
    
  #transforming the data to plot it
  {
  results_4ab<-rbind(results_4ab1[, 1:13][-dim(results_4ab1)[1], ], 
                     results_4ab2[, 1:13][-dim(results_4ab2)[1], ], 
                     results_4ab3[, 1:13][-dim(results_4ab3)[1], ], 
                     results_4ab4[, 1:13])
            
    
  write.table(as.data.frame(results_4ab), './DataHW1999/dataresults_4ab.txt')
  }
  
}




