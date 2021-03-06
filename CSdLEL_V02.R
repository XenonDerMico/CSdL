ZufallStart <- function(x,y,p){
  # for(i in 1:dim(M)[1]){
  #   for(j in 1:dim(M)[2]){
  #     rn <- runif(1,0,100)
  #     if(rn <= p){
  #       M[i,j] <- 1
  #     }else{
  #       M[i,j] <- 0
  #     }
  #   }
  # }
  #return(M)
  M <- matrix(unlist(lapply(runif(x*y)<=(p/100),function(x){if(x){1.0}else{0.0}})),y,x)
  #cat(typeof(M),"\n")
  return(M)
  #return(apply(Zufall(p),c(1,2),M))
}

# Schwellwert <- function(x){
#   if(x){
#     return(1)
#   }else{
#     return(0)
#   }
# }

# Zufall <- function(p){
#   rn <- runif(1,0,100)
#   if(rn <= p){
#     return(1)
#   }else{
#     return(0)
#   }
# }

LinksVerschiebung <- function(M){
  #ErsteSpalte <- M[,1]
  #RestSpalte <- M[,2:dim(M)[2]]
  #M <- cbind(RestSpalte,ErsteSpalte)
  #return(M)
  
  return(cbind(M[,2:dim(M)[2]],M[,1]))
}

RechtsVerschiebung <- function(M){
  #ErsteSpalte <- M[,dim(M)[2]]
  #RestSpalte <- M[,1:dim(M)[2]-1]
  #M <- cbind(ErsteSpalte,RestSpalte)
  #return(M)
  
  return(cbind(M[,dim(M)[2]],M[,1:dim(M)[2]-1]))
}

UntenVerschiebung <- function(M){
  #ErsteSpalte <- M[dim(M)[1],]
  #RestSpalte <- M[1:dim(M)[1]-1,]
  #M <- rbind(ErsteSpalte,RestSpalte)
  #return(M)
  
  return(rbind(M[dim(M)[1],],M[1:dim(M)[1]-1,]))
}

ObenVerschiebung <- function(M){
  #ErsteSpalte <- M[1,]
  #RestSpalte <- M[2:dim(M)[1],]
  #M <- rbind(RestSpalte,ErsteSpalte)
  #return(M)
  
  return(rbind(M[2:dim(M)[1],],M[1,]))
}

ObenLinksVerschiebung <- function(M){
  return(LinksVerschiebung(ObenVerschiebung(M)))
}

ObenRechtsVerschiebung <- function(M){
  return(RechtsVerschiebung(ObenVerschiebung(M)))
}

UntenLinksVerschiebung <- function(M){
  return(LinksVerschiebung(UntenVerschiebung(M)))
}

UntenRechtsVerschiebung <- function(M){
  return(RechtsVerschiebung(UntenVerschiebung(M)))
}

Vergroessern <- function(M){
  senkrecht <- matrix(data = 0, nrow = 1, ncol = dim(M)[2])
  M <- rbind(senkrecht,M,senkrecht)
  wagerecht <- matrix(data = 0, nrow = dim(M)[1], ncol = 1)
  M <- cbind(wagerecht,M,wagerecht)
  return(M)
}

Verkleinern <- function(M){
  schritte <- 2
  schwellwert <- 0.1
  while(schritte > 0){
    if(max(M[1,]) <= schwellwert){
      M <- M[2:dim(M)[1],]
    }
    if(max(M[dim(M)[1],]) <= schwellwert){
      M <- M[1:dim(M)[1] - 1,]
    }
    if(max(M[,1]) <= schwellwert){
      M <- M[,2:dim(M)[2]]
    }
    if(max(M[,dim(M)[2]]) <= schwellwert){
      M <- M[,1:dim(M)[2] - 1]
    }
    schritte <- schritte - 1
  }
  return(M)
}

SummenMatrix <- function(M){
  #OL <- floor(ObenLinksVerschiebung(M))
  #O <- floor(ObenVerschiebung(M))
  #OR <- floor(ObenRechtsVerschiebung(M))
  #L <- floor(LinksVerschiebung(M))
  #R <- floor(RechtsVerschiebung(M))
  #UL <- floor(UntenLinksVerschiebung(M))
  #U <- floor(UntenVerschiebung(M))
  #UR <- floor(UntenRechtsVerschiebung(M))
  #MS <- OL+O+OR+L+R+UL+U+UR
  #return(MS)
  #print(typeof(M))
  #print((UntenRechtsVerschiebung(M)))
  return(floor(ObenLinksVerschiebung(M))
        +floor(ObenVerschiebung(M))
        +floor(ObenRechtsVerschiebung(M))
        +floor(LinksVerschiebung(M))
        +floor(RechtsVerschiebung(M))
        +floor(UntenLinksVerschiebung(M))
        +floor(UntenVerschiebung(M))
        +floor(UntenRechtsVerschiebung(M)))
}

Update <- function(M, ueberleben, geburt){
  
  M <- Vergroessern(M)
  #print(M)
  MS <- SummenMatrix(M)
  
  for(i in 1:dim(M)[1]){
    for(j in 1:dim(M)[2]){
      if(M[i,j]<1){
        if(max(MS[i,j]==geburt)){
          M[i,j] <- 1
        }else{
          M[i,j] <- M[i,j]/2
        }
      }else if(M[i,j]==1){
        if(!max(MS[i,j]==ueberleben)){
          M[i,j] <- M[i,j]/2
        }else{
          
        }
      }else{
        warning("ERROR WEDER TOD NOCH LEBENDIG")
      }
    }
  }
  M <- Verkleinern(M)
}

Run <- function(s,x,y,p,M = "NICHT_DEF"){
  grau = gray((0:256)/256)
  ueberleben <- c(2,3)
  geburt <- c(3)

  if(M == "NICHT_DEF"){
    M <- ZufallStart(x,y,p)
  }
  #print(typeof(M))
  
  #M <- matrix(0,y,x)
  #M <- ZufallStart(M,p)
  Warten(0.05)
  for(ii in 1:s){
    
    M <- Update(M,ueberleben, geburt)
    
    
    cat(dim(M),"\t",ii, "\t", round(((sum(M)*100)/(dim(M)[1]*dim(M)[2])),digits = 2),"%\n")
    image(M, col = grau,asp=1,x = seq(0,dim(M)[1]-1,length.out = dim(M)[1]),
                              y = seq(0,dim(M)[2]-1,length.out = dim(M)[2]))
    Warten(1/25)
  }
}

Warten <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

Start <- function(){
  #Run(Frams,yAchse,xAchse,StartLeben%)
  #Rprof(tmp <- tempfile())
  #A <- matrix(round(runif(n = 64,min = 0,max = 1)),8,8)
  #Run(s = 100,M = A)
  #Warten(5)
  Run(s = 100,x = 64,y = 128,p = 20)
  #Warten(5)
  #Run(s = 100,M = A)
  #Rprof()
  #summaryRprof(tmp)
  #require(profr)
  #require(ggplot2)
  #require(proftools)
  #plotProfileCallGraph(readProfileData(tmp),score = "total")
  #return(tmp)
}