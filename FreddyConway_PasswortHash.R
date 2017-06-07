Zufallsmatrix <- function(x,y,p, type = "double"){
  # Zufallsmatrix erzeugt eine x*y große Matrix deren Einträge mit einer
  # Warscheinlichkeit p 1 und sonst 0 sind
  if(type == "double"){
    M <- matrix(as.double(runif(x*y)<=p),x,y) 
  }else{
    M <- matrix(as.integer(runif(x*y)<=p),x,y) 
  }
  
  return(M)
}

alsRegelliste <- function(regelstring){
  # Übersetzt einen Regelstring in eine Liste mit den Regeln
  #
  # Hängt Zahlen in die erste Liste ein bis ein '/' kommt
  # Hängt darauf alle weitern in die zweite Liste 
  regelliste = list(BESTEHEN = NULL,ENTSTEHEN = NULL)
  WO <- 1
  for(i in strsplit(regelstring,"")[[1]]){
    if(i != '/'){
      regelliste[[WO]] <- unlist(append(regelliste[WO],as.integer(i)))
    }else{
      WO = 2
    }
  }
  return(regelliste)
}


Nachbarn <- function(M){
  # Gibt eine Matrix der Größe von M+2 zurück welche die Anzahl an Nachbar enthält
  m <- array(0, dim(M)+2)
  
  m[1:nrow(M),1:ncol(M)] <-m[1:nrow(M),1:ncol(M)]+M
  m[1:nrow(M),1:ncol(M)+1] <-m[1:nrow(M),1:ncol(M)+1]+M
  m[1:nrow(M),1:ncol(M)+2] <-m[1:nrow(M),1:ncol(M)+2]+M
  
  m[1:nrow(M)+1,1:ncol(M)] <-m[1:nrow(M)+1,1:ncol(M)]+M
  
  m[1:nrow(M)+1,1:ncol(M)+2] <-m[1:nrow(M)+1,1:ncol(M)+2]+M
  
  m[1:nrow(M)+2,1:ncol(M)] <-m[1:nrow(M)+2,1:ncol(M)]+M
  m[1:nrow(M)+2,1:ncol(M)+1] <-m[1:nrow(M)+2,1:ncol(M)+1]+M
  m[1:nrow(M)+2,1:ncol(M)+2] <-m[1:nrow(M)+2,1:ncol(M)+2]+M
  
  return(m)
}

Verkleinern <- function(M){
  # Entfernt Ränder ohne lebende Zellen
  while(!any(M[1,])){
    M<-M[-1,]
  }
  while(!any(M[,1])){
    M<-M[,-1]
  }
  while(!any(M[nrow(M),])){
    M<-M[-nrow(M),]
  }
  while(!any(M[,ncol(M)])){
    M<-M[,-ncol(M)]
  }
  return(M)
}

Update <- function(M,R = "23/3"){
  # Update bekommt eine Matrix und einen Regelsatz
  # Update liefert eine Upgedatete Matrix
  #
  # M =  Matrix
  #
  # R = Regeln "überlebt/geboren"
  
  if(is.character(R)){
    REGELN = alsRegelliste(R)
  }else{
    REGELN = R
  }
  
  AnzahlNachbarn = Nachbarn(M)
  
  m <- array(0,dim(M)+2)
  m[-c(1,nrow(m)),-c(1,ncol(m))]<- M
  
  
  Ergebnis <- array(0,dim(M)+2)
  Ergebnis <- apply(((AnzahlNachbarn %in% REGELN$BESTEHEN )& m | AnzahlNachbarn %in% REGELN$ENTSTEHEN),c(1,2),as.integer)
  Ergebnis <- Verkleinern(Ergebnis)
  return(Ergebnis)
  
}

plotConway <- function(M){
  par(bg = 'gray')
  image(M,col = gray.colors(2,0,1),
        asp=1,
        x = seq(0,nrow(M)-1,length.out = nrow(M)),
        y=seq(0,ncol(M)-1,length.out = ncol(M))#,
        #xaxt='n',yaxt = 'n' , ann = FALSE,bty="n"
        )
}


PasswortHash <- function(S){
  I <- utf8ToInt(S)
  B <- intToBits(I)
  A <- as.array(as.integer(B))
  d <- ceiling(sqrt(length(A)))

  while(floor(sqrt(length(A)))<d){
    A <- c(A,0)
  }

  M <- matrix(A,d,d)
  
  
  #AUSSORTIERT: 1/3,2/3,3/3,13/3
  R <- c("02468/02468",
         "3/123","3/23","3/13","3/12","3/2","3/1",
         "2/123","2/23","2/12","2/13","2/2","2/1",
         "1/123","1/13","1/12","1/23","1/2","1/1",
         "24/3","24/2","24/1","24/12",
         "23/3","23/2","23/1",
         "12/1","12/2","12/3",
         "13/1","13/2",
         "245/3",
         "125/36",
         "1357/1357",
         "12345/3")
  
  Run(M,R[1],d)
}


########################
#                      #
#    StartBeispiele    #
#                      #
########################


gleiter = function(){
  return(matrix(c(1,1,1,1,0,0,0,1,0),3,3,byrow = F))
}

gleiter4 = function(){
  x = gleiter()
  x = cbind(x,0)
  x = rbind(x,0)
  x = cbind(x,x[,c(4,3,2,1)])
  x = rbind(x,x[c(4,3,2,1),])
  return(x)
}

explusion = function(){
  x =  matrix(c(1,1,1,0,1,1,1,1,0,0,0,0,0,1,1,1,1,0,1,1,1),3,7,byrow = TRUE)
  return(x)
}

rPentomino = function(){
  return(matrix(c(0,1,0,1,1,1,1,0,0),3,3))
}
########################
#                      #
#   Kleines Beispiel   #
#                      #
########################
x = Zufallsmatrix(50,100,0.1)
#y = (x + Update(x))/2
#x = Update(x)

#plotConway(x)
#x = Update(x)
#x = array(c(1,0),c(51,51))
Run <- function(x = Zufallsmatrix(64,64,0.2), Rule = "23/3", Länge = 100){
  plotConway(x)
  for(i in 1:Länge){
    x = Update(x, R = Rule)#,R = "12345/3")
    plotConway(x)
    Sys.sleep(0.2)
  }
  cat(dim(x))
  #return(x)
}

#plotConway(array(runif(100,min = 0,max = 2),c(10,10)))

#Maxtix zu klein
#Abmessungen beim Plot
#Dokumentatio