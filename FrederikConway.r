conway <- function(type = "rPentomino", rule = "23/3", ...) {
  x <- switch(type,
         "random" = {
           Zufallsmatrix(...)
         },
         "gleiter" = {
           gleiter()
         },
         "gleiter4" = {
           gleiter4()
         },
         "explusion" = {
           explusion()
         },
         "rPentomino" = {
           rPentomino()
         },
         "custom" = {
           matrix(...)
         })
  C <- list(field = x, rules = rule)
  class(C) <- c("conway")
  return(C)
}

#' create a random start matrix for CGoL
#' 
#' @param x number of rows
#' @param y number of columns
#' @param p probability that field value in the matrix is 0, else 1
#' @param type the number type of the matrix fields
#' @return a matrix with \code{x} rows and \code{y} columns, with random values {0, 1} of type \code{type} based on the probability \code{p}
#' @examples
#' Zufallsmatrix(10, 10, 0.5)
Zufallsmatrix <- function(x = 10, y = 10, p = 0.5, type = "double"){
  
  # Zufallsmatrix erzeugt eine x*y große Matrix deren Einträge mit einer
  # Warscheinlichkeit p 1./1/T und sonst 0./0/F sind
  
  if(type == "double"){
    M <- matrix(as.double(runif(x*y)<=p),x,y) 
  }
  if(type == "int"){
    M <- matrix(as.integer(runif(x*y)<=p),x,y) 
  }else{
    M <- matrix((runif(x*y)<=p),x,y) 
  }
  
  class(M) <- "conway"
  
  return(M)
}

alsRegelliste <- function(regelstring){
  
  # Übersetzt einen Regelstring in eine Liste mit den Regeln
  # Hängt Zahlen in die erste Liste ein bis ein '/' kommt
  # Hängt darauf alle weitern in die zweite Liste 
  
  regelliste = list(BESTEHEN = NULL,ENTSTEHEN = NULL)
  WO <- 1
  for(i in strsplit(regelstring,"")[[1]]){
    if(i != '/'){
      regelliste[[WO]] <- unlist(append(regelliste[[WO]],as.integer(i)))
    }else{
      WO = 2
    }
  }
  return(regelliste)
}

#' 
#' 
#' @param 
#' @return 
#' @examples
Nachbarn <- function(con){
  
  # Gibt eine Matrix der Größe von M+2 zurück welche die Anzahl an Nachbar enthält
  
  m <- array(0, dim(con$field)+2)
  
  m[1:nrow(con$field),1:ncol(con$field)] <-m[1:nrow(con$field),1:ncol(con$field)]+con$field
  m[1:nrow(con$field),1:ncol(con$field)+1] <-m[1:nrow(con$field),1:ncol(con$field)+1]+con$field
  m[1:nrow(con$field),1:ncol(con$field)+2] <-m[1:nrow(con$field),1:ncol(con$field)+2]+con$field
  
  m[1:nrow(con$field)+1,1:ncol(con$field)] <-m[1:nrow(con$field)+1,1:ncol(con$field)]+con$field
  
  m[1:nrow(con$field)+1,1:ncol(con$field)+2] <-m[1:nrow(con$field)+1,1:ncol(con$field)+2]+con$field
  
  m[1:nrow(con$field)+2,1:ncol(con$field)] <-m[1:nrow(con$field)+2,1:ncol(con$field)]+con$field
  m[1:nrow(con$field)+2,1:ncol(con$field)+1] <-m[1:nrow(con$field)+2,1:ncol(con$field)+1]+con$field
  m[1:nrow(con$field)+2,1:ncol(con$field)+2] <-m[1:nrow(con$field)+2,1:ncol(con$field)+2]+con$field
  return(m)
}


#' 
#' 
#' @param 
#' @return 
#' @examples
Verkleinern <- function(M){
  
  # Entfernt Ränder ohne lebende Zellen bis zu einer Mindestgröße von 2 X 2 
  # damit die Matrix erhalten bleibt
  
  while(nrow(M) > 2 & !any(M[1,])){
    M<-M[-1,]
  }
  while(ncol(M) > 2 & !any(M[,1]) ){
    M<-M[,-1]
  }
  while(nrow(M) > 2 & !any(M[nrow(M),])){
    M<-M[-nrow(M),]
  }
  while(ncol(M) > 2 & !any(M[,ncol(M)])){
    M<-M[,-ncol(M)]
  }
  return(M)
}


#' 
#' 
#' @param 
#' @return 
#' @examples
update.conway <- function(con){

  # Update bekommt eine Matrix und einen Regelsatz
  # Update liefert eine Upgedatete Matrix
  #
  # M =  Matrix
  #
  # R = Regeln "Bestehen/Entstehen"
  
  if(is.character(con$rules)){
    REGELN = alsRegelliste(con$rules)
  }else{
    REGELN = con$rules
  }
  
  AnzahlNachbarn = Nachbarn(con)
  
  m <- array(0,dim(con$field)+2)              # Alte Matrix mit der Dimension der Nachtbarmatrix
  m[-c(1,nrow(m)),-c(1,ncol(m))]<- con$field  
  
  Ergebnis <- array(0,dim(con$field)+2)
  Ergebnis <- apply(((AnzahlNachbarn %in% REGELN$BESTEHEN )& m | AnzahlNachbarn %in% REGELN$ENTSTEHEN),c(1,2),as.integer)
  Ergebnis <- Verkleinern(Ergebnis)
  con$field <- Ergebnis
  return(con)
}


#' 
#' 
#' @param 
#' @return 
#' @examples
plot.conway <- function(con){
  par(bg = 'black')
  image(con$field,col = gray.colors(2,0,1),
        asp=1,
        x = seq(0,nrow(con$field)-1,length.out = nrow(con$field)),
        y=seq(0,ncol(con$field)-1,length.out = ncol(con$field)),
        xaxt='n',yaxt = 'n' , ann = FALSE,bty="n")
  mtext(side = 1, text = nrow(con$field), line = 1, col = "white")
  mtext(side = 2, text = ncol(con$field), line = 1, col = "white")
  
}


summary.conway <- function(x, ...) {
  rows <- nrow(x$field)
  cols <- ncol(x$field)
  n_life <- sum(x$field)
  n_dead <- rows * cols - n_life
  print(paste("rows = ", rows))
  print(paste("columns = ", cols))
  print(paste("living cells = ", n_life))
  print(paste("dead cells = ", n_dead))
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
#x <- Zufallsmatrix(50,100,0.1)
x <- conway("random", p = 0.2, x=100)
#y = (x + Update(x))/2
#x = Update(x)

#plotConway(x)
#x = Update(x)
#x = array(c(1,0),c(51,51))
for(i in 1:1000){
  plot(x)
  x <- update(x)#,R = "12345/3	")
  Sys.sleep(0.2)
}

#plotConway(array(runif(100,min = 0,max = 2),c(1000,10)))
