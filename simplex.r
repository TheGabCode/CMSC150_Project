tableau = matrix(
  c(
    7,11,1,0,0,0,0,77,10,8,0,1,0,0,0,80,1,0,0,0,1,0,0,9,0,1,0,0,0,1,0,6,-150,-175,0,0,0,0,1,0
  ),
  nrow = 5,
  ncol = 8,
  byrow = T
)

colnames = list()
rownames = list()
colnames[1] <- "x1"
colnames[2] <- "x2"
colnames[3] <- "s1"
colnames[4] <- "s2"
colnames[5] <- "s3"
colnames[6] <- "s4"
colnames[7] <- "z"
colnames[8] <- "Sol"
rownames[1] <- "s1"
rownames[2] <- "s2"
rownames[3] <- "s3"
rownames[4] <- "s4"
rownames[5] <- "z"

colnames(tableau) <- colnames
rownames(tableau) <- rownames

checkBottomRow <- function(tableau){
  stillNegative <- F
  for(i in 1:(ncol(tableau)-1)){
    if(tableau[nrow(tableau),i] < 0){
      stillNegative <- T
      break
    }
  }
  return(stillNegative)
}

ratio <- function(a,b){
  return(a/b)
}

simplex <- function(tableau){
  while(checkBottomRow(tableau) == T){
    min = 0
    minRow = 0
    minCol = 0
    pivotElement = 0
    for(i in 1:(ncol(tableau)-1)){
      if(min > tableau[nrow(tableau),i]){
        min <-tableau[nrow(tableau),i]
        minCol <- i
      }
    }
    minRatio = ratio(tableau[1,ncol(tableau)],tableau[1,minCol]) ##initial ratio
    minRow = 1
    for(i in 2:(nrow(tableau)-1)){
      if(ratio(tableau[i,ncol(tableau)],tableau[i,minCol]) < minRatio && ratio(tableau[i,ncol(tableau)],tableau[i,minCol]) > 0){
        minRatio <- ratio(tableau[i,ncol(tableau)],tableau[i,minCol])
        minRow <- i
      }
    }
  pivotElement <- tableau[minRow,minCol]  
  break
  }
  print(pivotElement)
  print(minRatio)
  print(minRow)
  
  
}