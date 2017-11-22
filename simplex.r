tableau = matrix(
  c(
    7,11,1,0,0,0,0,77,10,8,0,1,0,0,0,80,1,0,0,0,1,0,0,9,0,1,0,0,0,1,0,6,-150,-175,0,0,0,0,1,0
  ),
  nrow = 5,
  ncol = 8,
  byrow = T
)
setUpTableau <- function(maximize){
  z <- function(x,y) 150*x + 175*y
  objective_fxn <- "p = (1/2)*x + 3*y + 1*z + 4*w"
  constraints = list()
  slacks = list()
  constraints[1] <- "1*x + 1*y + 1*z + 1*w <= 40"

  constraints[2] <- "2*x + 1*y - 1*z - 1*w >= 10"
  constraints[3] <- "1*w - 1*y >= 12"
  
  for(i in 1:length(constraints)){
    constraints[i] <- gsub("-","+-",constraints[i])
    constraints[i] <- gsub(" ","",constraints[i])
  }
  #print(constraints)
  countCharOccurrences <- function(char, s) {
    s2 <- gsub(char,"",s)
    return (nchar(s) - nchar(s2))
  }
  #generate slack variables
  for(i in 1:length(constraints)){
    slacks[i] <- paste("s",i,sep="")
  } 
  
  #fix constraints
  for(i in 1:length(constraints)){
    if(grepl("<=",constraints[i])){
      new = strsplit(as.character(constraints[i]),as.character("<="))
      constraints[i] <- paste(new[[1]][1],"+1 *",slacks[i],"=",new[[1]][2],sep="")
      constraints[i] <- gsub(" ","",constraints[i])
    }
    else if(grepl(">=",constraints[i])){
      new = strsplit(as.character(constraints[i]),as.character(">="))
      constraints[i] <- paste(new[[1]][1],"+-1 *",slacks[i],"=",new[[1]][2],sep="")
      constraints[i] <- gsub(" ","",constraints[i])    
    }
  }
  
  
  objective_initial <- strsplit(as.character(objective_fxn),as.character("="))
  objective_variable <- objective_initial[[1]][1] #z
  objective_variable <- gsub(" ","",objective_variable)
  objective_function <- objective_initial[[1]][2] #150x + 175y
  coefficients <- strsplit(as.character(objective_function),as.character("[\\+|\\-]"))
  coefficients <- unlist(coefficients)
  #print(coefficients)
  variable_count <- length(coefficients)
  variables = list()
  for(a in 1:variable_count){
    split = strsplit(as.character(coefficients[a]),as.character("\\*"))
    split <- unlist(split)
    variables[a] <- gsub(" ","",split[2])
  }
  
  empty <- matrix(
    c(0.0),
    nrow = length(slacks)+1,
    ncol = length(variables)+length(slacks)+2,
    byrow=T
  )
  
  #set up fucking tableau table
  
  #print(constraints)
  
  colnames = list()
  rownames = list()
  #rownames for tableau
  for(i in 1:length(slacks)){
    rownames[i] <- slacks[i]
  }
  rownames[nrow(empty)] <- objective_variable
  rownames(empty) <- rownames  
  
  #colnames for tableau
  for(i in 1:variable_count){
    colnames[i] <- variables[i]
  }
  for(i in 1:length(slacks)){
    colnames[i+variable_count] <- slacks[i]
  }
  colnames[length(colnames)+1] <- objective_variable
  colnames[length(colnames)+1] <- "Sol"
  colnames(empty) <- colnames  
  #print(constraints)
  for(i in 1:length(constraints)){
    dummy <- strsplit(as.character(constraints[i]),as.character("="))
    lhs <- strsplit(as.character(dummy[[1]][1]),as.character("[\\+]"))
    
    lhs <- unlist(lhs)
    #print(lhs)
    for(j in 1:length(lhs)){
      #print(lhs[j])
      dissect <- strsplit(as.character(lhs[j]),as.character("\\*"))
      dissect <- unlist(dissect)
      dissect[1] <- gsub("[\\(|\\)]","",dissect[1])
      #print(dissect[1])
      empty[i,dissect[2]] <- as.numeric(dissect[1])
      #print(dissect[2])
    }
    empty[i,"Sol"] <- as.numeric(dummy[[1]][2])
  }
  
  obj_coeffs <- strsplit(as.character(objective_function),as.character("\\+"))
  obj_coeffs <- unlist(obj_coeffs)
  for(j in 1:length(obj_coeffs)){
    dissect <- strsplit(as.character(obj_coeffs[j]),as.character("\\*"))
    dissect <-unlist(dissect)
    dissect[2] <- gsub(" ","",dissect[2])
    dissect[1] <- gsub("[\\(|\\)]","",dissect[1])
    dissect[1] = eval(parse(text=dissect[1]))
    if(maximize == T){
      empty[objective_variable,dissect[2]] <- -1*as.numeric(dissect[1])  
    }else{
      empty[objective_variable,dissect[2]] <- -1*-1*as.numeric(dissect[1])
    }
    
  }
  empty[objective_variable,objective_variable] <- 1
  #print(empty)
  return(empty)
}


#colnames(tableau) <- colnames
#rownames(tableau) <- rownames

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
  while(checkBottomRow(tableau)){
    if(checkBottomRow(tableau) == F){
      break
    }
    print(tableau)
    min = 0
    minRow = 0
    minCol = 0
    pivotElement = 0
    for(i in 1:(ncol(tableau)-1)){ #get column with smallest negative value
      if(min > tableau[nrow(tableau),i]){
        min <-tableau[nrow(tableau),i]
        minCol <- i

      }
    }
    print(paste("min element col",min))
    print(paste("Col",minCol))
    minRatio = Inf #initial ratio
    minRow = 0
    
    for(i in 1:(nrow(tableau)-1)){
      tr = ratio(tableau[i,ncol(tableau)],tableau[i,minCol])
      if(tr > 0 && tr < minRatio){
        minRatio = tr
        minRow = i
      }
    }
    
    print(paste("min row",minRow))
    
    
    pivotElement <- tableau[minRow,minCol]
    print(paste("pivot element",pivotElement))
    if(pivotElement != 1){
      #normalize
       for(i in 1:(ncol(tableau))){
        tableau[minRow,i] <- tableau[minRow,i]/pivotElement
      }
    }
    #print(tableau)
    replace = matrix(nrow = 1, ncol=ncol(tableau))

    for(i in 1:(nrow(tableau))){
      if(i != minRow){
        for(j in 1:ncol(tableau)){
          answer <- tableau[i,j] - (tableau[i,minCol] * tableau[minRow,j])
          replace[1,j] <- answer
        }
      tableau[i,] <- replace[1,]
      }
    }
    
    
  #print(tableau)
  }
  print(tableau)
}