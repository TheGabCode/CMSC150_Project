library(shiny)
library(shinythemes)
library(xtable)
objective_fxn <- "p = (1/2)*x + 3*y + 1*z + 4*w"
constraints = list()
constraints[1] <- "1*x + 1*y + 1*z + 1*w <= 40"
constraints[2] <- "2*x + 1*y - 1*z - 1*w >= 10"
constraints[3] <- "-1*w - 1*y >= 12"

setUpTableau <- function(objective_fxn,constraints,maximize){
  z <- function(x,y) 150*x + 175*y
  slacks = list()
  for(i in 1:length(constraints)){
    constraints[i] <- gsub("-","+-",constraints[i])
    constraints[i] <- gsub(" ","",constraints[i])
  }
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

  colnames = list()
  rownames = list()
  #rownames for tableau
  for(i in 1:length(slacks)){
    rownames[i] <- slacks[i]
  }
  rownames[nrow(empty)] <- objective_variable
  rownames(empty) <- rownames  
  

  for(i in 1:variable_count){
    colnames[i] <- variables[i]
  }
  for(i in 1:length(slacks)){
    colnames[i+variable_count] <- slacks[i]
  }
  colnames[length(colnames)+1] <- objective_variable
  colnames[length(colnames)+1] <- "Sol"
  colnames(empty) <- colnames  
  #+-1*w+-1*y+-1*s1=12
  for(i in 1:length(constraints)){
    dummy <- strsplit(as.character(constraints[i]),as.character("="))
    lhs <- strsplit(as.character(dummy[[1]][1]),as.character("[\\+]"))
    lhs <- unlist(lhs)
    remove <- c("\n")
    lhs <- setdiff(lhs,remove)
    print("nani")
    print(lhs)
    for(j in 1:length(lhs)){
      dissect <- strsplit(as.character(lhs[j]),as.character("\\*"))
      dissect <- unlist(dissect)
      dissect[1] <- gsub("[\\(|\\)]","",dissect[1])
      empty[i,dissect[2]] <- as.numeric(dissect[1])
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
  return(empty)
}


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

checkBottomRowNegative <- function(tableau){
  stillPositive <- F #still positive exists
  for(i in 1:(ncol(tableau)-1)){
    if(tableau[nrow(tableau),i] > 0){
      stillPositive <- T
      break
    }
  }
  return(stillPositive)
}

ratio <- function(a,b){
  return(a/b)
}

simplex <- function(tableau,maximize){
  mat <- list()
  sol <- list()
  count <- 1
  while(T){
    if(checkBottomRow(tableau) == F){
      if(count == 1){
        mat[[1]] <- tableau
        sol[[1]] <- findSolution(tableau)
        
      }
      break
    }
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
    minRatio = Inf #initial ratio
    minRow = 0
    
    for(i in 1:(nrow(tableau)-1)){
      if(tableau[i,minCol] != 0 && ratio(tableau[i,ncol(tableau)],tableau[i,minCol]) > 0 && minRatio == 0){
        minRatio <- ratio(tableau[i,ncol(tableau)],tableau[i,minCol])
        minRow <- i
      }
      else if(tableau[i,minCol] != 0 && ratio(tableau[i,ncol(tableau)],tableau[i,minCol]) > 0 && ratio(tableau[i,ncol(tableau)],tableau[i,minCol]) < minRatio){
        minRatio <- ratio(tableau[i,ncol(tableau)],tableau[i,minCol])
        minRow <- i
      }
    }
    

    
    pivotElement <- tableau[minRow,minCol]
    if(pivotElement != 1){
      for(i in 1:(ncol(tableau))){
        tableau[minRow,i] <- tableau[minRow,i]/pivotElement
      }
    }
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
    mat[[count]] <- tableau
    sol[[count]] <- findSolution(tableau)
    #print(findSolution(tableau))
#    print(tableau)
#    print(findSolution(tableau))
    count <- count + 1
  }

  return(mat)
}

mainSimplex <- function(objective_fxn,constraints,maximize){
  t<-setUpTableau(objective_fxn,constraints,maximize)
  tabs <- simplex(t)
  #print(tabs)
  return(tabs)
  
}

findSolution <- function(tableau){
  #print(tableau)
  sol <- list()
  for(i in 1:(ncol(tableau)-1)){
    one_count <- 0
    streak = T
    for(j in 1:nrow(tableau)){
      if(tableau[j,i] != 1.0){
        if(tableau[j,i] != 0){
            sol[i] <- paste(colnames(tableau)[i],"= 0")
            streak = F
            break
        }

      }else if(tableau[j,i] == 1 && one_count == 1){
        sol[i] <- paste(colnames(tableau)[i],"= 0")
        streak = F
        break
      }
      else{
        if(tableau[j,i] == 1){
          one_count <- one_count + 1
          sol[i] <- paste(colnames(tableau)[i],"=",as.character(tableau[j,"Sol"]))
        }

      }
    }
  }
 
  return(unlist(sol))
  }


ui <- fluidPage(theme=shinytheme("cerulean"),headerPanel('Ultimate Optimizer'),
                textInput(inputId = "objFxn",label = "Input objective function here...",value=" "),
                textAreaInput(inputId = "constraints",label = "Constraints here...",width="400px",height="150px"),
                radioButtons(inputId = "choice",label="???imize",choices = c("Max" = T,"Min" = F)),
                actionButton(inputId = "do_it",label = "Optimize"),
                uiOutput("tables"),
                uiOutput("texts"))

server <- function(input, output) {
  final = matrix()
  sol = list()
  constraints <- list()
  observeEvent(input$do_it, {
    split_constraints <- strsplit(input$constraints,",")
    split_constraints <- unlist(split_constraints)

    if(input$objFxn != " " && length(split_constraints > 0)){
      #print(input$objFxn)
      final <- mainSimplex(input$objFxn,split_constraints,input$choice)
      print(final)
      for(a in 1:length(final)){
        sol[[a]] <- findSolution((final[[a]]))
        print(toString(sol[[a]]))
      }
      output$texts <- renderUI({
        texts_out_list <- lapply(1:length(sol),function(i){
          textname <- paste("text",i)
          textOutput(textname)
        })
        do.call(tagList,texts_out_list)
      })
      
      output$tables <- renderUI({
        table_out_list <- lapply(1:length(final),function(i){
          tablename <- paste("table",i)
          tableOutput(tablename)
        })
        do.call(tagList,table_out_list)
      })
      for(i in 1:length(final)){
        local({
          deezI <- i
          tablename <- paste("table",deezI)
          textname <- paste("text",deezI)
          output[[tablename]] <- renderTable(expr = final[[deezI]],rownames = T,{
           
          }, caption = toString(sol[[deezI]]),
          caption.placement = getOption("xtable.caption.placement", "bottom"), 
          caption.width = getOption("xtable.caption.width", NULL))
          output[[textname]] <- renderText(expr = toString(sol[[deezI]])) 
        })

    }
  }

    
  })

}
shinyApp(ui = ui, server = server)