

evaluate<- function(string1,x,numb){
  w<- which(string1==numb)
  s<- append(string1,x,after=w)
  s<- s[-w]
  return(s)
}



terminals<- c('a','b')
proba_emission_model <- 0.6 #probability of emission
proba_epsilon_model<- 0.9 #probability of emission being empty

number_of_sentences<- 50
sentences<- list()

for(j in 1:number_of_sentences){
  tree_matrix<- matrix(ncol=8)
  colnames(tree_matrix)<- c("ind_1","ind_2","n_children","type","min","max","B","nrows")
  tree_matrix[1, 2]<- 1
  tree_matrix[1, 7]<- 1
  tree_matrix[1,8]<- 1
  left_functions<- list()
  right_functions<- list()
  rows<- 1
  sides<- "left"
  numbers<- 1
  row<- tail(rows,1)
  side<- tail(sides,1)
  number<- tail(numbers,1)
  
  count<- 1
  
  while(count>0){
    
    if(is.na(tree_matrix[row,4])){#then draw a new rule
      
      tree_matrix[row,8]<- nrow(tree_matrix)
      nonterminal<- tree_matrix[row, 7] #current nonterminal symbol
      rand<- sample(c(0, 1), 1, F, prob = c(proba_emission_model, 1 - proba_emission_model)) #emission or production
      
      if (rand==1){#production rule
        nonterminals<- c(1,1)
        #string1<- c(1,3)
        #string2<- c(4,2)
        string1<- c(1,2)
        string2<- c(3,4)
        N<- 2
        rule<- list(nonterminal, nonterminals, string1, string2, N)
        
        
        left_functions[[row]] <- as.list(rule[[3]])
        right_functions[[row]] <- as.list(rule[[4]])
        tree_matrix[row, 3] <- rule[[5]]
        tree_matrix[row, 4] <- 1
        
        
        nc <- tree_matrix[row, 3] #number of children
        nr <- nrow(tree_matrix) #number of rows
        if (nc == 1) {
          tree_matrix <-rbind(tree_matrix, c(row, nr + 1, rep(NA, 2), 0, 0, rule[[2]][1],nr+nc))
          left_functions[[nr + 1]] <-list()
          right_functions[[nr + 1]] <-list()
        }else{
          for(cc in 1:nc){
            tree_matrix <-rbind(tree_matrix, c(row, nr + cc, rep(NA, 4), rule[[2]][cc],nr+nc))
            left_functions[[nr + cc]] <- list()
            right_functions[[nr + cc]] <- list()
          }
        }
      }else{#emission rule
        tree_matrix[row,3]<- 0
        tree_matrix[row,4]<- 2
        
        symbol<- sample(terminals,1)
        x<- symbol
        y<- symbol
        
        if (side == "left") {
          left_functions[[row]] <- x
          right_functions[[row]]<- y
        }else if(side=="right"){
          left_functions[[row]]<- y
          right_functions[[row]]<- x
        }
        
        if(length(rows)>1){
          row <- rows[(length(rows) - 1)]
          side <- sides[(length(sides) - 1)]
          numb<- number
          number<- numbers[(length(numbers) -1)]
          rows <- rows[1:(length(rows) - 1)]
          sides <- sides[1:(length(sides) - 1)]
          numbers<- numbers[1:(length(numbers) -1)]
          
          if (side == "left") {
            string1 <- left_functions[[row]]
            left_functions[[row]] <- evaluate(string1,x,numb)
          } else if (side == "right") {
            string1 <- right_functions[[row]]
            right_functions[[row]] <- evaluate(string1, x, numb)
          }
        }else{
          if(side=="left"){
            side1<- "right"
            sides1<- "right"
          }else if(side=="right"){
            side1<- "left"
            sides1<- "left"
          }
          side<- side1
          sides<- sides1
        }
      }
    }else if(tree_matrix[row,4]==1){
      
      if(side=="left"){
        string<- left_functions[[row]]
      }else if(side=="right"){
        string<- right_functions[[row]]
      }
      vec<- vector(length=length(string))
      for(k in 1:length(vec)){vec[k]<- is.numeric(string[[k]])}
      
      if(row==1 & side == "left" & sum(vec) == 0 ){
        sides<- "right"
        side<- "right"
      }else{
        
        if(sum(vec)==0){
          if(length(string)==1){
            if(is.na(string)==TRUE){
              x<- ""
            }else{x<- string}
          }else{
            x<- paste(string,collapse="")
          }
          if(side=="left"){
            left_functions[[row]]<- x
          }else if(side=="right"){
            right_functions[[row]]<- x
          }
          row <- rows[(length(rows) - 1)]
          side <- sides[(length(sides) - 1)]
          numb<- number
          number<- numbers[(length(numbers)-1)]
          rows <- rows[1:(length(rows) - 1)]
          sides <- sides[1:(length(sides) - 1)]
          numbers<- numbers[1:(length(numbers)-1)]
          if (side == "left") {
            string1 <- left_functions[[row]]
            left_functions[[row]] <- evaluate(string1, x, numb)
          } else if (side == "right") {
            string1 <- right_functions[[row]]
            right_functions[[row]] <- evaluate(string1, x, numb)
          }
          
        }else{
          
          ii<- 1
          while(is.numeric(string[[ii]])==FALSE){ii<- ii+1}
          
          number <- string[[ii]]
          if (ceiling(number / 2) == number / 2) {
            side <- "right"
          } else if (ceiling(number / 2) != number / 2) {
            side <- "left"
          }
          row <- tree_matrix[row,8] + ceiling(number / 2)
          sides <- c(sides, side)
          rows <- c(rows, row)
          numbers<- c(numbers, number)
          
          
          
        }
      }
    }else if(tree_matrix[row,4]==2){
      
      if(side=="left"){
        x<- left_functions[[row]]
      }else if(side=="right"){
        x<- right_functions[[row]]
      }
      
      if(length(rows)>1){
        row <- rows[(length(rows) - 1)]
        side <- sides[(length(sides) - 1)]
        numb<- number
        number<- numbers[(length(numbers) -1)]
        rows <- rows[1:(length(rows) - 1)]
        sides <- sides[1:(length(sides) - 1)]
        numbers<- numbers[1:(length(numbers) -1)]
        
        if (side == "left") {
          string1 <- left_functions[[row]]
          left_functions[[row]] <- evaluate(string1,x,numb)
        } else if (side == "right") {
          string1 <- right_functions[[row]]
          right_functions[[row]] <- evaluate(string1, x, numb)
        }
      }else{
        if(side=="left"){
          side1<- "right"
          sides1<- "right"
        }else if(side=="right"){
          side1<- "left"
          sides1<- "left"
        }
        side<- side1
        sides<- sides1
      }
      
    }
    tree_matrix
    row
    left_functions[[1]]
    
    
    count1<- 0
    if(length(left_functions[[1]])>0){
      for(j in 1:length(left_functions[[1]])){
        count1<- count1+ is.numeric(left_functions[[1]][[j]])}
    }
    if(length(right_functions[[1]])>0){
      for(j in 1:length(right_functions[[1]])){
        count1<- count1+ is.numeric(right_functions[[1]][[j]])}
    }
    count<- count1
    tree_matrix
  }
  tree_matrix
  row
  left_functions
  
  sentence<- vector()
  for(i in 1:length(left_functions[[1]])){
    str1<- unlist(strsplit(left_functions[[1]][[i]],""))
    sentence<- c(sentence,str1)
  }
  for(i in 1:length(right_functions[[1]])){
    str1<- unlist(strsplit(left_functions[[1]][[i]],""))
    sentence<- c(sentence,str1)
  }
  sentences[[length(sentences)+1]]<- sentence
}

sentences1<- list()
for(i in 1:length(sentences)){
  if(length(sentences[[i]])>4){
    sentences1[[length(sentences1)+1]]<- sentences[[i]]
  }
}
sentences<- sentences1
sentences
