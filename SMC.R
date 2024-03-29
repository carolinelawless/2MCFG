C_rules<- 0 #factor to add to each of the observed rules
C_nonterminals<- 0 #factor to add to each of the observed nonterminals

list_nonterminals_vec_long<- list()
list_nonterminals_vec_short<- list()
list_p_rules<- list()
list_p_rules_short<- list()
list_e_rules<- list()
list_type_matrix<- list()
list_epsilon_matrix<- list()
list_terminals_matrix<- list()
list_permutations_vec<- list()
list_tree_matrix<- list()
list_left_functions<- list()
list_right_functions<- list()
list_rows<- list()
list_sides<- list()
list_numbers<- list()

list_max_nonterminals<- list()
vec_max_nonterminals<- vector(length = M)

for(i in 1:M){
  list_nonterminals_vec_long[[i]]<- vector()
  list_nonterminals_vec_short[[i]]<- vector()
  list_p_rules[[i]]<- list()
  list_p_rules_short[[i]]<- list()
  list_e_rules[[i]]<- list()
  list_type_matrix[[i]]<- matrix(c(1,b1,b2),nrow=1,ncol=3)
  list_epsilon_matrix[[i]]<- matrix(c(1,c1,c2),nrow=1,ncol=3)
  list_terminals_matrix[[i]]<- matrix(1,nrow=1,ncol=length(terminals)+1)
  list_permutations_vec[[i]]<- rep(permutations_param,factorial(5))
}
list_nonterminals_vec_long1<- list_nonterminals_vec_long
list_nonterminals_vec_short1<- list_nonterminals_vec_short
list_p_rules1<- list_p_rules
list_p_rules_short1<- list_p_rules_short
list_e_rules1<- list_e_rules
list_type_matrix1<- list_type_matrix
list_epsilon_matrix1<- list_epsilon_matrix
list_terminals_matrix1<- list_terminals_matrix
list_permutations_vec1<- list_permutations_vec

weights<- rep(1,M)

for(ss in 1:length(sentences)){
  print(paste0("ss=",ss))
  
  number_nonterminals_vec<- vector(length = M)  
  number_p_rules_vec<- vector(length = M)
  number_e_rules_vec<- vector(length = M)
  number_nonterminals_vec_3<- vector(length =M)
  number_nonterminals_vec_10<- vector(length =M)
  number_p_rules_vec_3<- vector(length = M)
  number_e_rules_vec_3<- vector(length = M)
  number_p_rules_vec_10<- vector(length = M)
  number_e_rules_vec_10<- vector(length = M)
  number_permutations_vec<- vector(length = M)
  number_permutations_vec_3<- vector(length = M)
  number_permutations_vec_10<- vector(length = M)
  
  sentence<- sentences[[ss]]
  
  for(i in 1:M){
    list_tree_matrix[[i]]<- matrix(ncol=8)
    colnames(list_tree_matrix[[i]])<- c("ind_1","ind_2","w_e1","type","min","max","B","nrows")
    list_tree_matrix[[i]][1,2]<- 1
    list_tree_matrix[[i]][1, 5]<- length(sentence)
    list_tree_matrix[[i]][1, 6]<- length(sentence)
    list_tree_matrix[[i]][1, 7]<- 1
    list_tree_matrix[[i]][1,8]<- 1
    list_left_functions[[i]]<- list()
    list_right_functions[[i]]<- list()
    list_rows[[i]]<- 1
    list_sides[[i]]<- "left"
    list_numbers[[i]]<- 1
  } 
  list_tree_matrix1<- list_tree_matrix
  list_left_functions1<- list_left_functions
  list_right_functions1<- list_right_functions
  list_rows1<- list_rows
  list_sides1<- list_sides
  list_numbers1<- list_numbers
  
  for(i in 1:M){
    
    nonterminals_vec_long<- list_nonterminals_vec_long[[i]]
    nonterminals_vec_short<- list_nonterminals_vec_short[[i]]
    p_rules<- list_p_rules[[i]]
    p_rules_short<- list_p_rules_short[[i]]
    e_rules<- list_e_rules[[i]]
    type_matrix<- list_type_matrix[[i]]
    epsilon_matrix<- list_epsilon_matrix[[i]]
    terminals_matrix<- list_terminals_matrix[[i]]
    permutations_vec<- list_permutations_vec[[i]]
    tree_matrix<- list_tree_matrix[[i]]
    left_functions<- list_left_functions[[i]]
    right_functions<- list_right_functions[[i]]
    rows<- list_rows[[i]]
    sides<- list_sides[[i]]
    numbers<- list_numbers[[i]]
    row<- tail(rows,1)
    side<- tail(sides,1)
    number<- tail(numbers,1)
    w<- 1
    
    
    for(ttt in 1:length(sentence)){
      #print(ttt)
      
      tt<- ttt
      
      while(tt == ttt){
        if(is.na(tree_matrix[row,4])){#then draw a rule
          tree_matrix[row,8]<- nrow(tree_matrix)
          minimum<- tree_matrix[row, 5] #minimal number of terminal symbols associated with current nonterminal
          maximum<- tree_matrix[row, 6] #maximal number of terminal symbols associated with current nonterminal
          nonterminal<- tree_matrix[row, 7] #current nonterminal symbol
          
          if(length(which(type_matrix[,1] == nonterminal))==0){
            type_matrix<- rbind(type_matrix,c(nonterminal,b1,b2))
            epsilon_matrix<- rbind(epsilon_matrix,c(nonterminal,c1,c2))
          }
          
          new_rule<- dp_random(nonterminal,minimum,maximum)
          
          rule<- new_rule[[1]]
          type<- new_rule[[2]]
          weight<- new_rule[[3]]
          e1_weight<- new_rule[[4]]
          
          tree_matrix[row,3]<- e1_weight
          tree_matrix[row,4]<- type
          
          w<- w*weight
          
          index<- which(type_matrix[,1]==nonterminal)
          if(type == 0|type==1){#production rule
            type_matrix[index,3]<- type_matrix[index,3]+1
          }else if(type==2|type==3){#emission rule
            type_matrix[index,2]<- type_matrix[index,2]+1
            if(type==2 & rule!=""){#no epsilon
              epsilon_matrix[index,3]<- epsilon_matrix[index,3]+1
            }else{#epsilon
              epsilon_matrix[index,2]<- epsilon_matrix[index,2]+1
            }
          }
          
          
          if (type == 0 | type ==1){#production rule
            
            p_rule_short<- c(rule[[1]],rule[[2]],rule[[6]])
            permutation<- p_rule_short[4]
            permutations_vec[permutation]<- permutations_vec[permutation]+1
            
            left_functions[[row]] <- as.list(rule[[3]])
            right_functions[[row]] <- as.list(rule[[4]])
            
            p_rules[[length(p_rules)+1]]<- rule
            p_rules_short[[length(p_rules_short)+1]]<- p_rule_short
            nonterminals_vec_short<- c(nonterminals_vec_short,nonterminal)
            
            
            nonterminals_vec_long<- c(nonterminals_vec_long,rule[[2]])
            
            
            nc <- 2 #number of children
            nr <- nrow(tree_matrix) #number of rows
            if (nc == 1) {
              tree_matrix <-rbind(tree_matrix, c(row, nr + 1, rep(NA, 2), minimum, maximum, rule[[2]][1],nr+nc))
              left_functions[[nr + 1]] <-list()
              right_functions[[nr + 1]] <-list()
            }else{
              for(cc in 1:nc){
                tree_matrix <-rbind(tree_matrix, c(row, nr + cc, rep(NA, 2), 1, maximum - nc + 1, rule[[2]][cc],nr+nc))
                left_functions[[nr + cc]] <- list()
                right_functions[[nr + cc]] <- list()
              }
              minimum<- max(tree_matrix[row,5],nc)
              tree_matrix[row,5]<- minimum
              tree_matrix<- update("min")
            }
          }else if(type == 2 | type== 3){#emission rule
            
            
            x<- rule
            
            if(type == 2 & x != ""){
              maximum<- 2
              tree_matrix[row,6]<- maximum
              tree_matrix<- update("max")
              minimum<- 2
              tree_matrix[row,5]<- minimum
              tree_matrix<- update("min")
            }else{
              maximum<- 1
              tree_matrix[row,6]<- 1
              tree_matrix<- update("max")
            }
            
            if (side == "left") {
              left_functions[[row]] <- x
              right_functions[[row]]<- list()
            }else if(side=="right"){
              left_functions[[row]]<- list()
              right_functions[[row]]<- x
            }
            
            if(x==sentence[tt]){
              tt<- tt+1
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
              }
              side<- side1
              sides<- sides1
            }
          }
        }else if(tree_matrix[row,4]==0| tree_matrix[row,4]==1){
          
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
        }else if(tree_matrix[row,4]==2| tree_matrix[row,4]==3){
          
          nonterminal<- tree_matrix[row,7]
          minimum<- tree_matrix[row,5]
          maximum<- tree_matrix[row,6]
          type<- tree_matrix[row,4]
          
          if(length(which(terminals_matrix[,1] == nonterminal))==0){
            terminals_matrix<- rbind(terminals_matrix,c(nonterminal,rep(1,length(terminals))))
          }
          
          if(type == 2){
            x<- sentence[tt]
          }else{
            x<- ""
          }
          
          if (side == "left") {
            left_functions[[row]] <- x
            y<- right_functions[[row]]
          }else if(side== "right"){
            right_functions[[row]]<- x
            y<- left_functions[[row]]
          }
          
          complete_rule<- c(nonterminal, left_functions[[row]],right_functions[[row]])
          e_rules[[length(e_rules)+1]]<- complete_rule
          
          w<- w*weight_e2(nonterminal)
          
          ind_nt<- which(terminals_matrix[,1]==nonterminal)
          if(x!=""){
            ind_x<- which(terminals ==x)
            terminals_matrix[ind_nt,(ind_x+1)]<-  terminals_matrix[ind_nt,(ind_x+1)]+1
          }
          if(y!=""){
            ind_y<- which(terminals ==y)
            terminals_matrix[ind_nt,(ind_y+1)]<-  terminals_matrix[ind_nt,(ind_y+1)]+1
          }
          
          if(x==sentence[tt]){
            tt<- tt+1
          }
          
          if(length(rows)!=1){
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
          }
        }
        tree_matrix
        ttt
        row
      }# while tt == ttt
      
    }#ttt in 1:length(sentence)
    
    
    
    count<- 0
    if(length(left_functions[[1]])>0){
      for(j in 1:length(left_functions[[1]])){
        count<- count+ is.numeric(left_functions[[1]][[j]])}
    }
    if(length(right_functions[[1]])>0){
      for(j in 1:length(right_functions[[1]])){
        count<- count+ is.numeric(right_functions[[1]][[j]])}
    }
    
    
    while(count > 0){
      
      if(tree_matrix[row,4]==0| tree_matrix[row,4]==1){
        
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
      }else if(tree_matrix[row,4]==2| tree_matrix[row,4]==3){
        
        nonterminal<- tree_matrix[row,7]
        minimum<- tree_matrix[row,5]
        maximum<- tree_matrix[row,6]
        type<- tree_matrix[row,4]
        
        if(length(which(terminals_matrix[,1] == nonterminal))==0){
          terminals_matrix<- rbind(terminals_matrix,c(nonterminal,rep(1,length(terminals))))
        }
        
        if(type == 2){
          x<- sentence[tt]
        }else{
          x<- ""
        }
        
        if (side == "left") {
          left_functions[[row]] <- x
          y<- right_functions[[row]]
        }else if(side== "right"){
          right_functions[[row]]<- x
          y<- left_functions[[row]]
        }
        
        complete_rule<- c(nonterminal, left_functions[[row]],right_functions[[row]])
        e_rules[[length(e_rules)+1]]<- complete_rule
        
        w<- w*weight_e2(nonterminal)
        
        ind_nt<- which(terminals_matrix[,1]==nonterminal)
        if(x!=""){
          ind_x<- which(terminals ==x)
          terminals_matrix[ind_nt,(ind_x+1)]<-  terminals_matrix[ind_nt,(ind_x+1)]+1
        }
        if(y!=""){
          ind_y<- which(terminals ==y)
          terminals_matrix[ind_nt,(ind_y+1)]<-  terminals_matrix[ind_nt,(ind_y+1)]+1
        }
        
        if(length(rows)!=1){
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
        }
      }
      
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
    }
    
    list_nonterminals_vec_long[[i]]<- nonterminals_vec_long
    list_nonterminals_vec_short[[i]]<- nonterminals_vec_short
    list_p_rules[[i]]<- p_rules
    list_p_rules_short[[i]]<- p_rules_short
    list_e_rules[[i]]<- e_rules
    list_type_matrix[[i]]<- type_matrix
    list_epsilon_matrix[[i]]<- epsilon_matrix
    list_terminals_matrix[[i]]<- terminals_matrix
    list_permutations_vec[[i]]<- permutations_vec
    list_tree_matrix[[i]]<- tree_matrix
    list_left_functions[[i]]<- left_functions
    list_right_functions[[i]]<- right_functions
    list_rows[[i]]<- rows
    list_sides[[i]]<- sides
    list_numbers[[i]]<- numbers
    weights[i]<- w
    
    
    number_nonterminals_vec[i]<- max(nonterminals_vec_long)
    number_nonterminals_vec_3[i]<- length(which(table(nonterminals_vec_long)>3))
    number_nonterminals_vec_10[i]<- length(which(table(nonterminals_vec_long)>10))
    
    number_p_rules_vec[i]<- length(unique_frequencies(p_rules_short))
    number_p_rules_vec_3[i]<- length(which(unique_frequencies(p_rules_short)>3))
    number_p_rules_vec_10[i]<- length(which(unique_frequencies(p_rules_short)>10))
    
    number_e_rules_vec[i]<- length(unique_frequencies((e_rules))) 
    number_e_rules_vec_3[i]<- length(which(unique_frequencies(e_rules)>3)) 
    number_e_rules_vec_10[i]<- length(which(unique_frequencies(e_rules)>10)) 
    
    number_permutations_vec[i]<- length(which(permutations_vec > permutations_param))
    number_permutations_vec_3[i]<- length(which(permutations_vec >= (permutations_param + 3)))
    number_permutations_vec_10[i]<- length(which(permutations_vec >= (permutations_param + 10)))
  }
  list_number_p_rules[[length(list_number_p_rules)+1]]<- number_p_rules_vec
  list_number_e_rules[[length(list_number_e_rules)+1]]<- number_e_rules_vec
  list_number_nonterminals[[length(list_number_nonterminals)+1]]<- number_nonterminals_vec
  list_number_p_rules_3[[length(list_number_p_rules_3)+1]]<- number_p_rules_vec_3
  list_number_e_rules_3[[length(list_number_e_rules_3)+1]]<- number_e_rules_vec_3
  list_number_p_rules_10[[length(list_number_p_rules_10)+1]]<- number_p_rules_vec_10
  list_number_e_rules_10[[length(list_number_e_rules_10)+1]]<- number_e_rules_vec_10
  list_number_nonterminals_3[[length(list_number_nonterminals_3)+1]]<- number_nonterminals_vec_3
  list_number_nonterminals_10[[length(list_number_nonterminals_10)+1]]<- number_nonterminals_vec_10
  list_number_permutations[[length(list_number_permutations) + 1]]<- number_permutations_vec
  list_number_permutations_3[[length(list_number_permutations_3) + 1]]<- number_permutations_vec_3
  list_number_permutations_10[[length(list_number_permutations_10) + 1]]<- number_permutations_vec_10
  
  weights<- weights/sum(weights)
  ESS[ss]<- 1/sum(weights^2)
  particles<- sample(1:M,M,weights,replace=TRUE)
  for(i in 1:M){
    j<- particles[i]
    list_nonterminals_vec_long1[[i]]<- list_nonterminals_vec_long[[j]]
    list_nonterminals_vec_short1[[i]]<- list_nonterminals_vec_short[[j]]
    list_p_rules1[[i]]<- list_p_rules[[j]]
    list_p_rules_short1[[i]]<- list_p_rules_short[[j]]
    list_e_rules1[[i]]<- list_e_rules[[j]]
    list_type_matrix1[[i]]<- list_type_matrix[[j]]
    list_epsilon_matrix1[[i]]<- list_epsilon_matrix[[j]]
    list_terminals_matrix1[[i]]<- list_terminals_matrix[[j]]
    list_permutations_vec1[[i]]<- list_permutations_vec[[j]]
    list_tree_matrix1[[i]]<- list_tree_matrix[[j]]
    list_left_functions1[[i]]<- list_left_functions[[j]]
    list_right_functions1[[i]]<- list_right_functions[[j]]
    list_rows1[[i]]<- list_rows[[j]]
    list_sides1[[i]]<- list_sides[[j]]
    list_numbers1[[i]]<- list_numbers[[j]]
  }
  for(i in 1:M){
    list_nonterminals_vec_long[[i]]<- list_nonterminals_vec_long1[[i]]
    list_nonterminals_vec_short[[i]]<- list_nonterminals_vec_short1[[i]]
    list_p_rules[[i]]<- list_p_rules1[[i]]
    list_p_rules_short[[i]]<- list_p_rules_short1[[i]]
    list_e_rules[[i]]<- list_e_rules1[[i]]
    list_type_matrix[[i]]<- list_type_matrix1[[i]]
    list_epsilon_matrix[[i]]<- list_epsilon_matrix1[[i]]
    list_terminals_matrix[[i]]<- list_terminals_matrix1[[i]]
    list_permutations_vec[[i]]<- list_permutations_vec1[[i]]
    list_tree_matrix[[i]]<- list_tree_matrix1[[i]]
    list_left_functions[[i]]<- list_left_functions1[[i]]
    list_right_functions[[i]]<- list_right_functions1[[i]]
    list_rows[[i]]<- list_rows1[[i]]
    list_sides[[i]]<- list_sides1[[i]]
    list_numbers[[i]]<- list_numbers1[[i]]
  }
  weights<- rep(1,M)
  
}##ss 1:length(sentences)
