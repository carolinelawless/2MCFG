library(LaplacesDemon)
##Draw from the DP of nonterminals ##returns a vector of length n of nonterminals
draw_nt<- function(n){
  nt<- vector(length=n)
  ntv<-nonterminals_vec_long
  for(i in 1:n){
    if(length(ntv)==0){
      nt[i]<- 1
    }else{
      draw<- sample(c("new","old"),1,prob=c(alpha1,length(ntv)))
      if(draw=="old"){
        s<- sample(1:length(ntv),1)
        nt[i]<- ntv[s]
      }else if(draw=="new"){
        nt[i]<- max(ntv)+1
      }
    }
    ntv<- c(ntv,nt[i])
  }
  return(nt)
}

##Random base production rule
base_production_random<- function(nonterminal,gamma1){
  if(grammar=="g0"){
    N<- rpois(1,gamma1)+2
    sigma<- sample(1:(2*N),2*N,replace=FALSE)
    cut<- sample(1:(2*N+1),1)
    if(cut==1){
      string1<- NA
      string2<- sigma
    }else if(cut==(2*N+1)){
      string1<- sigma
      string2<- NA
    }else{
      string1<- sigma[1:(cut-1)]
      string2<- sigma[cut:(2*N)]
    }
    nonterminals<- draw_nt(N)
  }else if(grammar== "g1"){
    ss<- sample(terminals,1)
    string1<- c(ss,1)
    string2<- c(ss,2)
    nonterminals<- 2
  }else if(grammar=="g2"){
    string1<- c("a",1,"b")
    string2<- c("c",2,"d")
    nonterminals<- 2
  }else if(grammar== "g3"){
    N<- 1
    string1<- c("a",1)
    string2<- c(2,"b")
    nonterminals<- draw_nt(N)
  }else if(grammar== "alpha^2"){
    N<- 1
    ts<- sample(terminals,1)
    string1<- c(ts,as.numeric(1))
    string2<- c(ts,as.numeric(2))
    nonterminals<- draw_nt(N)
  }else if(grammar== "cf"){
    N<- 2
    string1<- 1:2
    string2<- 3:4
    nonterminals<- draw_nt(N)
  }else if(grammar== "regular"){
    N<- 1
    string1<- sample(terminals,1)
    string2<- 1:2
    nonterminals<- draw_nt(N)
  }
  
  r<- list(nonterminal, nonterminals, string1, string2, N)
  return(r)
}

kernel_parameters_function<- function(nonterminal,minimum,maximum){
  index<- which(gamma_matrix[,1]==nonterminal)
  gamma1<- rgamma(1,gamma_matrix[index,2],gamma_matrix[index,3])
  proba_emission<- rbeta(1,type_matrix[index,2],type_matrix[index,3])
  proba_epsilon<- rbeta(1,epsilon_matrix[index,2],epsilon_matrix[index,3])
  p_rules_star<- list()
  q_star<- 0
  qq<- 0
  if(maximum>1){#this means production rules possible
    index = which(nonterminals_vec_short == nonterminal)
    qq<- length(index)
    if(length(index)>0){
      for(i in 1:length(index)){
        j<- index[i]
        if(p_rules[[j]][5] <= maximum){
          p_rules_star[[length(p_rules_star)+1]]<- p_rules[[j]]
        }
      }
    }
    q_star<- length(p_rules_star)
    s<- sum(dpois(2:maximum - 2,gamma1))
  }
  if(minimum == 1 & maximum > 1){
    proba_emission_star<- proba_emission/(s*(1-proba_emission) + proba_emission)
    proba_production_star<- 1 - proba_emission_star
    alpha_star<- alpha2*(s*(1-proba_emission) + proba_emission)
  }
  if(minimum == 2){
    proba_emission_star<- (1-proba_epsilon)*proba_emission/(s*(1-proba_emission)+(1-proba_epsilon)*proba_emission)
    proba_production_star<- 1 - proba_emission_star
    alpha_star<- alpha2*(s*(1-proba_emission) + (1-proba_epsilon)*proba_emission)
  }
  if(minimum > 2){
    proba_emission_star<- 0
    proba_production_star<- 1
    alpha_star<- alpha2*s*(1-proba_emission)
  }
  if(maximum == 1){
    proba_emission_star<- 1
    proba_production_star<- 0
    alpha_star<- alpha2*proba_emission*proba_epsilon
  }
  #parameter_list<- list(proba_emission_star,proba_production_star,alpha_star,p_rules_star,q_star,qq,gamma1,proba_emission,proba_epsilon)
  parameter_list<- list(proba_emission_star,proba_production_star,alpha_star,p_rules_star,q_star,qq,gamma1,proba_emission,proba_epsilon)
    return(parameter_list)
}

dp_random<- function(nonterminal,minimum,maximum){
  kernel_params<- kernel_parameters_function(nonterminal,minimum,maximum)
  proba_emission_star<- kernel_params[[1]]
  proba_production_star<- kernel_params[[2]]
  alpha_star<- kernel_params[[3]]
  p_rules_star<- kernel_params[[4]]
  q_star<- kernel_params[[5]]
  qq<- as.numeric(kernel_params[[6]])
  gamma1<- as.numeric(kernel_params[[7]])
  proba_emission<- as.numeric(kernel_params[[8]])
  proba_epsilon<- as.numeric(kernel_params[[9]])
  draw1<- sample(0:1,1,prob=c(alpha_star,q_star))
  if(draw1==0){#new rule
    draw2<- sample(0:1,1,prob=c(proba_production_star,proba_emission_star))
    if(draw2==0){#new production rule
      nn <- maximum + 1
      while (nn > maximum) {
        rr <- base_production_random(nonterminal,gamma1)
        nn <- rr[[5]]
        rr[[6]]<- "new"
      }
      rule<- rr
      type<- 0
    }else if(draw2==1){#new (partial) emission rule
      if(minimum==1 & maximum >1){
        draw3<- sample(0:2,1,prob=c(1-proba_epsilon,proba_epsilon/2,proba_epsilon/2))
        if(draw3==0){
          x<- sentence[tt] #### the index here is tt?
          y<- "symbol"
          type<- 2
        }else if(draw3==1){
          x<- sentence[tt]
          y<- ""
          type<- 3
        }else if(draw3==2){
          x<- ""
          y<- "symbol"
          type<- 2
        }
      }else if(minimum==2){
        x<- sentence[tt]
        y<- "symbol"
        type<- 2
      }else if(maximum==1){
        draw3<- sample(0:1,1)
        if(draw3 == 0){
          x<- sentence[tt]
          y<- ""
          type<- 3
        }else if(draw3 == 1){
          x<- ""
          y<- "symbol"
          type<- 2
        }
      }
      rule<- x
    }
  }else if(draw1==1){#old (production) rule
    samp<- sample(1:q_star,1)
    rule<- p_rules_star[[samp]]
    rule[[6]]<- "old"
    type<- 1
  }
  weight<- (alpha_star + q_star)/(alpha2 + qq)
  #params<- c(gamma1,proba_emission,proba_epsilon)
  output<- list(rule,type,weight)
  return(output)
}

evaluate<- function(string1,x,numb){
  w<- which(string1==numb)
  s<- append(string1,x,after=w)
  s<- s[-w]
  return(s)
}

update<- function(min_or_max){
  tm<- tree_matrix
  l<- length(rows)
  if(min_or_max == "min"){
    for(i in 1:(l-1)){
      par<- rows[l-i]
      ch<- which(tm[,1] == par)
      tm[par,5]<- max(tm[par,5], sum(tm[ch,5]))
    }
    for(node in 2:nrow(tm)){
      par<- tm[node,1]
      ch<- which(tm[,1]==par)
      update<- tm[par,6] - sum(tm[ch,5]) + tm[node,5]
      tm[node,6]<- min(tm[node,6], update)
    }
  }else if(min_or_max== "max"){
    for(i in 1:(l-1)){
      par<- rows[l-i]
      ch<- which(tm[,1] == par)
      tm[par,6]<- min(tm[par,6], sum(tm[ch,6]))
    }
    for(node in 2:nrow(tm)){
      par<- tm[node,1]
      ch<- which(tm[,1]==par)
      update<- tm[par,5] - sum(tm[ch,6]) + tm[node,6]
      tm[node,5]<- max(tm[node,5], update)
    }
  }
  return(tm)
}

weight_e2<- function(nonterminal){
  w<- 1
  
  nt_ind<- which(terminals_matrix[,1]==nonterminal)
  if(x!=""){
    x_ind<- which(terminals == x)
    d1<- terminals_matrix[nt_ind,(x_ind+1)]/(sum(terminals_matrix[nt_ind,2:(length(terminals)+1)]))
    w<- w*d1
  }
  if(y!=""){
    y_ind<- which(terminals == y)
    d2<- terminals_matrix[nt_ind,(y_ind+1)]/(sum(terminals_matrix[nt_ind,2:(length(terminals)+1)]))
    w<- w*d2
  }
  
  freq<- 0
  if(side=="left"){
    for(i in 1:length(e_rules)){
      if(e_rules[[i]][[1]]==nonterminal & e_rules[[i]][[2]]==x & e_rules[[i]][[3]]==y){
        freq<- freq+1
      }
    }
  }else if(side=="right"){
    for(i in 1:length(e_rules)){
      if(e_rules[[i]][[1]]==nonterminal & e_rules[[i]][[2]]==y & e_rules[[i]][[3]]==x){
        freq<- freq+1
      }
    }
  }
  ind_emission<- which(type_matrix[,1]==nonterminal)
  ind_epsilon<- which(epsilon_matrix[,1]== nonterminal)
  proba_epsilon_expected<- epsilon_matrix[ind_epsilon,2]/(epsilon_matrix[ind_epsilon,2] + epsilon_matrix[ind_epsilon,3])
  proba_emission_expected<- type_matrix[ind_emission,2]/(type_matrix[ind_emission,2]+ type_matrix[ind_emission,3])
  if(x!="" & y!= ""){
    w<- w + freq/(proba_emission_expected*(1-proba_epsilon_expected))
  }else{
    w<- w + freq/(proba_emission_expected*proba_epsilon_expected/2)
  }

  return(w)
}










#emission_random<- function(nonterminal){
#  
 ## if(grammar=="g0"){
   # draw<- sample(c(0,1),1,replace = FALSE,prob = c(proba_epsilon,1-proba_epsilon))
    #if(draw ==1){
     # t_symbols<- rcat(2, emission_probas/sum(emission_probas))
      #x<- terminals[t_symbols[1]]
      #y<- terminals[t_symbols[2]]
    #}else{
     # x<- terminals[rcat(1,emission_probas/sum(emission_probas))]
      #y<- ""
    #}
  #}else if(grammar== "g1"){
   # x<- terminals[rcat(1,emission_probas/sum(emission_probas))]
    #y<- x
  #}else if(grammar=="g2"){
   # string1<- ""
    #string2<- ""
  #}else if(grammar=="g3"){
   # string1<- "a"
  #  string2<- "b"
  #}else if(grammar== "alpha^2"){
   # x<- terminals[rcat(1,emission_probas/sum(emission_probas))]
  #  y<- x
  #}else if(grammar=="cf"){
   # x<- terminals[rcat(1,emission_probas/sum(emission_probas))]
  #  y<- ""
  #}else if(grammar=="regular"){
   # x<- terminals[rcat(1,emission_probas/sum(emission_probas))]
  #  y<- ""
  #}
  #return(c(x,y))
#}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
