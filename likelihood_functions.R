
base_likelihood_production<- function(rule,alpha1,gamma1,nonterminals_vec_all,proba_emission){
nonterminal<- rule[[1]]
nonterminals<- nonterminals_vec_all
N<- length(rule[[2]])
likelih<- (1-proba_emission)*dpois((N-1),1/gamma1)/((2*N+1)*factorial(2*N))
for(i in 1:N){
nt<- rule[[2]][i]
if(length(which(nonterminals==nt))>0){
likelih<- likelih*length(which(nonterminals== nt))/(length(nonterminals)+ alpha1)
}else if(length(which(nonterminals==nt)==0)){
likelih<- likelih*alpha1/(length(nonterminals)+alpha1)
}
nonterminals<- c(nonterminals,nt)
}
return(likelih)
}

base_likelihood_emission<- function(rule,side,alphabet,proba_emisson,proba_epsilon){
  if(side=="left"){
    x<- rule[[3]]
  }else if(side=="right"){
    x<- rule[[4]]
  }
  if(x==""){
    likelih<- proba_emission*proba_epsilon
  }else{
    likelih<- proba_emission*(1-proba_epsilon)/length(alphabet)
  }
  return(likelih)
}

likelihood_emission1<- function(rule,rule_left,rule_right, side, alphabet,proba_emission, proba_epsilon, nonterminals_vec_all, rules, rules_left, rules_right){
base_likelih<- base_likelihood_emission(rule,side,alphabet,proba_emission,proba_epsilon)
likelih<- alpha2*base_likelih
if(side=="left"){
for(i in 1:length(rules_left)){
likelih<- likelih + setequal(rule_left, rules_left[[i]])
}
}else if(side == "right"){
for(i in 1:length(rules_right)){
likelih<- likelih + setequal(rule_right,rules_right[[i]])
}
}
likelih<- likelih/(alpha2 + length(rules))
}



likelihood_emission2<- function(rule,rule_left,rule_right, side, alphabet,proba_emission, proba_epsilon, nonterminals_vec_all, rules,rules_left,rules_right){
  base_likelih<- base_likelihood_emission(rule,side,alphabet,proba_emission,proba_epsilon)

  likelih<- alpha2*base_likelih 
  for(i in 1:length(rules)){
  likelih<- likelih + setequal(rule, rules[[i]])
  }
  denom<- alpha2
  if(side=="left"){
  for(i in 1:length(rules_right)){
  denom<- denom + setequal(rule_right, rules_right[[i]])
  }
  }else if(side=="right"){
    for(i in 1:length(rules_left)){
    denom<- denom + setequal(rule_left, rules_left[[i]])
    }
  }
  likelih<- likelih/denom
  return(likelih)
}


likelihood_production<- function(rule, alpha1, alpha2, gamma1, nonterminals_vec_all, proba_emission, rules ){
base_likelih<- base_likelihood_production(rule,alpha1,gamma1,nonterminals_vec_all,proba_emission)

likelih<- alpha2*base_likelih

if(length(rules)>0){
for(i in 1:length(rules)){
likelih<- likelih + setequal(rule, rules[[i]])
}
}
likelih<- likelih/(length(rules)+ alpha2)
return(likelih)
}
  
  
  