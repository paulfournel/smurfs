require(schoolmath)
require(pracma)
number_of_smurfs = 3

has_solution = function(mat){
  return(any(rowSums(mat)==dim(mat)[1]))
}

infos = function(day, smurfs){
  prime_decomposition = as.integer(factorize(day))
  if(any(prime_decomposition<=max(smurfs))){
    return(unique(prime_decomposition[prime_decomposition<=max(smurfs)]))
  }
  return(NULL)
}

test_smurfs = function(number_of_smurfs){
  smurfs = primes(100000)[2:(number_of_smurfs+1)]
  knowledge = matrix(rep(0, length(smurfs)*length(smurfs)), length(smurfs))
  day = 1
  light = F
  while(!has_solution(knowledge)){
    day = day + 1
    selected_smurf = sample(1:length(smurfs),1)
    knowledge[selected_smurf, selected_smurf] = 1
    if(light){
      knowledge[selected_smurf, match(infos(day, smurfs), smurfs)] = 1
    }
    if(all(is.element(infos(day+1, smurfs), smurfs[knowledge[selected_smurf,]==1]))){
      light = T
    }else{
      light = F
    }
  }
  return(day)
}

# Simulating for different number of sumrfs
N = 50
size = sample(3:100, N, replace=T)
res = rep(0, N)

for(i in 1:N){
  res[i] = test_smurfs(size[i])
  print(i)
}

plot(size, res)

