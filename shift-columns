### This function shifts all the columns of a matrix n places to the right.
### ARGUMENTS :::: matriz = matrix, lugares = places (how many places you want to shift your columns to the right)


desplazar <- function(matriz, lugares=0){
  
  nueva <- matrix(ncol = ncol(matriz), nrow = nrow(matriz)) 
  
  for(ind in 1:ncol(matriz)){  
    
    col <- matriz[,ind]  
    
  if(ind + lugares <= ncol(matriz)){  
    
    nueva[,ind + lugares] <- col 
  
  } else { nueva[,(ind + lugares) - ncol(matriz)] <- col } 

    
  }
    
return(nueva) 
  
}

##### Example:

# mi_matriz <- matrix(1:30, ncol = 5, nrow = 6)
# print(mi_matriz)
# desplazar(mi_matriz, 3) 
