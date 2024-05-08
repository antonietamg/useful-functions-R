### This function creates a reversed version of a matrix, where the last column of the original matrix becomes into the first column of the new matrix, 
### the penultimate column becomes the second one and so on...

reverse_matrix <- function(matriz){
  
  nueva <- matrix(ncol = ncol(matriz), nrow = nrow(matriz)) 
  s=0
  
  for(ind in 1:ncol(matriz)){  
    
    col <- matriz[,ind]  
    
    nueva[,ncol(matriz) - s] <- col 
    
    s = s + 1 
    
  }
  
  return(nueva) 
  
}


### Example:

# mi_matriz <- matrix(1:30, ncol = 5, nrow = 6)
# reverse_matrix(mi_matriz)
