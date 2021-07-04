#When I need to create a dataframe naming my variables, I use this function to write some of the variables names.
#In "x" I write the name or string I want to repeat "z" times. In "y" I define the max number that my variable will get.
#See how it works in: https://progynerdismo.wordpress.com/2021/07/04/r-en-espanol-funcion-para-repetir-un-nombre-con-una-variable/

##### REPEAT WITH SPRINTF FUNCTION

rep_sprintf <- function(x,y,z){
  
  df1 <- data.frame()
  for(i in 1:y){
    tt <- sprintf(x,i)
    tt <- data.frame(rep(tt,z))
    df1 <- rbind(df1, tt)
  }
  return(df1)
}

##### EXAMPLE

#x = "AB%dLin" # name or string to repeat
#y = 5  # maximum value for %d (it starts in 1)
#z = 3 # number of times that x will be repeated with each %d value

# lin <- rep_sprintf(x,y,z)
# lin
# 
# rep.tt..z.
# 1      AB1Lin
# 2      AB1Lin
# 3      AB1Lin
# 4      AB2Lin
# 5      AB2Lin
# 6      AB2Lin
# 7      AB3Lin
# 8      AB3Lin
# 9      AB3Lin
# 10     AB4Lin
# 11     AB4Lin
# 12     AB4Lin
# 13     AB5Lin
# 14     AB5Lin
# 15     AB5Lin

