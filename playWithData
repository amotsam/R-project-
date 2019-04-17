#set environment
setwd("C:/Users/amotsam")

#Packges needed 

install.packages("tidyverse")
install.packages("dplyr")    # for grouping (and show some SQL skills)
install.packages("ggplot2")

#Load the data
shapesDF<-read.csv("shapes.csv") 



     #Before start lets check missing values
      any(is.na(shapesDF))  #no missing values
      any(shapesDF == 0)    # no zeros
     
       #question 1 boxplot
         
        boxplot(area~shape,data=shapesDF, main="area by shape", 
        xlab="shape", ylab="area distribution", 
        varwidth = FALSE)  #The maximum point of circles make it hard to read the plot
        
    
       #question 2 max, mean and sd of area size per color
        summary(shapesDF$color) # blue, green, red, yellow
      
      
        tapply(shapesDF$area,shapesDF$color , mean)
        tapply(shapesDF$area,shapesDF$color , median)
        tapply(shapesDF$area,shapesDF$color , sd)
      
      #More suitable solution will be group_by 
       group_by(shapesDF,color) %>%
        summarise(
          mean = mean(area, na.rm = TRUE),
          sd = sd(area, na.rm = TRUE),
          max= max(area, na.rm=TRUE)
        )
      
      #question 3- yeallow square average size is 3333.207
      
        tb1<-subset(shapesDF ,color=="yellow" & shape== "square")
        mean(tb1$area)
      
       #can be done also with groub_by
      group_by(shapesDF,shape,color) %>%  
        summarise(
          mean = mean(area, na.rm = TRUE),
          sd = sd(area, na.rm = TRUE),
          max= max(area, na.rm=TRUE)
          
        )

      #question 4- most likely to be green is circle with 25.83%
      tab<-table(shapesDF$shape, shapesDF$color)
      tab
      tab1prop<-prop.table(tab,margin=1)*100 #make table percentaged
      tab1prop
      barplot(tab1prop,legend.text =T , col=c("red", "blue", "green"))
      
      
      #question5.given object is red and areas size > 3000. whats the chances to any shape
      
      tab2<-subset(shapesDF,shapesDF$area>3000 & shapesDF$color=="red")
      tab2
      prop.table(summary(tab2$shape)) # answer 
      
      # question 6 Function calc the side/radius of a shape
      
         #Function gets 2 arguments: shapeN-String, area-Integer
         #return the side/radius depending on the shape
      
         shape_side<-function(shapeN,area) {
        
           if(shapeN == "square")
             return( round(sqrt(area)))
           
           else if(shapeN == "circle")
             return( round(sqrt(area/3.14)))
           
           else if(shapeN =="triangle")
             return  (round(sqrt(area/(sqrt(3)/4))))
         }
           
      # question7. apply the function 
      
      table1<-mapply(shape_side,shapesDF$shape,shapesDF$area)
      shapesDF$side<-table1  
      
      # question8 
      
      #compare shape distribution by area and side 
      par(mfrow=c(1,2))
      
      boxplot(side~shape,data=shapesDF, main="side by shape", 
              xlab="shape", ylab="side distribution")
       
       boxplot(area~shape,data=shapesDF, main="area by shape", 
               xlab="shape", ylab="area distribution", 
               varwidth = TRUE)
       
      # I can infer that distribution of side/radius by its shape, 
      # is more symetric compare to area distribution by shape. 
    
      
      
      
      
                     
      
