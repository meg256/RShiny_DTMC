# Rshiny interface for discrete-time Markov chain model
# M. Godec meg256@cornell.edu
# last edit July 2021

##############################################
#LOAD PACKAGES
##############################################

library(shiny)
library(shinyMatrix)
library(pomp)
library(ggplot2)
library(magrittr)
library(markovchain)


##############################################
#DEFINE VARIABLES AND FUNCTIONS
##############################################

#Our five compartments
states = c("Lumen","Adherent","Internalized","Submucosal","Death")

#Predefined starting transition matrix values
transmat=matrix(c(0.3,0.2,0,0,0.5,
                  0.1,0.7,0.1,0,0.1,
                  0.01,0,0.59,0.1,0.3,
                  0,0,0,0.5,0.5,
                  0.01,0.01,0.01,0.01,0.96),nrow=5,byrow=T,dimnames=list(states,states))

#Function to get fvals as dataframe
fvals<-function(mchain,initialstate,n){
  out<-data.frame() #data frame that we're gonna bind output into
  names(initialstate)<-names(mchain) 
  for (i in 0:n) #n timesteps
  {
    iteration<-initialstate*mchain^(i)
    out<-rbind(out,iteration) 
  }
  out<-cbind(out,i=seq(0,n))
  out<-out[,c(4,0:4)] #0 to 4 = five states - need to update this w/o hardcode 
  return(out)
}

##############################################
#CONSTRUCT UI
##############################################

ui=fluidPage(
  titlePanel("Interactive DTMC"),
  sidebarPanel("sidebarpanel",
                 sliderInput("time","Number of timesteps",min=10,max=1000,value=100),
                 sliderInput("num","Population",min=10,max=1000,value=100),
               div("Transition matrix. Each cell is probability of transition FROM row TO column. Rows must sum to 1.0",style="color:red"),  
               matrixInput("transmat", value=transmat,#start with predetermined transmat values
                    rows=list(names=TRUE), #show row names
                    cols=list(names=TRUE), #show col names
                    class="numeric"
                 ),
                 actionButton("do","Update")
    ),
    mainPanel("main panel",
      fluidRow(splitLayout(cellWidths=c("20%","20%","20%","20%","20%"),plotOutput("da1"),plotOutput("da2"),plotOutput("da3"),plotOutput("da4"),plotOutput("da5")))
    )
)

   
##############################################
#SERVER FUNCTIONS
############################################## 
server<-function(input,output) {
  
  reacvals <- reactiveValues(da1=NULL,da2=NULL,da3=NULL,da4=NULL,da5=NULL)
  
  observeEvent(input$do, { #when update button is pressed
    initstate=c(input$num,0,0,0,0)
    tr=input$transmat
    
    #check if all rows add to 1
    infect<-new("markovchain",states=states,byrow=T,transitionMatrix=transmat,name="Infection subpops")
    da=fvals(mchain=infect,initialstate=initstate,n=input$time)
    names(da)[names(da)=='Submucosal.1'] <- 'Death' 
  
    output$da1 <- renderPlot({
      reacvals$da1<-plot(da[,1],ylab=names(da[1]),type="l")
    })
    output$da2 <- renderPlot({
      reacvals$da2<-plot(da[,2],ylab=names(da[2]),type="l")
    })
    output$da3 <- renderPlot({
      reacvals$da3<-plot(da[,3],ylab=names(da[3]),type="l")
    })
    output$da4 <- renderPlot({
      reacvals$da4<-plot(da[,4],ylab=names(da[4]),type="l")
    })
    output$da5 <- renderPlot({
      reacvals5<-plot(da[,5],ylab=names(da[5]),type="l")
   })
  }
)}


##############################################
#CREATE SHINY obj
##############################################

shinyApp(ui,server)
