library(ggplot2)
library(plotly)
library(shiny)


# Loading Functions 
DensityNB<-function(R=100, a=5, c=40, K=200, T=1000, H0=10, P0=1){ 
  
  H=rep(NA,T) #T=length of simulation time series
  P=rep(NA,T)
  
  H[1]=H0
  P[1]=P0
  
  for(t in 2:T){
    H[t]=H[t-1]*exp(R*((R*H[t-1])/K)-a*P[t-1])
    P[t]=c*H[t-1]*(1-exp(-a*P[t-1]))
  }
  res=list(H=H, P=P)
  return(res)
} 


# ui.R ----
ui <- fluidPage(
  titlePanel("Density Dependent Nicholson-Bailey Model"),
  sidebarLayout(position="left",
                sidebarPanel(
                  sliderInput(inputId = "Rep", 
                              label = "Select R (Host reproductive Rate)", 
                              min=0, max=30, value= c(1)),
                  sliderInput(inputId = "egg", 
                              label = "Select c (Average parasitoid eggs laid per host)",
                              min=0, max=50, value= c(1)),
                  sliderInput(inputId = "search", 
                              label = "a (searching efficiency)",
                              min=0, max=40, value= c(1)),
                  sliderInput(inputId = "carrying", 
                              label = "K (carrying capacity of host)",
                              min=0, max=100, value= c(11))),
                
                mainPanel(
                  plotOutput("nbplot"),
                  
                )
  )
)


# server.R ----
server <- function(input, output) {
  result<-reactive({DensityNB(input$Rep,input$search, input$egg,input$carrying)})
  simdata<-reactive({data.frame(H=result()$H, P=result()$P, Generation=1:1000)})
  
  output$nbplot<-renderPlot(ggplot(data=simdata(),aes(x=simdata()$Generation, y=simdata()$H))+geom_point()+geom_point(aes(y=simdata()$P),color="red")+ylab("Individuals")+xlab("Generations"))
#red is parasitoids, black is host
  #Add legend for hosts and parasitoids
}

# Run the app ----
shinyApp(ui = ui, server = server)

