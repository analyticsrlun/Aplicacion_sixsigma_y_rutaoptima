library(shiny)
library(GA)
library(ggplot2)
library(plotly)
library(grid)
##################################CONTROL############################################################
datos1 <- read.csv("datoscartas.csv", sep=";")
attach(datos1)
a <- ggplot(datos1, aes(x=K, y=RANGO )) + 
    geom_line(colour="red")  + 
    geom_point( size=2, shape=20, fill="white", colour="red") + 
    geom_hline(yintercept=9.93984, colour="blue") + 
    geom_hline(yintercept=4.96, colour="orange", linetype="dashed") +
    geom_hline(yintercept=0, colour="green") +#Linea horizontal cuando y es igual a 0
    theme_minimal()+
    labs(x="Numero de observaciones",y="Rango")+
    ggtitle('Carta de rangos')+
    theme(plot.title = element_text(hjust=0.5))
b <- ggplot(datos1, aes(x=K, y=MEDIA )) + 
    geom_line(colour="red")  + 
    geom_point( size=2, shape=20, fill="white", colour="red") + 
    geom_hline(yintercept=180.144,colour="orange", linetype="dashed") + 
    geom_hline(yintercept=183.0049, colour="blue") +
    geom_hline(yintercept=177.2831, colour="green") +#Linea horizontal cuando y es igual a 0
    theme_minimal()+
    labs(x="Numero de observaciones",y="Media")+
    ggtitle('Carta de medias')+
    theme(plot.title = element_text(hjust=0.5))
############################################ SERVER  ##############################################
shinyServer(function(input, output) {
   
    output$plot1 <- renderPlot({
        datos=read.csv(file = "DatosDist.csv", sep=";")
        rownames(datos)=colnames(datos)
        datos <- datos[!(names(datos) %in% input$b),!(names(datos) %in% input$b)]  
        MaDis=as.matrix(datos)
        
        LuDis=as.dist(MaDis)
        
        # Función para calcular la longitud del tour
        
        tourLength <- function(tour, distMatrix) {
            tour <- c(tour, tour[1])
            route <- embed(tour, 2)[,2:1]
            sum(distMatrix[route])
        }
        
        # Función a maximizar
        
        tspFitness <- function(tour, ...) 1/tourLength(tour, ...)
        
        # Iterar. Optimizar para encontrar la solución
        
        GA <- ga(type = "permutation", fitness = tspFitness, distMatrix = MaDis,
                 min = 1, max = attr(LuDis, "Size"), popSize = 50, maxiter = 5000,
                 run = 500, pmutation = 0.2)
        
        # Establecer coordenadas para el plano
        
        mds <- cmdscale(LuDis)
        
        x <- mds[, 1]
        y <- -mds[, 2]
        plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
        abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
               col = "light gray")
        tour <- GA@solution[1, ]
        tour <- c(tour, tour[1])
        n <- length(tour)
        arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
               length = 0.15, angle = 25, col = "red", lwd = 2)
        text(x, y, labels(LuDis), cex=0.8)
    })

        output$cr <- renderPlotly({
            ggplotly(a)
        })
        output$cm <- renderPlotly({
            ggplotly(b)
        })
        output$comp <- renderPlotly({
            df <- data.frame(x=c(178,179,180,181,182),
                             y=c(-1,-1,-1,-1,-1))
            
            datos2 <- read.csv("comportamientodatos.csv", sep=";")
            attach(datos2)
            colnames(datos2)<-c("Diametro","Frecuencia") 
            c <- ggplot(data=datos2, aes(x=Diametro, y=Frecuencia)) + 
                geom_bar(stat="identity", position="stack",colour="black", 
                         fill="lightblue")+
                geom_vline(xintercept=180, colour="red",linetype="dashed",
                           size=1) +
                theme_minimal()+
                labs(x="Diámetros",y="Frecuencia")+
                ggtitle('Comportamiento de los datos')+
                theme(plot.title = element_text(hjust=0.5))+
                geom_line(data=df,aes(x,y))+
                annotate("text", x=c(178,182,180), y=-2, label= c("EI=178mm","ES=182mm","VN"))+
                geom_vline(xintercept=178, colour="red",linetype="dashed",size=0.5)+
                geom_vline(xintercept=182, colour="red",linetype="dashed",size=0.5)
            ggplotly(c)
            
        })
        
        output$pareto <- renderPlotly({
            datos<-read.csv("Datos Defectos.csv")
            numdefect<-datos[,-3]
            numdefect$Defectos<-factor(numdefect$Defectos,levels = numdefect$Defectos)
            numdefect$cum<-cumsum(numdefect$Cantidad)
            numdefect.sum<-sum(numdefect$Cantidad)
            numdefect$cum_perc<-50*numdefect$cum/numdefect.sum
            pareto1<-ggplot(numdefect)+
                geom_bar(aes(x=Defectos,y=Cantidad,fill=Defectos),stat = 'identity')+
                theme_minimal()+theme(axis.text.x = element_blank(),legend.position = "none")+geom_point(aes(x=Defectos,y=cum_perc),size=1)+
                geom_path(aes(x=Defectos,y=cum_perc,group=1))+scale_y_continuous("Cantidad",sec.axis = sec_axis(~c(0,100),name="Porcentaje"))+
                scale_fill_brewer(palette="Dark2")
            ggplotly(pareto1)
        })

    
    


})
