#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  title = 'Perfect Store',
  theme = bs_theme(bootswatch = "lux"),
  
  titlePanel("Stockout"),
  fluidRow(
    column(4, wellPanel(
      
      numericInput('stockout','¿Cuántas cajas de snickers faltan en el anaquel?',value = 0,min = 0),
      br(),
      checkboxInput("checkbox", label = "¿Hubo invasión de anaquel?", value = FALSE),
      
      
      br(),
      actionButton("goButton", '¿Qué hago?')
    )),
    column(8,
           h4('El mensaje que le llega al promotor es:'),
           textOutput("summary"),
           hr(),
           h4('Costo por stockout:'),
           textOutput("costS"),
           h4('Costo por invasión de anaquel:'),
           textOutput("costI")

    )
  ),
  
  titlePanel("¿Cómo funciona?"),
  p('Fijamos el valor mínimo de cajas en exhibición por tienda en 8 para saber cuántas cajas pedirle al promotor que coloque en el anaquel.
    El valor mínimo  es aleatorio con oportunidad de parametrizarse según las reglas del negocio.'),
  p('Asignamos un valor a la caja de snickers de $1000 considerando la venta pérdida en días. El valor nuevamente es arbitrario y con fines ilustrativos pero puede adaptarse según
    la categoría, formato de tienda, ubicación, etc.'),
  p('Calculamos el costo por stockout multiplicando las cajas faltantes por el valor por caja.'),
  p('De manera similar, nuestro costo por invasión de anaquel lo fijamos en $2000 considerando la venta potencial que se perdió y el
  costo de volver a adquirir al cliente en caso de que cambie su preferencia de consumo.'),
  p('Una regla más. Si la foto detecta que faltan a lo más 2 cajas, podemos considerar que fue debido a la demanda del momento y solo resurtir.
    Más de 3 cajas faltantes son un tema de negocio y repercuten en los costos. Invasión de anaquel siempre se considera costo. Nuestra métrica
    como equipo es tener la gráfica de costos lo más cercana a 0 posible generando reglas que den instrucciones precisas a los promotores.'),
  
  fluidRow(
    column(4,
           h4('Costo acumulado total:'),
           textOutput("costTotal"),
           h4('Costo acumulado por stockout:'),
           textOutput("costStotal"),
           h4('Costo acumulado por invasión de anaquel:'),
           textOutput("costItotal")

    ),
    column(8,
           h4('Costos: mensual acumulado'),
           plotlyOutput('costPlt')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  # In the code above, the call to isolate() is used inline in
  # a function call. However, isolate can take any expression,
  # as shown in the code below.
  output$summary <- renderText({
    input$goButton
    
    # Any sort of expression can go in isolate()
    isolate({
      stockout <- isolate(input$stockout)
      invasion <- ifelse(input$checkbox==T, ' y avisa de inmediato al gerente.', '.')
      restock <- ifelse(isolate(input$stockout)<8, '', ' Se notifica a corporativo para resurtir tienda.')
      if(stockout==0){
        'Buen trabajo!'
      } else{
      paste0('Coloca ', ifelse(isolate(input$stockout)<8,(8-isolate(input$stockout)), isolate(input$stockout)), ' caja(s) en el anaquel', invasion, restock)
      }
    })
  })
  
  output$costI <- renderText({
    input$goButton
    
    # Any sort of expression can go in isolate()
    isolate({ifelse(input$checkbox==T, 2000, 0)})
  })
  
  output$costS <- renderText({
    input$goButton
    
    # Any sort of expression can go in isolate()
    isolate({input$stockout*1000})
  })
  
  totalI <- reactiveValues(total = 0) # Defining & initializing the reactiveValues object
  observeEvent(input$goButton, {
    totalI$total <-  totalI$total +ifelse(input$checkbox==T, 2000, 0)     # if the add button is clicked, increment the value by 1 and update it
  })
  
  output$costItotal <- renderText({
    input$goButton
    totalI$total
    
  })
  
  totalS <- reactiveValues(total = 0) # Defining & initializing the reactiveValues object
  observeEvent(input$goButton, {
    totalS$total <-  totalS$total + (input$stockout * 1000)    # if the add button is clicked, increment the value by 1 and update it
  })
  
  output$costStotal <- renderText({
    input$goButton
    totalS$total
    
  })
  
  output$costTotal <- renderText({
    input$goButton
    totalS$total + totalI$total
    
  })
  
  output$costPlt <- renderPlotly({
    input$goButton
    meses <- c("October", "November", "December (current)")
    invasion <- c(2000, 14000, totalI$total)
    stockout <- c(12000, 18000,totalS$total)
    data <- data.frame(meses, invasion, stockout)
    
    fig <- plot_ly(data, x = ~meses, y = ~stockout, type = 'bar', name = 'Stockout',  marker = list(color ='rgba(171, 50, 96, 0.6)'))
    fig <- fig %>% add_trace(y = ~invasion, name = 'Invasión', marker = list(color ='rgba(201, 0, 0, 0.6)'))
    fig <- fig %>% layout(yaxis = list(title = 'Cost'), barmode = 'stack')
    

    
    fig
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
