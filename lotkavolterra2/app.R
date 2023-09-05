## Basic Lotka-Volterra model
##
## implemented by tpetzoldt as a shiny app

library("deSolve")
library("dygraphs")

LV <- function(t, state, parameters) {
  with (as.list(c(state, parameters)), {
    growth     <- a * Prey
    grazing    <- b * Prey * Pred
    mortality  <- e * Pred
    dPrey_dt   <-     growth  - grazing
    dPred_dt   <- g * grazing - mortality
    list(c(dPrey_dt, dPred_dt))     # output variable
  })
}

y0    <- c(Prey=1, Pred=0.5)          # state variable initial conditions
y_label <- c("Prey", "Predator")

parms <- c(a=0.1, b=0.1, e=0.1, g=1)  # parameter values
parms_label <- c("a: prey growth", "b: grazing", "e: predator mortality", "g: grazing efficiency")

makelist <- function(i, obj, min=NA, max=NA, step=NA, width=NULL, label=NULL) {
  if (is.null(label)) label = names(obj)
  list(inputId=names(obj[i]), label=label[i],
       value=unname(obj[i]), min=min, max=max, step=step,
       width=width)
}

## two lists of lists
L_y0    <- lapply(1:length(y0), makelist, obj=y0, min=0, max=5, step=0.1, width=200, label=y_label)
L_parms <- lapply(1:length(parms), makelist, obj=parms, min=0, max=1, step=0.02, width=200, label=parms_label)

server <- function(input, output) {
  simulation <- reactive({
    y0    <- c(Prey=input$Prey, Pred=input$Pred)
    parms <- c(a=input$a, b=input$b, g=input$g, e=input$e)

    times <- seq(0, 500, 1)
    out <- ode(y0, times, LV, parms, method="bdf", atol=1e-8, rtol=1e-8)
    out
  })

  output$timeseries <- renderDygraph({
    out <- as.data.frame(simulation())[c("time", "Prey", "Pred")]
    ymax <- max(5, out$Prey, out$Pred)
    dygraph(out, group="grp") %>%
      dySeries("Prey", color="green", label="Prey") %>%
      dySeries("Pred", color="red", label="Predator") %>%
      dyAxis("y", label = "relative units", labelHeight = 12, valueRange = c(0, ymax)) %>%
      dyAxis("x", label = "time units", labelHeight = 18) %>%
      dyOptions(
                fillGraph = TRUE,
                #stackedGraph = TRUE,
                fillAlpha = 0.3,
                animatedZooms = TRUE) %>%
      dyLegend(labelsSeparateLines = TRUE)
  })

}

ui <- fluidPage(
  #includeHTML("www/header_ihb_en_.html"), # <---
  headerPanel("Predator-Prey model"),

  #p("This application implements a Lotka-Volterra Predator-Prey model."),

  sidebarLayout(
    sidebarPanel(
      ## generic creation of UI elements
      h3("Initial values"),
      lapply(L_y0, function(x) do.call("numericInput", x)),

      h3("Parameters"),
      lapply(L_parms, function(x) do.call("sliderInput", x)),

      h3("More information"),
      a(href="https://github.com/tpetzoldt/", "Github page"), br(),
      a(href="https://tu-dresden.de/Members/thomas.petzoldt", "Author homepage"), br()
    ),
    mainPanel(
    h2("Model"),
## preformatted ------------------------------
pre("
growth     = a * Prey
grazing    = b * Prey * Pred
mortality  = e * Pred

dPrey_dt   =     growth  - grazing
dPred_dt   = g * grazing - mortality"),
##---------------------------------------------
      h2("Simulation"),
      dygraphOutput("timeseries", height = "500px")
    )
  ),
  #includeHTML("www/footer_en_.html"), # <---
  lang = "en"
)

shinyApp(ui = ui, server = server)
