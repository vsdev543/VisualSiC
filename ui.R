side<-bs4DashSidebar(skin = 'light',collapsed = T,
                     bs4SidebarMenu(id = 'mainM',
                                    bs4SidebarMenuItem(text = "Data input",icon = icon('database'),tabName = 'dataIn',badgeColor = 'warning',selected = T),
                                    bs4SidebarMenuItem(text = "Data processing",icon = icon('cogs'),tabName = 'dataPr',badgeColor = 'warning'),
                                  bs4SidebarMenuItem(text = "Visualization",icon = icon('chart-bar'),tabName = 'vizT',badgeColor = 'warning')
                     )
)


cbar<-bs4DashControlbar(id = "lmn",skin = 'light',uiOutput(NS(namespace = "viz",id = 'cbarUI')),overlay = F,pinned = T)

nav<-bs4DashNavbar(title = bs4DashBrand(tags$b("VisualSiC!"),image = "logo.jpg"),leftUi = uiOutput('navLeft'),rightUi = uiOutput('navRight'))


body<-bs4DashBody(
  bs4TabItems(
    bs4TabItem(tabName = "dataIn",
               useCss("style.css"),
               
               tags$head(
                 HTML(
                 '<link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">
                   <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
                   <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
                   <link rel="manifest" href="/site.webmanifest">'
                 )
               ),
               fluidRow(
                 column(3,useShinyalert(force = T),
                        fileInput('genDIn',"Genetics Data set (cbmc.rna)",accept = ".txt",width = "100%"),
                        fileInput('annDIn',"Annotation (cell_annotation)",width = "100%"),
                        tags$i(icon("info-circle")," Input handlers above are reactive, once data is uploaded, only the first 10 rows will be read.
                               Data read will be displayed in the right side. \n Confirm that the data are as it intend and go to the data proccessing tab.
                               "),#hr(),
                        #actionBttn(inputId = "lockD","Lock Data",icon = icon('lock'),style = 'stretch',block = T,color = 'primary')
                 ),
                 column(9,
                        tabsetPanel(
                          tabPanel(title = "Gene Data preview",
                                   # br(),
                                   div(class='lightBg pad10',
                                    DTOutput('genDdt')%>%withSpinner()
                                   )
                                   ),
                          tabPanel(title = "Annotation Data preview",
                                   # br(),
                                   div(class='lightBg pad10',
                                       DTOutput('annDdt')%>%withSpinner()
                                       )
                          )
                                   
                        )
                 )
               )
    ),
    bs4TabItem(tabName = "dataPr",
      fluidRow(column(7,
                      fluidRow(column(6,offset = 6,
                      uiOutput("downSUI"))),
                      uiOutput("dPro_step"),
                      hr(),
                      fluidRow(
                        column(6,offset = 6,
                      fluidRow(
                        column(6,
                               actionBttn("resetDPr","Reset",style = 'stretch',color = 'danger',block = T,icon=icon("redo-alt"))
                               ),
                        column(6,
                               actionBttn("doDPr","Process Data",icon = icon('cogs'),style = 'stretch',color = 'success',block = T)
                        )
                      )
                      ))
                      ),
               column(5,
                      # selectInput("func","Function",choices = modalNames,width = "100%"),
                      div(style="background-color:#fff;color:#333;padding:10px;border-radius:10px;",
                      fluidRow(
                        column(5,
                      radioGroupButtons("func","Function",choices = modalNames,width = "100%",direction = "vertical")),
                      column(6,
                      uiOutput("dPro")
                      )
                      )
                      )
                      )
               )
    ),
    bs4TabItem(tabName = "vizT",
               viz_UI(id="viz")
               )
  )
)

ui<-dashboardPage(header = nav,sidebar = side,body = body,controlbar = NULL,title = "VisualSiC!",fullscreen = TRUE,dark = F)