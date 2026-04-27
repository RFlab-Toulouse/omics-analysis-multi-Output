library(shiny)

THEMES = c("cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal",
            "litera", "lumen", "lux", "materia", "minty", "pulse", "sandstone", "simplex", "sketchy",
            "slate", "solar", "spacelab", "superhero", "united", "yeti",
            "quartz", "morph", "minty", #"vapor" ,
            "superhero")
shinyUI(fluidPage(
  theme = bslib::bs_theme(
     bootswatch = "minty",
     #primary = "#EA80FC", 
     secondary =  "#32a8a2" #"#48DAC6"
    ),
  # div(input_dark_mode(id = "mode",
  #                     mode = "light"),
  #      style = "font-size:15px; color:grey; text-align:right;"
  #      ),
  # 
  # titlePanel(
  #   tags$div(
  #     #"Omics analysis",
  #     tags$div(
  #       class = "float-end",
  #       style = "margin-top: -42px; min-width: 150px;", 
  #       selectInput( "theme_app", 
  #                     # label = tags$strong("Themes"),
  #                     label =  "Menu theme",
  #                     choices = setNames(THEMES, tools::toTitleCase(THEMES)), 
  #                     selected = "cerulean"
  #       )
  #     )
  #   )
  # ),
  
  # Application title
  
  titlePanel(
    div(
      img(src = 'dna-structure.png', height = "40px", width = "40px"),
      "Omics analysis : multi-label classification"
     ) 
    ),
  hr(nrow = 2),
  sidebarLayout(
    sidebarPanel(
      wellPanel( 
        conditionalPanel(condition ="input.confirmdatabutton==0" ,
                         radioButtons("analysis","",c("new analysis","previous analysis"),inline=T),
                         conditionalPanel( condition="input.analysis=='previous analysis' ",     
                                           fileInput("modelfile",label=h4("previous analysis"),accept=".RData")
                         ), 
                         conditionalPanel(condition="input.analysis=='new analysis' ",
                                          fluidRow(
                                            column(12,br(),radioButtons("filetype", "Extention of the file",c("csv" = "csv", "xlsx" = "xlsx"),inline = TRUE))
                                          ),
                                          fluidRow(
                                            column(12,conditionalPanel(condition ="input.help",
                                                                       helpText("Learning file is obligatory to continue")
                                            ),
                                            fileInput("learningfile", 
                                                      label = h4("learning File"),
                                                      accept =  c("text/csv",
                                                                  "application/vnd.ms-excel",
                                                                  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                                                  ".xls",".xlsx"))
                                            )
                                            ,
                                            column(12,
                                                   fileInput("validationfile", label = h4("validation File "),accept =  c("text/csv","application/vnd.ms-excel","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xls",".xlsx")) 
                                            )
                                          ),
                                          fluidRow(
                                            conditionalPanel(condition ="input.filetype=='csv' ",column(6,textInput('dec', 'character for decimal point',value = "." ))),
                                            column(6,textInput("NAstring", label = "characters for missing values",value = "NA"))
                                          ),   
                                          fluidRow(
                                            conditionalPanel(condition ="input.filetype=='csv' ",
                                                             radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),inline = TRUE )),
                                            conditionalPanel(condition ="input.filetype=='xlsx' ",
                                                             column(6,numericInput("skipn",label = "number of lines to skip",value = 0)),
                                                             column(6,numericInput("sheetn",label = "sheet",value = 1)))
                                          ),hr(),
                                          checkboxInput("transpose","Transpose the table",FALSE),
                                          checkboxInput("zeroegalNA","consider 0 as NA",FALSE)
                         )
                         ,
                         actionButton("confirmdatabutton","Confirm data", 
                                      style = "background-color: #63BFBF;
                                  color: white;
                                  border-color: #63BFBF;"),
                         conditionalPanel(condition ="input.help",
                                          helpText("Data has to be confirm to continue"))
        ),
        conditionalPanel(condition ="input.confirmdatabutton!=0",
                         h4("Learning data"),
                         textOutput("namefilelearn",inline=T), tags$head(tags$style("#namefilelearn{color: grey;font-size: 15px;font-style: italic;}")),
                         br(),
                         textOutput("dim1learn",inline=T), "lines (individuals)",
                         br(),
                         textOutput("dim2learn",inline=T), "columns (variables)",
                         br()
        ),
        conditionalPanel(condition ="input.confirmdatabutton!=0 & output.fileUploadedval",
                         h4("Validation data"),
                         textOutput("namefileval",inline=T), tags$head(tags$style("#namefileval{color: grey;font-size: 15px;font-style: italic;}")),
                         br(),
                         textOutput("dim1val",inline=T), "lines (individuals)",br(),
                         textOutput("dim2val",inline=T), "columns (variables)",
                         br()
        ), 
        conditionalPanel(condition ="input.confirmdatabutton!=0",
                         hr(),
                         fluidRow(
                           column(1
                                    ,
                                   shinyjs::disabled(
                                     checkboxInput("invers", " " , value = FALSE)
                                   )
                                  ),
                           column(11,
                                  h4("Classes in learning dataset:"),
                                  textOutput("positif",inline=T)
                                  # p(textOutput("positif",inline=T),HTML( '&#x21D2;'), "case ",br(),
                                  #   textOutput("negatif",inline=T),HTML( '&#x21D2;'), "control",align="center")
                           )
                           
                           # column(12,
                           #        h4("Classes in dataset:"),
                           #        textOutput("class_summary"),
                           #        tags$head(tags$style("#class_summary{color: #007bff;font-size: 16px;font-weight: bold;}")),
                           #        br(),
                           #        uiOutput("class_count_indicator"),
                           #        tags$head(tags$style("#class_count_indicator{color: #28a745;font-size: 15px;font-weight: bold;background-color: #f0f9ff;padding: 8px;border-radius: 5px;border-left: 4px solid #28a745;}"))
                           # )
                         ),
                         hr(),
                         radioButtons("paramdownplot","Download images as",choices=list("png"="png","jpg"="jpg","pdf"="pdf"),selected="png"),
                         radioButtons("paramdowntable","Download datasets as",choices=list("csv"="csv","xlsx"="xlsx"),selected="csv"),
                         hr(),
                         downloadButton("savestate","Save settings RData",class = "dlButton"),
                         hr(),
                         downloadButton("savestatetable","Save settings table and main results",class = "dlButton")
                         
        ),
        hr(),
        checkboxInput("help","show help",FALSE)
      ),width=3      
    ) ,
    
    mainPanel(
      conditionalPanel(condition ="!output.fileUploaded & !output.modelUploaded",
                       h3("The aim of this application is to provide a user-friendly tool for creating a model to 
                          predict glomerular filtration rate (GFR) based on omics data.",
                          align="center"),
                       h4("Check the box 'show help' for any further informations."),br(),br(),br(),
                       
                       
                       fluidRow(column(6,imageOutput("image1")),column(2,imageOutput("image2"))),
                       br(),
                       h4("This application is developped in the 12th team of I2MC for internal used.",align="center")
                       
      ),           
      conditionalPanel(condition ="output.fileUploaded || output.modelUploaded",
                       tabsetPanel(id = "data",              
                                   tabPanel("Learning Data", icon = icon("clipboard-list"),
                                            br(),
                                            conditionalPanel(condition ="input.help",
                                                             fluidRow(
                                                               column(5,br(),helpText("To verify if the import parameters are correct : the first column has to be the names of the individual, 
                                       the second the groups. Others are the datas."), 
                                                                      helpText("Non attributes values have to appears empty, "),
                                                                      helpText()
                                                               ),
                                                               column(7,imageOutput("image3",width = "100%")))
                                            ),
                                            dataTableOutput("JDDlearn")%>% withSpinner(color="#0dc5c1",type = 1),
                                            p(downloadButton("downloaddataJDDlearn","Download dataset"),align="center")
                                   ),
                                   tabPanel("Validation Data", icon = icon("check"),
                                            conditionalPanel(condition ="output.fileUploadedval",
                                                             br(),
                                                             dataTableOutput("JDDval")%>% withSpinner(color="#0dc5c1",type = 1),
                                                             p(downloadButton("downloaddataJDDval","Download dataset"),align="center")
                                            )
                                   ),
                                   
                                   tabPanel("Select Data", icon = icon("filter"),
                                            conditionalPanel(condition ="input.help",
                                                             helpText(" Select variables to extract variables from the learning dataset according to the number or the structure of Non-Attribute values (missing values)")
                                            ),  
                                            fluidRow(
                                              column(7, numericInput("prctvalues","Percentage minimum of values" , 0, min = 0, max = 100, step = 5),
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("")),br(),
                                                     checkboxInput("NAstructure", "Select variables with a NA's structure " , value = FALSE),
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("The structure test is a proportion test of the Non Attributes values between the 2 groups."))
                                              ),
                                              column(5,radioButtons("selectmethod","Methods of selection ",c("selection on all samples"="nogroup","each group has more than x% of values "="bothgroups","at least one group has more than x% of more"="onegroup")),
                                                     conditionalPanel(condition ="input.help",helpText("3 ways of selection : select variables which got at least x% of values in all samples, "),
                                                                      helpText("                select variables which which have more than x% in the two groups"),
                                                                      helpText("                select variables which have at leat one group whith more than x% of values"))
                                              )
                                              
                                            ),p(downloadButton('downloaddataselect', 'Download data selected'),align="center"),
                                            hr(),
                                            
                                            fluidRow(
                                              column(7,textOutput("nvarselect",inline=T), "selected variables" ,
                                                     plotOutput("heatmapNA",width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1) ,
                                                     p(downloadButton("downloadplotheatmapNA","Download plot"),downloadButton('downloaddataheatmapNA', 'Download raw data'),align="center")
                                              ),
                                              column(5,br(),
                                                     conditionalPanel(condition ="input.help",helpText("The 3 curves present the number of variables selected according to the three possible options and the % of Na's selected"))
                                                     ,
                                                     plotOutput("plotNA",width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                     p(downloadButton("downloadplotNA","Download plot"),downloadButton('downloaddataplotNA', 'Download raw data'),align="center")
                                              )
                                            ),
                                            hr(),
                                            fluidRow(
                                              column(6,  
                                                     conditionalPanel(condition ="input.NAstructure==true",
                                                                      conditionalPanel(condition ="input.help",helpText("Consider the NA in the group with less values as real 0 (replace by 0) the NA in the group with more values are raplace by the solution chosen later")),
                                                                      numericInput("thresholdNAstructure","pvalue for the structure test" , 0.05, min = 0, max = 1, step = 0.005))
                                              ),
                                              conditionalPanel(condition ="input.NAstructure==true",
                                                               column(6,radioButtons("structdata", "search structure in : ",c("all dataset" = "alldata","selected dataset" = "selecteddata"))))
                                            ), 
                                            hr(),
                                            fluidRow(
                                              conditionalPanel(condition ="input.NAstructure==true", 
                                                               column(9,textOutput("nstructuredfeatures",inline=T),"structured features",
                                                                      plotOutput("heatmapNAstructure" ,width = "95%",height = 600)%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("downloadstructur","Download plot"),downloadButton('downloaddatastructur', 'Download raw data'),align="center")),
                                                               column(3,br(),br(),
                                                                      numericInput("maxvaluesgroupmin","The group with the minimum number of values has at most x% of values",value = 25,min = 0,max = 100,step = 5),
                                                                      numericInput("minvaluesgroupmax","The group with the maximum number of values has at least y% of values",value = 75,min = 0,max = 100,step = 5))
                                              )
                                            )
                                   ),
                                   tabPanel("Transform Data", icon =  icon("magic"),
                                            conditionalPanel(condition ="input.help",
                                                             helpText("")),
                                            fluidRow(
                                              column(5,radioButtons("rempNA", "Replacing NA (Not Attributes) by:",
                                                                    c("zero" = "z","mean of the cohort" = "moy",
                                                                      "mean by group"="moygr","PCA estimation" = "pca","Random forest estimation /!\\" = "missforest")),
                                                     helpText("/!\\ process can be long"),
                                                     
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("Random Forest can "))),
                                              column(3,
                                                     checkboxInput("log","transform data in log",FALSE),
                                                     checkboxInput("standardization","standardization dataset",FALSE),
                                                     conditionalPanel(condition ="input.help",helpText("dividing the columns quadratic mean")),
                                                     checkboxInput("arcsin","arcsine transformation",FALSE),
                                                     conditionalPanel(condition ="input.help",helpText("each column is rescaled between 1 and 0, and arcsin transformation is applying"))
                                              ),
                                              column(4,
                                                     conditionalPanel(condition="input.log",
                                                                      radioButtons("logtype",label = NULL,c("ln"="logn","log 10"="log10","log2"="log2"),inline = TRUE)))
                                            ),p(downloadButton('downloaddatatransform', 'Download transform data '),align="center"),
                                            hr(),
                                            
                                            fluidRow(
                                              column(5,plotOutput("plotheatmaptransformdata" ,width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                     p(downloadButton("downloadplotheatmap","Download plot"),
                                                       downloadButton('downloaddataheatmap', 'Download raw data'),align="center")),  
                                              
                                              column(7,conditionalPanel(condition ="input.help",
                                                                        helpText("The mds (MultiDimensionnal Scaling) calcul the distances between the individuals (rows) and represented it on a plan as well as possible."),
                                                                        helpText("The aim of this graphic is to vizualized if the selection and transform parameters separate well the 2 groups.")),
                                                     plotOutput("plotmds",height=500,width = "100%")%>% withSpinner(color="#0dc5c1",type = 1),
                                                     p(downloadButton("downloadplotmds","Download plot"),
                                                       downloadButton('downloaddatamds', 'Download raw data'),align="center"))),
                                            plotOutput("plothist",height=500,width = "100%")%>% withSpinner(color="#0dc5c1",type = 1),
                                            p(downloadButton("downloadplothist","Download plot"),
                                              downloadButton('downloaddatahist', 'Download raw data'),align="center")
                                   ),
                                   tabPanel("Statistics", icon =  icon("calculator"),
                                            conditionalPanel(condition ="input.help",
                                                             helpText("")),
                                            fluidRow(
                                              column(6,
                                                     radioButtons("test", "Variable Selection Methods",
                                                                  c("No test"="notest",
                                                                    "Kruskal-wallis Test (univariate)" = "Kruskal",
                                                                    # "Clustering + ElasticNet (multivariate)" = "clustEnet",
                                                                    "Anova Test" = "ANOVA",
                                                                    "Lasso (multivariate)" = "lasso",
                                                                    "ElasticNet (multivariate)" = "elasticnet",
                                                                    "Boruta selection" = "boruta"
                                                                    # ,
                                                                    # "Ridge (multivariate)" = "ridge"
                                                                    )
                                                                  ),
                                                     conditionalPanel(condition ="input.help",
                                                                      helpText("Univariate tests: Wilcoxon (non-parametric) and Student (parametric) test each variable independently."),
                                                                      helpText("Multivariate methods: Lasso, ElasticNet, and Ridge use regularization to select variables while considering their joint effects."),
                                                                      helpText("Clustering + ElasticNet: Clusters correlated variables, selects best from each cluster, then applies bootstrap ElasticNet for robust selection.")
                                                                      ),
                                                     checkboxInput("SFtest","Shapiro and Fisher Tests",F),
                                                     conditionalPanel(condition ="input.help",helpText("The shapiro test is a test of normallity. The F test is a test of equality of variance."))
                                              ),
                                              column(6,br(),
                                                     conditionalPanel(condition ="input.test=='Kruskal' || input.test=='ANOVA'",
                                                                      numericInput("thresholdFC","Fold change threshold" , 0, min =0, max = 5, step = 0.5),
                                                                      conditionalPanel(condition ="input.help",helpText("Fold Change is the ratio of means between groups.")),
                                                                      numericInput("thresholdpv","p-value threshold" , 0.05, min =0, max = 1, step = 0.01),
                                                                      checkboxInput("adjustpv", " " , value = FALSE),
                                                                      conditionalPanel(condition ="input.help", helpText("Benjamini & Hochberg correction"))
                                                     ),
                                                     conditionalPanel(condition ="input.test=='clustEnet'",
                                                                      h5("Clustering + ElasticNet Parameters"),
                                                                      numericInput("nclusters","Number of clusters" , 100, min =10, max = 500, step = 10),
                                                                      conditionalPanel(condition ="input.help",helpText("Number of clusters for hierarchical clustering of correlated variables.")),
                                                                      numericInput("nbootstrap","Bootstrap iterations" , 500, min =100, max = 2000, step = 100),
                                                                      conditionalPanel(condition ="input.help",helpText("Number of bootstrap samples for ElasticNet selection.")),
                                                                      numericInput("alphaclustenet","Alpha (ElasticNet mixing)" , 0.5, min =0, max = 1, step = 0.1),
                                                                      conditionalPanel(condition ="input.help",helpText("Alpha=1: Lasso, Alpha=0: Ridge, 0<Alpha<1: ElasticNet")),
                                                                      numericInput("minselectionfreq","Minimum selection frequency" , 0.5, min =0, max = 1, step = 0.05),
                                                                      conditionalPanel(condition ="input.help",helpText("Minimum proportion of bootstrap iterations a variable must be selected to be included in final results.")),
                                                                      checkboxInput("preprocessclustenet","Preprocess variables",TRUE),
                                                                      conditionalPanel(condition ="input.help",helpText("Filter low variance and low frequency variables before clustering."))
                                                     ),
                                                     conditionalPanel(condition ="input.test=='lasso' || input.test=='elasticnet' || input.test=='ridge'",
                                                                      h5("Regularization Parameters"),
                                                                      conditionalPanel(condition ="input.test=='elasticnet'",
                                                                                       numericInput("alphaselection","Alpha (ElasticNet mixing)" , 0.5, min =0, max = 1, step = 0.1),
                                                                                       conditionalPanel(condition ="input.help",helpText("Alpha=1: Lasso, Alpha=0: Ridge, 0<Alpha<1: ElasticNet"))
                                                                      ),
                                                                      checkboxInput("autolambda", "Automatic lambda selection (CV)" , value = TRUE),
                                                                      conditionalPanel(condition ="!input.autolambda",
                                                                                       numericInput("lambdaselection","Lambda (regularization strength)" , 0.01, min =0, max = 10, step = 0.01)
                                                                      ),
                                                                      conditionalPanel(condition ="input.help",helpText("Lambda controls the strength of regularization. Use automatic selection via cross-validation for optimal results."))
                                                     ),
                                                     conditionalPanel(condition ="input.test=='boruta'",
                                                                      h5("Boruta Parameters"),
                                                                      numericInput("boruta_maxRuns","Max iterations (maxRuns)" , 100, min = 20, max = 1000, step = 50),
                                                                      conditionalPanel(condition ="input.help",helpText("Maximum number of random forest runs for Boruta. Increase for more stable results.")),
                                                                      numericInput("boruta_pValue","p-value threshold" , 0.01, min = 0.001, max = 0.1, step = 0.005),
                                                                      conditionalPanel(condition ="input.help",helpText("Significance level for variable importance comparison against shadow attributes."))
                                                     )
                                              )
                                            ),br(),
                                            p(downloadButton('downloaddatastatistics', 'Download statistics'),
                                              downloadButton('downloadddatadiff', 'Download differently expressed variables'),align="center"),
                                            hr(),
                                            conditionalPanel(condition= "input.test=='Kruskal' || input.test=='ANOVA'",
                                                             fluidRow(
                                                               column(6,
                                                                      textOutput("nvarselect2",inline=T), "selected variables",
                                                                      plotOutput("volcanoplot" ,width = 500,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton("downloadvolcanoplot","Download plot"),downloadButton('downloaddatavolcanoplot', 'Download raw data'),align="center")
                                                               ),
                                                               column(6,
                                                                      textOutput("nbdiff",inline=T), "differently expressed",
                                                                      plotOutput("barplottest" ,width = 500,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),   
                                                                      p(downloadButton("downloadbarplottest","Download plot"),downloadButton('downloaddatabarplottest', 'Download raw data'),align="center")
                                                               )
                                                             )
                                            )
                                            ,
                                            conditionalPanel(condition= "input.test=='clustEnet'",
                                                             fluidRow(
                                                               column(12,
                                                                      h4("Clustering + ElasticNet Selection Results"),
                                                                      textOutput("nbclustenetselected",inline=T), " variables selected",br(),br(),
                                                                      h5("Method Parameters"),
                                                                      fluidRow(
                                                                        column(3, strong("Clusters:"), textOutput("clustenetnclusters",inline=T)),
                                                                        column(3, strong("Bootstrap iterations:"), textOutput("clustenetnbootstrap",inline=T)),
                                                                        column(3, strong("Alpha:"), textOutput("clustenetalphaused",inline=T)),
                                                                        column(3, strong("Min. selection freq.:"), textOutput("clustenetminfreq",inline=T))
                                                                      ),br(),
                                                                      dataTableOutput("clustenetresultstable")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton('downloadclustenetresults', 'Download clustering results'),align="center")
                                                               ),
                                                               plotOutput("PcaVarsSel") %>% withSpinner(color = "#0dc5c1",type = 1),
                                                               p(downloadButton("donwloadPCAPlot", "download the plot"), align  =  'center')
                                                             )
                                            )
                                            ,
                                            conditionalPanel(condition= "input.test=='lasso' || input.test=='elasticnet' || input.test=='ridge'",
                                                             fluidRow(
                                                               column(12,
                                                                      h4("Multivariate Selection Results"),
                                                                      textOutput("nbmultivariateselected",inline=T), " variables selected",br(),br(),
                                                                      conditionalPanel(condition ="input.autolambda",
                                                                                       h5("Optimal Hyperparameters (Cross-Validation)"),
                                                                                       fluidRow(
                                                                                         column(4, strong("Lambda (optimal):"), textOutput("optimallambda",inline=T)),
                                                                                         column(4, strong("Lambda (1SE):"), textOutput("lambda1se",inline=T)),
                                                                                         column(4, conditionalPanel(condition ="input.test=='elasticnet'",
                                                                                                                    strong("Alpha:"), textOutput("alphaused",inline=T)))
                                                                                       ),br()
                                                                      ),
                                                                      dataTableOutput("multivariateresultstable")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                      p(downloadButton('downloadmultivariateresults', 'Download multivariate results'),align="center")
                                                               )
                                                             )
                                            ),
                                            conditionalPanel( condition =  "input.test=='boruta'",
                                                              fluidRow(
                                                                
                                                                column(12,
                                                                  h4("Boruta Selection Results"),
                                                                 div(
                                                                   class = "well",
                                                                   style  = 'color :  gray;',
                                                                   fluidRow(
                                                                     column(
                                                                       width = 3 , 
                                                                       tags$b("Confirmed: "), textOutput("boruta_n_confirmed", inline = TRUE), " variables" #, br()
                                                                     ) ,
                                                                     column(
                                                                       width = 3, 
                                                                       tags$b("Tentative: "), textOutput("boruta_n_tentative", inline = TRUE), " variables"#, br()
                                                                     ),
                                                                     column(
                                                                       width = 3 ,
                                                                       tags$b("Rejected: "), textOutput("boruta_n_rejected", inline = TRUE), " variables" #, br()
                                                                     ),
                                                                     column(
                                                                       width = 3 ,
                                                                       tags$b("Shadow max threshold: "), textOutput("boruta_shadow_max", inline = TRUE) #, br(), br()
                                                                     )
                                                                   )
                                                                 )
                                                                )
                                                              ),
                                                              br(), br(),
                                                              fluidRow(
                                                                column(12,
                                                                  plotOutput("boruta_importance_plot", width = "100%", height = 500) %>% withSpinner(color="#0dc5c1", type = 1),
                                                                  p(downloadButton("download_boruta_plot", "Download plot"),
                                                                    downloadButton("download_boruta_data", "Download raw data"), align = "center")
                                                                )
                                                              ),
                                                              fluidRow(
                                                                column(12,
                                                                  h5("Variable Importance Table"),
                                                                  dataTableOutput("boruta_results_table") %>% withSpinner(color="#0dc5c1", type = 1)
                                                                )
                                                              )
                                            ),
                                            conditionalPanel(condition ="input.SFtest==true  ",
                                                             column(6,conditionalPanel(condition ="input.help",
                                                                                       helpText("Barplot presents the Results of shapiro and Fisher test")),
                                                                    plotOutput("plottestSF")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                    p(downloadButton("downloadplottestSF","Download plot"),downloadButton('downloaddatatestSF', 'Download raw data'),align="center"))
                                                             
                                            ),
                                            hr(),
                                            fluidRow(
                                              column(12,
                                                     h4("Principal component analysis (PCA)", align = "center"),
                                                     helpText(" ")
                                              )
                                            ),
                                            
                                            fluidRow(
                                              column(6,
                                                     h5(" 2D View (PC1 vs PC2)"),
                                                     plotlyOutput("pca_plot_2d_stats", height = "450px") %>% 
                                                       withSpinner(color = "#0dc5c1", type = 1),
                                                     br(),
                                                     p(downloadButton("downloadplotPCA2D") %>% 
                                                         withSpinner(color = "#0dc5c1", type = 1), align  = 'center')
                                              ),
                                              column(6,
                                                     h5("3D  view (PC1 vs PC2 vs PC3)"),
                                                     plotlyOutput("pca_plot_3d_stats", height = "450px") %>% 
                                                       withSpinner(color = "#0dc5c1", type = 1),
                                                     br(),
                                                     p(downloadButton("downloadplotPCA3D") %>% 
                                                         withSpinner(color = "#0dc5c1", type = 1), align = 'center')
                                              )
                                            )
                                            # ,
                                            # 
                                            # br(nrow = 2),
                                            # p(downloadButton("download_pca_combined", "Download images"), align = "center")
                                   ),
                                   # tabPanel("PCA Visualization", icon = icon("chart-area"),
                                   #          br(),
                                   #          conditionalPanel(condition = "input.help",
                                   #                           helpText("Visualisations PCA (Analyse en Composantes Principales) des variables sélectionnées."),
                                   #                           helpText("Les graphiques sont colorés selon les vrais labels du jeu d'entraînement."),
                                   #                           helpText("La vue 2D montre les 2 premières composantes principales."),
                                   #                           helpText("La vue 3D montre les 3 premières composantes principales (interactif - vous pouvez faire pivoter le graphique).")
                                   #          ),
                                   #          
                                   #          fluidRow(
                                   #            column(12,
                                   #                   h4("Analyse en Composantes Principales (PCA)", align = "center"),
                                   #                   hr()
                                   #            )
                                   #          ),
                                   #          
                                   #          # Section pour sélectionner les données à visualiser
                                   #          fluidRow(
                                   #            column(4,
                                   #                   radioButtons("pca_data_source", 
                                   #                                "Source des données pour la PCA:",
                                   #                                choices = c(
                                   #                                  "Données transformées" = "transformed",
                                   #                                  "Variables sélectionnées (test statistique)" = "selected",
                                   #                                  "Variables du modèle" = "model"
                                   #                                ),
                                   #                                selected = "transformed")
                                   #            ),
                                   #            column(4,
                                   #                   conditionalPanel(
                                   #                     condition = "input.pca_data_source == 'model'",
                                   #                     helpText("Utilise les variables incluses dans le modèle de prédiction")
                                   #                   ),
                                   #                   conditionalPanel(
                                   #                     condition = "input.pca_data_source == 'selected'",
                                   #                     helpText("Utilise les variables sélectionnées par le test statistique")
                                   #                   ),
                                   #                   conditionalPanel(
                                   #                     condition = "input.pca_data_source == 'transformed'",
                                   #                     helpText("Utilise toutes les variables après transformation")
                                   #                   )
                                   #            ),
                                   #            column(4,
                                   #                   textOutput("pca_n_variables", inline = TRUE),
                                   #                   " variables utilisées pour la PCA"
                                   #            )
                                   #          ),
                                   #          
                                   #          hr(),
                                   #          
                                   #          # Visualisation 2D
                                   #          fluidRow(
                                   #            column(12,
                                   #                   h5("Vue 2D (PC1 vs PC2)", style = "font-weight: bold;"),
                                   #                   plotlyOutput("pca_plot_2d", height = "500px") %>% 
                                   #                     withSpinner(color = "#0dc5c1", type = 1),
                                   #                   p(downloadButton("download_pca_2d", "Télécharger la vue 2D"), align = "center")
                                   #            )
                                   #          ),
                                   #          
                                   #          hr(),
                                   #          
                                   #          # Visualisation 3D
                                   #          fluidRow(
                                   #            column(12,
                                   #                   h5("Vue 3D (PC1 vs PC2 vs PC3) - Interactif", style = "font-weight: bold;"),
                                   #                   helpText("Vous pouvez faire pivoter le graphique 3D avec la souris pour explorer les données sous différents angles."),
                                   #                   plotlyOutput("pca_plot_3d", height = "600px") %>% 
                                   #                     withSpinner(color = "#0dc5c1", type = 1),
                                   #                   p(downloadButton("download_pca_3d", "Télécharger la vue 3D"), align = "center")
                                   #            )
                                   #          ),
                                   #          
                                   #          hr(),
                                   #          
                                   #          # Tableau de variance expliquée
                                   #          fluidRow(
                                   #            column(12,
                                   #                   h5("Variance expliquée par les composantes principales", style = "font-weight: bold;"),
                                   #                   dataTableOutput("pca_variance_table") %>% 
                                   #                     withSpinner(color = "#0dc5c1", type = 1),
                                   #                   p(downloadButton("download_pca_variance", "Télécharger le tableau"), align = "center")
                                   #            )
                                   #          )
                                   # ),
                                   tabPanel("Model", icon = icon("cogs"),
                                            fluidRow(
                                              column(4,
                                                     radioButtons("model", "Type of model to adjust",
                                                                  c("No model" = "nomodel",
                                                                    "Random Forest"="randomforest",
                                                                    "Support Vector Machine" = "svm",
                                                                    "Penalized Logistic Regression (ElasticNet)"="elasticnet",
                                                                    "XGBoost"="xgboost",
                                                                    #"LightGBM"="lightgbm",
                                                                    "Naive Bayes"="naivebayes",
                                                                    "K-Nearest Neighbors (KNN)"="knn"))
                                                     #,
                                                     # conditionalPanel(condition ="input.help",
                                                     #                  helpText("Random Forest: ensemble method with automatic mtry tuning."),
                                                     #                  helpText("SVM: automatic hyperparameter tuning (C, gamma)."),
                                                     #                  helpText("ElasticNet: penalized logistic regression with L1/L2 regularization."),
                                                     #                  helpText("XGBoost: gradient boosting with automatic hyperparameter tuning.")
                                                     #                  )
                                              ),
                                              column(4,
                                                     #numericInput("thresholdmodel","Threshold model" ,0, min = -1, max = 1, step = 0.05),
                                                     conditionalPanel(condition ="input.help", helpText("The threshold of the score is used for classification")),
                                                     fluidRow(
                                                       column(12,checkboxInput("fs","features selection by cross validation /!\\ ",F))
                                                     ),
                                                     helpText("/!\\ process can be long")
                                              ),
                                              column(4,
                                                     conditionalPanel(condition ="input.model=='elasticnet'",
                                                                      h5("ElasticNet Hyperparameters"),
                                                                      radioButtons("tuning_method_en", "Tuning method:",
                                                                                   c(
                                                                                     #"Manual parameters" = "manual",
                                                                                     "Cross-validation (cv.glmnet)" = "traditional",
                                                                                     "GridSearchCV (superml)" = "gridsearch"),
                                                                                   selected = "traditional"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Manual: set alpha/lambda manually"),
                                                                                       helpText("cv.glmnet: optimizes lambda for fixed alpha"),
                                                                                       helpText("GridSearchCV: jointly optimizes alpha and lambda")),
                                                                      conditionalPanel(condition ="input.tuning_method_en=='manual' || input.tuning_method_en=='traditional'",
                                                                                       numericInput("alphamodel","Alpha (mixing parameter)" , 0.5, min =0, max = 1, step = 0.1),
                                                                                       conditionalPanel(condition ="input.help",helpText("Alpha=1: Lasso (L1), Alpha=0: Ridge (L2), 0<Alpha<1: ElasticNet"))
                                                                      ),
                                                                      conditionalPanel(condition ="input.tuning_method_en=='manual'",
                                                                                       numericInput("lambdamodel","Lambda" , 0.01, min =0, max = 10, step = 0.01)
                                                                      )
                                                     ),
                                                     conditionalPanel(condition ="input.model=='randomforest'",
                                                                      h5("Random Forest Hyperparameters"),
                                                                      radioButtons("tuning_method_rf", "Tuning method:",
                                                                                   c("Manual parameters" = "manual",
                                                                                     "tuneRF" = "traditional",
                                                                                     "GridSearchCV (superml)" = "gridsearch"),
                                                                                   selected = "traditional"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Manual: set all hyperparameters manually"),
                                                                                       helpText("tuneRF: optimizes mtry, nodesize and ntree via OOB error"),
                                                                                       helpText("GridSearchCV: full grid search over ntree, mtry, nodesize, maxnodes, sampsize")),
                                                                      # ── Manual mode ──
                                                                      conditionalPanel(
                                                                        condition = "input.tuning_method_rf == 'manual'",
                                                                        numericInput("ntreerf", "Number of trees (ntree)", 1000, min = 100, max = 5000, step = 100),
                                                                        numericInput("mtryrf", "mtry (variables per split)", 5, min = 1, max = 500, step = 1),
                                                                        numericInput("nodesizerf", "Minimum node size (nodesize)", 1, min = 1, max = 50, step = 1),
                                                                        numericInput("maxnodesrf", "Max terminal nodes (maxnodes, 0=unlimited)", 0, min = 0, max = 1000, step = 10),
                                                                        sliderInput("sampsizerf", "Bootstrap sample fraction (sampsize)", min = 0.3, max = 1.0, value = 1.0, step = 0.05),
                                                                        checkboxInput("replacerf", "Sample with replacement", value = TRUE),
                                                                        conditionalPanel(condition ="input.help",
                                                                                         helpText("nodesize: Minimum number of observations in a terminal node"),
                                                                                         helpText("maxnodes: Maximum number of terminal nodes (0 = unlimited)"),
                                                                                         helpText("sampsize: Fraction of training data sampled per tree"),
                                                                                         helpText("replace: Whether to sample with replacement (bootstrap)"))
                                                                      ),
                                                                      # ── Traditional tuneRF mode ──
                                                                      conditionalPanel(
                                                                        condition = "input.tuning_method_rf == 'traditional'",
                                                                        checkboxGroupInput("ntree_range_rf", "ntree values to test:",
                                                                                           choices  = c("100" = 100, "300" = 300, "500" = 500, "1000" = 1000, "2000" = 2000),
                                                                                           selected = c(100, 500, 1000), inline = TRUE
                                                                        ),
                                                                        checkboxGroupInput("nodesize_range_rf", "nodesize values to test:",
                                                                                           choices  = c("1" = 1, "3" = 3, "5" = 5, "10" = 10, "20" = 20),
                                                                                           selected = c(1, 5, 10), inline = TRUE
                                                                        ),
                                                                        sliderInput("sampsize_trad_rf", "Bootstrap sample fraction", min = 0.3, max = 1.0, value = 1.0, step = 0.05),
                                                                        checkboxInput("replace_trad_rf", "Sample with replacement", value = TRUE),
                                                                        conditionalPanel(condition ="input.help",
                                                                                         helpText("tuneRF optimizes mtry via OOB error for each ntree/nodesize combination"),
                                                                                         helpText("The best (ntree, mtry, nodesize) combination is selected"))
                                                                      ),
                                                                      # ── GridSearchCV mode ──
                                                                      conditionalPanel(
                                                                        condition = "input.tuning_method_rf == 'gridsearch'",
                                                                        checkboxGroupInput("ntree_grid_values", "ntree grid values:",
                                                                                           choices  = c("100" = 100, "300" = 300, "500" = 500, "1000" = 1000, "2000" = 2000),
                                                                                           selected = c(100, 500, 1000), inline = TRUE),
                                                                        checkboxGroupInput("nodesize_grid_values", "nodesize grid values:",
                                                                                           choices  = c("1" = 1, "3" = 3, "5" = 5, "10" = 10, "20" = 20),
                                                                                           selected = c(1, 5, 10) , inline = TRUE),
                                                                        checkboxGroupInput("maxnodes_grid_values", "maxnodes grid values (0=unlimited):",
                                                                                           choices  = c("Unlimited" = 0, "10" = 10, "20" = 20, "50" = 50, "100" = 100),
                                                                                           selected = c(0, 20, 50), inline = TRUE),
                                                                        checkboxGroupInput("sampsize_grid_values", "sampsize fraction grid:",
                                                                                           choices  = c("0.5" = 0.5, "0.632" = 0.632, "0.8" = 0.8, "1.0" = 1.0),
                                                                                           selected = c(0.632, 1.0), inline = TRUE),
                                                                        conditionalPanel(condition ="input.help",
                                                                                         helpText("mtry grid: automatically includes sqrt(p), log2(p), p/3, p/2"),
                                                                                         helpText("sampsize: fraction of n used per tree (0.632 is classic bootstrap proportion)"))
                                                                      )
                                                     ),
                                                     conditionalPanel(condition ="input.model=='svm'",
                                                                      h5("SVM Hyperparameters"),
                                                                      checkboxInput("autotunesvm", "Automatic hyperparameter tuning (tune.svm)" , value = FALSE),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Automatically find optimal cost and gamma")
                                                                                       ),
                                                                      conditionalPanel(condition ="!input.autotunesvm",
                                                                                       numericInput("costsvm","Cost (C)" , 1, min =0.001, max = 100, step = 0.1),
                                                                                       numericInput("gammasvm","Gamma" , 0.1, min =0.00001, max = 10, step = 0.01),
                                                                                       selectInput("kernelsvm", "Kernel type:",
                                                                                                  c("Radial" = "radial",
                                                                                                    "Linear" = "linear",
                                                                                                    "Polynomial" = "polynomial",
                                                                                                    "Sigmoid" = "sigmoid"),
                                                                                                  selected = "radial"),
                                                                                       conditionalPanel(condition ="input.help",
                                                                                                        helpText("Manually set SVM hyperparameters")
                                                                                                        )
                                                                      )
                                                                      # ,
                                                                      # conditionalPanel(condition ="input.help"
                                                                      #                  
                                                                      # )
                                                     ),
                                                     conditionalPanel(condition ="input.model=='xgboost'",
                                                                      h5("XGBoost Hyperparameters"),
                                                                      radioButtons("tuning_method_xgb", "Tuning method:",
                                                                                   c("Manual parameters" = "manual",
                                                                                     "Cross-validation (xgb.cv)" = "traditional",
                                                                                     "GridSearchCV (superml)" = "gridsearch"),
                                                                                   selected = "traditional"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Manual: set all parameters manually"),
                                                                                       helpText("xgb.cv: basic cross-validation"),
                                                                                       helpText("GridSearchCV: comprehensive multi-parameter tuning")),
                                                                      conditionalPanel(condition ="input.tuning_method_xgb=='manual'",
                                                                                       bslib::tooltip(
                                                                                       numericInput("nroundsxgb","Number of rounds" , 100, min =10, max = 1000, step = 10),
                                                                                       placement =  "right",
                                                                                       htmltools::span("Also known as num_boost_round; defines the number of boosting iterations.")
                                                                                       ),
                                                                                       numericInput("maxdepthxgb","Max depth" , 6, min =1, max = 20, step = 1),
                                                                                       fluidRow(
                                                                                         column(
                                                                                           6,  numericInput("lambdaxgb","Lambda (L2 regularization)" , 
                                                                                                            1, min =0, max = Inf, step = 0.1)
                                                                                         ), 
                                                                                         column(
                                                                                           6,  numericInput("alphaxgb","Alpha (L1 regularization)", 
                                                                                                            0, min = 0, max = 1, step = 0.1)
                                                                                         )
                                                                                       )
                                                                                       ,
                                                                                       fluidRow(
                                                                                         # column(12 , 
                                                                                         column(6 , 
                                                                                                numericInput("etaxgb","Learning rate (eta)" , 0.3, min =0.01, max = 1, step = 0.01)
                                                                                         ),
                                                                                         column(6, 
                                                                                                numericInput("colsamplexgb","Colsample by tree" , 1, min =0.1, max = 1, step = 0.1)
                                                                                         )
                                                                                         # )
                                                                                       ),
                                                                                       fluidRow(
                                                                                         # column(12, 
                                                                                         column(6,
                                                                                                numericInput("subsamplexgb","Subsample" , 1, min =0.1, max = 1, step = 0.1)
                                                                                         ),
                                                                                         column(6,
                                                                                                numericInput('gamme_xgb', "gamma", value = 0.1, min = 0, max = 1, step = 0.1)
                                                                                         )
                                                                                         # )
                                                                                       )
                                                                                       ,
                                                                                       # numericInput("etaxgb","Learning rate (eta)" , 0.3, min =0.01, max = 1, step = 0.01),
                                                                                       conditionalPanel(condition ="input.help",helpText("Manually set XGBoost hyperparameters"))
                                                                      )
                                                                      # ,
                                                                      # conditionalPanel("input.help" 
                                                                      #  
                                                                      # )
                                                     ),
                                                     conditionalPanel(condition ="input.model=='lightgbm'",
                                                                      h5("LightGBM Hyperparameters"),
                                                                      checkboxInput("autotunelgb", "Automatic hyperparameter tuning (CV)" , value = TRUE),
                                                                      conditionalPanel(condition ="input.help",helpText("Automatically find optimal parameters via cross-validation")),
                                                                      conditionalPanel(condition ="!input.autotunelgb",
                                                                                       numericInput("nroundslgb","Number of rounds" , 100, min =10, max = 1000, step = 10),
                                                                                       numericInput("numleaves","Num leaves" , 31, min =10, max = 200, step = 5),
                                                                                       numericInput("learningratelgb","Learning rate" , 0.05, min =0.001, max = 0.5, step = 0.01),
                                                                                       conditionalPanel(condition ="input.help",helpText("Manually set LightGBM hyperparameters"))
                                                                      )
                                                     ),
                                                     
                                                     conditionalPanel(condition ="input.model=='naivebayes'",
                                                                      h5("Naive Bayes Hyperparameters"),
                                                                      radioButtons("tuning_method_nb", "Tuning method:",
                                                                                   c("No tuning (laplace=0)" = "manual",
                                                                                     "GridSearchCV (superml)" = "gridsearch"),
                                                                                   selected = "manual"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("No tuning: uses default laplace=0"),
                                                                                       helpText("GridSearchCV: optimizes laplace smoothing parameter"))
                                                                      #,  conditionalPanel("input.help" )
                                                     ),

                                                     conditionalPanel(condition ="input.model=='knn'",
                                                                      h5("KNN Hyperparameters"),
                                                                      radioButtons("tuning_method_knn", "Tuning method:",
                                                                                   c("Manual parameters" = "manual",
                                                                                     "Cross-validation" = "traditional"
                                                                                     # ,
                                                                                     # "GridSearchCV (superml)" = "gridsearch"
                                                                                     ),
                                                                                   selected = "manual"),
                                                                      conditionalPanel(condition ="input.help",
                                                                                       helpText("Manual: set k manually"),
                                                                                       helpText("Traditional CV: basic cross-validation for k"),
                                                                                       helpText("GridSearchCV: systematic grid search")),
                                                                      conditionalPanel(condition ="input.tuning_method_knn=='manual'",
                                                                                       numericInput("kneighbors","Number of neighbors (k)" , 5, min =1, max = 50, step = 2),
                                                                                       conditionalPanel(condition ="input.help",
                                                                                                        helpText("Manually set k parameter"))
                                                                      )
                                                                      ) 
                                                                  ) 
                                                
                                                    # )$$
                                                     ),
                                                      # affcihage des help en fonction du model
                                                      fluidRow(
                                                        conditionalPanel("input.help", 
                                                                          conditionalPanel(condition ="input.model=='knn'",
                                                                                        div(
                                                                                            class =  "myDiv",
                                                                                            h4("KNN Hyperparameter Explanation"),
                                                                                            p(strong("Number of Neighbors (k):"), "Determines how many nearest neighbors are considered when classifying a new data point. 
                                                                                           A smaller k can capture local patterns but may be sensitive to noise, while a larger k provides smoother decision boundaries but may overlook local nuances."
                                                                                            )
                                                                                          )
                                                                                          
                                                                         ),
                                                                         conditionalPanel(condition = "input.model== 'naivebayes'",
                                                                                          div(class = "myDiv",
                                                                                              h4("Naive Bayes Hyperparameter Explanation"),
                                                                                              p(strong("Laplace Smoothing:"), "A technique used to handle zero probabilities in categorical data. 
                                                                                               It adds a small constant (usually 1) to each count to ensure that no probability is exactly zero, 
                                                                                               which can be particularly useful when dealing with unseen features in the training data.") 
                                                                                          )
                                                                                        ),
                                                                         conditionalPanel(condition = "input.model=='svm'", 
                                                                                          tags$head(
                                                                                            tags$style(HTML("
                                                                                                      .myDiv {
                                                                                                        border: 5px outset green;
                                                                                                        background-color: lightblue;    
                                                                                                        text-align: left;
                                                                                                      }
                                                                                                    "))
                                                                                          ),
                                                                                          helpText("Lambda controls the strength of regularization. Use automatic 
                                                                                                selection via cross-validation for optimal results.")
                                                                                          ,
                                                                                          div(
                                                                                            class = "myDiv",
                                                                                            p("In SVM with RBF (Radial Basis Function) kernel, the hyperparameters C and gamma together control the complexity and generalisation capacity of the model.
                                                                                                They are linked in the sense that modifying one influences the effect of the other on the decision boundary."),
                                                                                            p("A high C value with a low gamma can lead to overfitting, while a low C with a high gamma may result in underfitting. 
                                                                                                 Therefore, it's crucial to tune both parameters together to find the optimal balance 
                                                                                              for the specific dataset."),
                                                                                            HTML("<ul>
                                                                                                    <li>High C + high gamma → very flexible model, high risk of overfitting. </li>
                                                                                                    <li>Low C + low gamma → very smooth model, risk of underfitting. </li>
                                                                                                    <li> High C + low gamma → model that tries to classify well but with a generally smooth boundary. </li>
                                                                                                    <li> Low C + high gamma → model that accepts errors but with complex local boundaries.  </li>
                                                                                                 </ul>"
                                                                                              
                                                                                            )
                                                                                            # ,
                                                                                            #   p("High C + high gamma → very flexible model, high risk of overfitting."),
                                                                                            #   p("Low C + low gamma → very smooth model, risk of underfitting."),
                                                                                            #   p("High C + low gamma → model that tries to classify well but with a generally smooth boundary."),
                                                                                            #   p("Low C + high gamma → model that accepts errors but with complex local boundaries. ")

                                                                                          )
                                                                                          ),
                                                                         conditionalPanel(condition ="input.model=='randomforest'",
                                                                                          div(
                                                                                            class =  "myDiv",
                                                                                            h4("Random Forest Hyperparameters Explanation"),
                                                                                            p(strong("ntree:"), "Number of trees in the forest; more trees can improve performance but increase computation time."),
                                                                                            p(strong("mtry:"), "Number of variables randomly sampled at each split; controls tree diversity and model robustness.")
                                                                                          )
                                                                                          ),
                                                                         conditionalPanel(condition ="input.model=='xgboost'",
                                                                                          div(
                                                                                            class =  "myDiv",
                                                                                            h4("XGBoost Hyperparameters Explanation"),
                                                                                            p(strong("nrounds:"), "Also known as num_boost_round; defines the number of boosting iterations."),
                                                                                            p(strong("max_depth:"), "Maximum depth of a tree; controls model complexity and overfitting."),
                                                                                            p(strong("eta (learning rate):"), "Step size shrinkage used to prevent overfitting; smaller values require more rounds.")
                                                                                          )
                                                                                          ),
                                                                         conditionalPanel(condition = "input.model=='elasticnet'", 
                                                                                          conditionalPanel(condition ="input.help",
                                                                                                           div(class =  "myDiv",
                                                                                                               h4("ElasticNet Hyperparameters Explanation"),
                                                                                                               p(strong("Alpha:"), "Controls the mix between L1 (Lasso) and L2 (Ridge) regularization.
                                                                                                                        Alpha=1 corresponds to Lasso, Alpha=0 to Ridge, and values in between represent a combination of both."),
                                                                                                               p(strong("Lambda:"), "Lambda controls the strength of regularization.
                                                                                                                        Use automatic selection via cross-validation for optimal results.")
                                                                                                               
                                                                                                           )
                                                                                          )  
                                                                                        )
                                                      ) # fin du grand conditionPanel
                                                      ),
                                                     conditionalPanel(condition ="output.fileUploadedval & input.model!='nomodel'  ",
                                                                      checkboxInput("adjustval","Adjust model on validation data",F)
                                                     )
                                                     ,
                                                     hr(),
                                                     conditionalPanel(condition ="input.model!='nomodel'  ",
                                                                      fluidRow(
                                                                        column(4,
                                                                               textOutput('nbselectmodel',inline=T),'selected variables',
                                                                               h3("Model Learning")
                                                                        ),
                                                                        column(4,br(),downloadButton('downloaddatalearning', 'Download learning data'))
                                                                        # ,
                                                                        # column(4,radioButtons("plotscoremodel", "",c( "boxplot"="boxplot","points" = "points"))
                                                                        #        )
                                                                        
                                                                      ),
                                                                      conditionalPanel(condition ="input.model=='elasticnet' || input.model=='svm' || input.model=='randomforest' || input.model=='xgboost'",
                                                                                       fluidRow(
                                                                                         column(12,
                                                                                                h4("Optimal Hyperparameters (Training)"),
                                                                                                conditionalPanel(condition ="input.model=='elasticnet'",
                                                                                                                 div(
                                                                                                                   class =  "well",
                                                                                                                   style = 'color :  blue;',
                                                                                                                   fluidRow(
                                                                                                                     column(3, strong("Alpha:"), textOutput("modelalpha",inline=T)),
                                                                                                                     column(3, strong("Lambda:"), textOutput("modellambda",inline=T)),
                                                                                                                     column(3, strong("Lambda (1SE):"), textOutput("modellambda1se",inline=T)),
                                                                                                                     column(3, strong("Non-zero coef:"), textOutput("modelnonzerocoef",inline=T))
                                                                                                                   )
                                                                                                                 )
                                                                                                                 
                                                                                                ),
                                                                                                conditionalPanel(condition ="input.model=='svm'",
                                                                                                                 div(
                                                                                                                   class =  "well",
                                                                                                                   style = 'color :  blue;',
                                                                                                                   fluidRow(
                                                                                                                     column(4, strong("Cost (C):"), textOutput("svmcost",inline=T)),
                                                                                                                     column(4, strong("Gamma:"), textOutput("svmgamma",inline=T)),
                                                                                                                     column(4, strong("Kernel:"), textOutput("svmkernel",inline=T)
                                                                                                                            #"Radial"
                                                                                                                            )
                                                                                                                   )
                                                                                                                 )
                                                                                                ),
                                                                                                conditionalPanel(condition ="input.model=='randomforest'",
                                                                                                                 div(
                                                                                                                   class =  "well",
                                                                                                                   style = 'color :  blue;',
                                                                                                                   fluidRow(
                                                                                                                     column(4, strong("Optimal mtry:"), textOutput("rfmtry",inline=T)),
                                                                                                                     column(4, strong("Number of trees:"), textOutput("rfntree",inline=T)),
                                                                                                                     column(4, strong("Tuning method:"), "tuneRF")
                                                                                                                   )
                                                                                                                 )
                                                                                                ),
                                                                                                conditionalPanel(condition ="input.model=='xgboost'",
                                                                                                                 div(
                                                                                                                   class =  "well",
                                                                                                                   style = 'color :  blue;',
                                                                                                                   fluidRow(
                                                                                                                     column(2, strong("Optimal nrounds:"), textOutput("xgbnrounds",inline=T)),
                                                                                                                     column(2, strong("Max depth:"), textOutput("xgbmaxdepth",inline=T)),
                                                                                                                     column(2, strong("Learning rate (eta):"), textOutput("xgbeta",inline=T)),
                                                                                                                     column(2, strong("Min child weight:"), textOutput("xgbminchild",inline=T)),
                                                                                                                     # column(2, strong("Colsample by tree:"), textOutput("xgbcolsample",inline=T)),
                                                                                                                     column(2, strong("Subsample:"), textOutput("xgbsubsample",inline=T)),
                                                                                                                     column(2, strong("Gamma:"), textOutput("xgbgamma",inline=T))
                                                                                                                   )
                                                                                                                 )
                                                                                                ),
                                                                                                conditionalPanel( condition = "input.model=='knn'",
                                                                                                             fluidRow(
                                                                                                               column(3,
                                                                                                                     strong("Optimal K in KNN:"), textOutput("knnk",inline=T)
                                                                                                                     )
                                                                                                             )
                                                                                                ),
                                                                                                hr()
                                                                                         )
                                                                                       )
                                                                      ),
                                                                      fluidRow(
                                                                        column(6,
                                                                               plotOutput("plotmodeldecouvroc")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                               p(downloadButton("downloadplotdecouvroc","Download plot"),
                                                                                 downloadButton('downloaddatadecouvroc', 'Download raw data'),align="center")
                                                                        ),
                                                                        column(6,
                                                                               plotOutput("tabmodeldecouv", height = "400px")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                               
                                                                               p(downloadButton("downloadtabmodeldecouv","Download plot"),
                                                                                    #downloadButton('downloaddatatabmodeldecouv', 'Download raw data'),
                                                                                 align="center")
                                                                               
                                                                               # plotOutput("plotmodeldecouvbp")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                               # p(downloadButton("downloadplotmodeldecouvbp","Download plot"),
                                                                               #   downloadButton('downloaddatamodeldecouvbp', 'Download raw data'),align="center")
                                                                        ),
                                                                        #column(),
                                                                        fluidRow(
                                                                          column(
                                                                            width = 5
                                                                          ),
                                                                          column(
                                                                            width = 3,
                                                                            conditionalPanel(condition="input.plotscoremodel=='points'",
                                                                                             checkboxInput("shownames1","show indivuals names",value=FALSE)
                                                                            ),
                                                                            br(),
                                                                            
                                                                            h4("Average Metrics"),
                                                                            tableOutput("average_metrics_decouv"),
                                                                            downloadButton("download_average_metrics_decouv")
                                                                            # ,
                                                                            # 
                                                                            #   "Sensibility = ",textOutput("sensibilitydecouv",inline=T), 
                                                                            #   br(),
                                                                            #   "Specificity = ",textOutput("specificitydecouv",inline=T) #,
                                                                            #   conditionalPanel(
                                                                            #             condition = "output.fileUploaded",
                                                                            #             h4("Detailed Metrics by Class"),
                                                                            #             tableOutput("detailed_metrics_decouv"),
                                                                            #             h4("Average Metrics"),
                                                                            #             tableOutput("average_metrics_decouv")
                                                                            # )
                                                                            # br(),hr(),br(),
                                                                            # tableOutput("youndendecouv")
                                                                            
                                                                            
                                                                          ),
                                                                          column(
                                                                            4,
                                                                            br(),
                                                                            h4("Detailed Metrics by Class"),
                                                                            tableOutput("detailed_metrics_decouv"),
                                                                            br(nrow = 2),
                                                                            downloadButton("download_detailed_metrics_decouv")
                                                                          )
                                                                        )
                                                                        
                                                                      ),
                                                                      hr(),
                                                                      conditionalPanel(condition ="input.adjustval==true  ",
                                                                                       fluidRow(div(
                                                                                         column(6,h3("model validation"))
                                                                                         # , 
                                                                                         # column(6,br(),downloadButton('downloaddatavalidation', 'Download validation data'))
                                                                                         )
                                                                                       ), 
                                                                                       fluidRow(
                                                                                         column(6,plotOutput("plotmodelvalroc")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                                                p(downloadButton("downloadplotvalroc","Download plot"),
                                                                                                  downloadButton('downloaddatavalroc', 'Download raw data'),align="center")
                                                                                         ),
                                                                                         column(6,
                                                                                                plotOutput("tabmodelval", height = "400px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                                                p(downloadButton("downloadtabmodelval","Download plot"),
                                                                                                  #downloadButton('downloaddatatabmodeldecouv', 'Download raw data'),
                                                                                                  align="center")
                                                                                                
                                                                                                ),
                                                                                         # column(),
                                                                                         fluidRow(
                                                                                           column(width = 5),
                                                                                           column(3,
                                                                                                  
                                                                                                  #conditionalPanel(condition="input.plotscoremodel=='points'",checkboxInput("shownames2","show indivuals names",value=FALSE)),
                                                                                                  h4("Average Metrics"),
                                                                                                  tableOutput("average_metrics_val"),
                                                                                                  br(n_row =2),
                                                                                                  downloadButton("download_average_metrics_val")
                                                                                                  # ,
                                                                                                  # "Sensibility = ",textOutput("sensibilityval",inline=T), 
                                                                                                  # br(),
                                                                                                  # "Specificity = ",textOutput("specificityval",inline=T)
                                                                                                  
                                                                                                  # 
                                                                                                  # br(),hr(),br(),
                                                                                                  # tableOutput("youndenval")
                                                                                                  
                                                                                                  
                                                                                                  ),
                                                                                           column(
                                                                                             width =  4,
                                                                                             h4("Detailed Metrics by Class"),
                                                                                             tableOutput("detailed_metrics_val"),
                                                                                             br(nrow = 2),
                                                                                             downloadButton("download_detailed_metrics_val")
                                                                                           )
                                                                                         )
                                                                                       )
                                                                      )
                                                     )
                                                     ),
                                              tabPanel("Details of the model", icon =  icon("file-alt"),
                                                       h3("Summary of the model"),
                                                       verbatimTextOutput("summarymodel"),
                                                       plotOutput("plotimportance"),
                                                       p(downloadButton("downloadplotimportance","Download plot"),
                                                         downloadButton('downloaddataplotimportance', 'Download raw data'),align="center")
                                              ),
                                   # Dans tabPanel "Advanced Visualizations" ou nouveau tabPanel "Learning Curve"
                                   # tabPanel("Learning Curve",
                                   #          sidebarLayout(
                                   #            sidebarPanel(
                                   #              h4("Learning Curve Parameters"),
                                   #              
                                   #              selectInput("learning_curve_metric",
                                   #                          "Metric",
                                   #                          choices = c("Accuracy" = "accuracy",
                                   #                                      "AUC" = "auc",
                                   #                                      "F1 Score" = "f1",
                                   #                                      "Precision" = "precision",
                                   #                                      "Recall" = "recall"),
                                   #                          selected = "accuracy"),
                                   #              
                                   #              sliderInput("learning_curve_train_sizes",
                                   #                          "Training sizes (%)",
                                   #                          min = 10,
                                   #                          max = 90,
                                   #                          value = 10,
                                   #                            #c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                                   #                          step = 10),
                                   #              
                                   #              numericInput("learning_curve_cv_folds",
                                   #                           "CV folds (if no validation)",
                                   #                           value = 5,
                                   #                           min = 2,
                                   #                           max = 10),
                                   #              
                                   #              checkboxInput("use_validation",
                                   #                            "Use validation set as test",
                                   #                            value = TRUE),
                                   #              
                                   #              hr(),
                                   #              
                                   #              actionButton("run_learning_curve",
                                   #                           "Generate Learning Curve",
                                   #                           class = "btn-primary"),
                                   #              
                                   #              helpText("Learning curve shows model performance vs training size.",
                                   #                       "Use to diagnose underfitting (high bias) or overfitting (high variance).")
                                   #            ),
                                   #            
                                   #            mainPanel(
                                   #              h4("Learning Curve"),
                                   #              plotOutput("learning_curve_plot") %>% withSpinner(color="#0dc5c1",type = 1),
                                   #              
                                   #              hr(),
                                   #              
                                   #              fluidRow(
                                   #                column(6,
                                   #                       downloadButton("download_learning_curve", "Download Plot", class = "btn-info")
                                   #                ),
                                   #                column(6,
                                   #                       downloadButton("download_learning_curve_data", "Download Data", class = "btn-info")
                                   #                )
                                   #              )
                                   #            )
                                   #          )
                                   #          ),
                                   tabPanel("Learning Curve", icon = icon("chart-line"),
                                            br(),
                                            h3("Learning Curve — Multi-class"),
                                            helpText("Shows how model performance evolves as training set size increases.",
                                                     "A large gap between training and CV curves indicates overfitting.",
                                                     "Convergence of the two curves suggests underfitting or insufficient data."),
                                            hr(),
                                            fluidRow(
                                              column(4,
                                                     sliderInput("lc_size_min", "Minimum training size (%):",
                                                                 min = 10, max = 50, value = 10, step = 5)
                                                     
                                                     ) ,
                                              column(
                                                4 , 
                                                numericInput("lc_n_steps", "Number of size steps:",
                                                             value = 8, min = 3, max = 20, step = 1)
                                              ),
                                              #br(),
                                              column(
                                                4,
                                                actionButton("run_learning_curve",
                                                             h4("Compute Learning Curve"),
                                                             style = "background-color: #2980B9; color: white; border-color: #2980B9;" #, width = 150
                                                             )
                                              ),
                                              
                                              br(),
                                              helpText("/!\\ Each step trains the model from scratch.",
                                                       "Computation time scales with number of steps and model complexity.")
                                              
                                            ), 
                                            fluidRow(
                                              # column(3,
                                              #        sliderInput("lc_size_min", "Minimum training size (%):",
                                              #                    min = 10, max = 50, value = 10, step = 5),
                                              #        numericInput("lc_n_steps", "Number of size steps:",
                                              #                     value = 8, min = 3, max = 20, step = 1),
                                              #        br(),
                                              #        actionButton("run_learning_curve",
                                              #                     h4("Compute Learning Curve"),
                                              #                     style = "background-color: #2980B9; color: white; border-color: #2980B9;",
                                              #                     width = 150),
                                              #        br(),
                                              #        helpText("/!\\ Each step trains the model from scratch.",
                                              #                 "Computation time scales with number of steps and model complexity.")
                                              # ),
                                              column(12,
                                                     fluidRow(
                                                       column(6,
                                                              plotOutput("plot_lc_auc", height = "500px") %>% withSpinner(color = "#0dc5c1", type = 1),
                                                              p(downloadButton("download_lc_auc", "Download AUC plot"), align = "center")
                                                       ),
                                                       column(6,
                                                              plotOutput("plot_lc_accuracy", height = "500px") %>% withSpinner(color = "#0dc5c1", type = 1),
                                                              p(downloadButton("download_lc_accuracy", "Download Accuracy plot"), align = "center")
                                                       )
                                                     )
                                              )
                                            ),
                                            hr(),
                                            h4("Raw Data"),
                                            dataTableOutput("table_lc") %>% withSpinner(color = "#0dc5c1", type = 1),
                                            p(downloadButton("download_lc_data", "Download CSV"), align = "center")
                                   ),
                                              tabPanel("Model Comparison", icon = icon("balance-scale"),
                                                       br(),
                                                       h3("Compare All Models"),
                                                       helpText("Train all available models on the same data and compare their performance."),
                                                       fluidRow(
                                                         column(6,
                                                                checkboxGroupInput("models_to_compare", "Models to compare:",
                                                                                   c("Random Forest" = "randomforest",
                                                                                     "SVM" = "svm",
                                                                                     "ElasticNet" = "elasticnet",
                                                                                     "XGBoost" = "xgboost",
                                                                                     "Naive Bayes" = "naivebayes",
                                                                                     "KNN" = "knn"),
                                                                                   selected = c("randomforest", "svm", "elasticnet"))
                                                         ),
                                                         column(6,
                                                                actionButton("run_comparison", h4("Run Comparison"),
                                                                             style = "background-color: #63BFBF; color: white; border-color: #63BFBF;",
                                                                             width = 200),
                                                                br(), br(),
                                                                helpText("This will train all selected models with automatic tuning.")
                                                         )
                                                       ),
                                                       hr(),
                                                       h4("Performance Summary"),
                                                       dataTableOutput("comparison_metrics_table") %>% withSpinner(color="#0dc5c1",type = 1),
                                                       p(downloadButton("download_comparison_metrics", "Download metrics"), align = "center"),
                                                       hr(),
                                                       fluidRow(
                                                         shiny::plotOutput("plotcompared_model",height = "450px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                         p(downloadButton("downloadplotcompared_model", "downlaod plot"), align= 'center')
                                                       ), 
                                                       fluidRow(
                                                         column(6,
                                                                h4("Radar Plot - Training"),
                                                                plotOutput("radar_plot_train", height = "450px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("download_radar_train", "Download plot"), align = "center")
                                                         ),
                                                         column(6,
                                                                h4("Radar Plot - Validation"),
                                                                plotOutput("radar_plot_val", height = "450px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("download_radar_val", "Download plot"), align = "center")
                                                         )
                                                       ),
                                                       hr(),
                                                       h4("DeLong Test (AUC Comparison)"),
                                                       helpText("Statistical test comparing AUC between pairs of models (p-value < 0.05 = significant difference)."),
                                                       dataTableOutput("delong_test_table") %>% withSpinner(color="#0dc5c1",type = 1),
                                                       p(downloadButton("download_delong_table", "Download DeLong results"), align = "center"),
                                                       hr(),
                                                       h4("DeLong Test — FDR Corrected (Benjamini-Hochberg)"),
                                                       helpText("P-values adjusted for multiple pairwise comparisons using the Benjamini-Hochberg method."),
                                                       dataTableOutput("delong_corrected_table") %>% withSpinner(color="#0dc5c1",type = 1),
                                                       p(downloadButton("download_delong_corrected", "Download FDR-corrected results"), align = "center"),
                                                       hr(),
                                                       h4("DeLong Test — Per-Class Detail (One-vs-Rest)"),
                                                       helpText("DeLong p-values for each class separately (OvR decomposition). Shows AUC per model and per class."),
                                                       dataTableOutput("delong_perclass_table") %>% withSpinner(color="#0dc5c1",type = 1),
                                                       p(downloadButton("download_delong_perclass", "Download per-class results"), align = "center")
                                              ),
                                              tabPanel("Interpretability (XAI)", icon = icon("lightbulb"),
                                                       br(),
                                                       h3("Model Interpretability"),
                                                       conditionalPanel(condition = "input.model != 'nomodel'",
                                                         fluidRow(
                                                           column(12,
                                                                  h4("SHAP Values (Feature Importance)"),
                                                                  helpText("SHAP values measure the contribution of each feature to individual predictions."),
                                                                  fluidRow(
                                                                    column(4, numericInput("shap_n_samples", "Number of samples for SHAP", 30, min = 5, max = 100, step = 5)),
                                                                    column(4, actionButton("compute_shap", "Compute SHAP",
                                                                                           style = "background-color: #63BFBF; color: white; margin-top: 25px;"))
                                                                  ),
                                                                  plotOutput("shap_importance_plot", height = "500px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                  p(downloadButton("download_shap_plot", "Download SHAP plot"),
                                                                    downloadButton("download_shap_data", "Download SHAP data"), align = "center")
                                                           )
                                                         ),
                                                         hr(),
                                                         fluidRow(
                                                           column(12,
                                                                  h4("Partial Dependence Plot (PDP)"),
                                                                  helpText("Shows the marginal effect of a feature on model predictions."),
                                                                  fluidRow(
                                                                    column(4, selectInput("pdp_feature", "Select feature:", choices = NULL)),
                                                                    column(4, actionButton("compute_pdp", "Generate PDP",
                                                                                           style = "background-color: #63BFBF; color: white; margin-top: 25px;"))
                                                                  ),
                                                                  plotOutput("pdp_plot", height = "400px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                  p(downloadButton("download_pdp_plot", "Download PDP"), align = "center")
                                                           )
                                                         ),
                                                         hr(),
                                                         fluidRow(
                                                           column(12,
                                                                  h4("LIME (Local Interpretable Model-agnostic Explanations)"),
                                                                  helpText("Explains individual predictions by approximating the model locally with an interpretable model."),
                                                                  fluidRow(
                                                                    column(3, numericInput("lime_n_features", "Number of features", 10, min = 3, max = 30, step = 1)),
                                                                    column(3, numericInput("lime_sample_idx", "Sample index to explain", 1, min = 1, max = 100, step = 1)),
                                                                    column(3, actionButton("compute_lime", "Generate LIME",
                                                                                           style = "background-color: #63BFBF; color: white; margin-top: 25px;"))
                                                                  ),
                                                                  plotOutput("lime_plot", height = "500px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                  p(downloadButton("download_lime_plot", "Download LIME plot"), align = "center")
                                                           )
                                                         )
                                                       ),
                                                       conditionalPanel(condition = "input.model == 'nomodel'",
                                                                        h4("Please select and train a model first.", style = "color: grey; text-align: center; margin-top: 50px;")
                                                       )
                                              ),
                                              tabPanel("Advanced Visualizations", icon = icon("chart-area"),
                                                       br(),
                                                       h3("Advanced Visualizations"),
                                                       fluidRow(
                                                         column(4,
                                                                radioButtons("adv_viz_data_source", "Data source:",
                                                                             c("Transformed data" = "transformed",
                                                                               "Selected variables (test)" = "selected",
                                                                               "Model variables" = "model"),
                                                                             selected = "transformed")
                                                         ),
                                                         column(4,
                                                                numericInput("tsne_perplexity", "t-SNE perplexity", 30, min = 5, max = 100, step = 5),
                                                                numericInput("umap_n_neighbors", "UMAP n_neighbors", 15, min = 2, max = 50, step = 1)
                                                         ),
                                                         column(4,
                                                                numericInput("heatmap_n_top", "Heatmap top N features", 30, min = 5, max = 100, step = 5),
                                                                numericInput("cor_threshold", "Correlation network threshold", 0.6, min = 0.1, max = 0.95, step = 0.05)
                                                         )
                                                       ),
                                                       hr(),
                                                       h4("Dimensionality Reduction"),
                                                       fluidRow(
                                                         column(6,
                                                                h5("t-SNE"),
                                                                plotlyOutput("tsne_plot", height = "450px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("download_tsne", "Download t-SNE"), align = "center")
                                                         ),
                                                         column(6,
                                                                h5("UMAP"),
                                                                plotlyOutput("umap_plot", height = "450px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("download_umap", "Download UMAP"), align = "center")
                                                         )
                                                       ),
                                                       hr(),
                                                       h4("Feature Analysis"),
                                                       fluidRow(
                                                         column(12,
                                                                h5("Clustered Heatmap"),
                                                                plotOutput("clustered_heatmap", height = "600px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("download_heatmap", "Download heatmap"), align = "center")
                                                         )
                                                       ),
                                                       hr(),
                                                       fluidRow(
                                                         column(12,
                                                                h5("Correlation Network"),
                                                                plotOutput("correlation_network", height = "600px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("download_cor_network", "Download network"), align = "center")
                                                         )
                                                       ),
                                                       hr(),
                                                       conditionalPanel(condition = "input.model != 'nomodel'",
                                                         fluidRow(
                                                           column(12,
                                                                  h5("Calibration Plot"),
                                                                  helpText("Shows how well predicted probabilities match actual outcomes."),
                                                                  plotOutput("calibration_plot", height = "500px") %>% withSpinner(color="#0dc5c1",type = 1),
                                                                  p(downloadButton("download_calibration", "Download calibration plot"), align = "center")
                                                           )
                                                         )
                                                       )
                                              ),
                                              tabPanel("Test parameters", icon  =  icon("cog"),
                                                       fluidRow(
                                                         column(6,
                                                                h4("Selection Parameters"),
                                                                sliderInput("prctvaluestest", "Percent of values accepted",min = 0, max = 100, value = c(50,50),width="60%"),
                                                                checkboxGroupInput("selectmethodtest","Methods of selection ",c("selection on all samples"="nogroup","each group has more than x% of values "="bothgroups",
                                                                                                                                "at least one group has more than x% of more"="onegroup"),selected ="bothgroups" )
                                                         ),
                                                         column(6,
                                                                checkboxGroupInput("NAstructuretest", "Select variables with a NA's structure " , choices = list("TRUE /!\\"=TRUE,"FALSE"=FALSE),selected ="FALSE"),
                                                                helpText("/!\\ process can be long"),
                                                                conditionalPanel(condition ="output.testNAstructure ",
                                                                                 fluidRow(
                                                                                   column(6,
                                                                                          numericInput("thresholdNAstructuretest","pvalue for the structure test" , 0.05, min = 0, max = 1, step = 0.005),
                                                                                          radioButtons("structdatatest", "search structure in",c("all dataset" = "alldata","selected dataset" = "selecteddata"))
                                                                                   ),
                                                                                   column(6,
                                                                                          numericInput("maxvaluesgroupmintest","The group with the minimum number of values has at most x% of values",value = 25,min = 0,max = 100,step = 5),
                                                                                          numericInput("minvaluesgroupmaxtest","The group with the maximum number of values has at least y% of values",value = 75,min = 0,max = 100,step = 5)
                                                                                   )
                                                                                 )
                                                                )
                                                         )
                                                       ),
                                                       #textOutput("testNAstructure"),
                                                       #hr(),
                                                       fluidRow(
                                                         column(6,h3("Transform Parameters")),
                                                         column(6,h3("Statistics Parameters"))
                                                       ),
                                                       fluidRow(
                                                         column(3,
                                                                checkboxGroupInput("rempNAtest", "Replacing NA (Not Attributes) by",
                                                                                   c("zero" = "z",
                                                                                     "mean of the cohort" = "moy",
                                                                                     "mean by group"="moygr",
                                                                                     "PCA estimation" = "pca",
                                                                                     "Random forest estimation /!\\" = "missforest"),
                                                                                   selected = "moygr")
                                                         ),
                                                         column(3,
                                                                #br(),br(),
                                                                checkboxGroupInput("logtest","transform data in log",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected = "FALSE"),
                                                                radioButtons("logtypetest",label = NULL,c("ln"="logn","log 10"="log10","log2"="log2"),inline = TRUE),
                                                                checkboxGroupInput("standardizationtest","standardization dataset",
                                                                                   choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,
                                                                                   selected = "FALSE"),
                                                                checkboxGroupInput("arcsintest","arcsine transformation",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected ="FALSE")
                                                         ),
                                                         #),
                                                         #hr(),
                                                         #fluidRow(
                                                         column(3,
                                                                checkboxGroupInput("testtest", "Tests",
                                                                                   
                                                                                   c( "No test"="notest",
                                                                                      "Wilcoxon Test" = "Kruskal",
                                                                                      "Student Test" = "ANOVA",
                                                                                      # "Clustering + ElasticNet" = "clustEnet",
                                                                                      "Lasso" = "lasso", 
                                                                                      "ElasticNet" = "elasticnet"
                                                                                      # ,
                                                                                      # "Ridge" = "ridge"
                                                                                      ),
                                                                                   selected = "Kruskal"),
                                                                checkboxGroupInput("adjustpvtest", " " , 
                                                                                   #adjust p-value
                                                                                   choices = list("TRUE"=TRUE,"FALSE"=FALSE),
                                                                                   inline = TRUE,
                                                                                   selected = "FALSE")
                                                         ),
                                                         column(3,
                                                                numericInput("thresholdFCtest","choise of the Fold change threshold" , 0, min =0, max = 5, step = 0.5),
                                                                numericInput("thresholdpvtest","choise of the p-value threshold %" , 0.05, min =0, max = 1, step = 0.01)
                                                         )
                                                       ),
                                                       #hr(),
                                                       h3("Model Parameters"),
                                                       fluidRow(
                                                         column(3,
                                                                # checkboxGroupInput("modeltest", "Type of model to adjust", 
                                                                #                    c("No model" = "nomodel",
                                                                #                      "Random Forest"="randomforest",
                                                                #                      "Support Vector Machine" = "svm"),
                                                                #                    selected = "svm")
                                                                checkboxGroupInput("modeltest", "Type of model to adjust",
                                                                                   c("No model" = "nomodel",
                                                                                     "Random Forest"="randomforest",
                                                                                     "Support Vector Machine" = "svm",
                                                                                     "ElasticNet"="elasticnet",
                                                                                     "XGBoost"="xgboost",
                                                                                     # "LightGBM"="lightgbm",  
                                                                                     "K-Nearest Neighbors"="knn",
                                                                                     "Naive Bayes"="naivebayes"),
                                                                                   selected = "svm")
                                                         ),
                                                         column(4,
                                                                #numericInput("thresholdmodeltest","threshold model" ,0, min = -1, max = 1, step = 0.05),
                                                                radioButtons("tuning_method_test", "Hyperparameter tuning:",
                                                                             c("Default parameters" = "default",
                                                                               "Automatic tuning" = "automatic"),
                                                                             selected = "default"),
                                                                helpText("Automatic tuning uses model-specific optimization (tune.svm, tuneRF, cv.glmnet, xgb.cv, etc.)"),
                                                                checkboxGroupInput("fstest","features selection by cross validation",choices = list("TRUE /!\\"=TRUE,"FALSE"=FALSE),inline = TRUE,selected ="FALSE"),
                                                                helpText("/!\\ process can be long"),
                                                                helpText("Multi-class classification: predicted class is the one with highest probability")
                                                                # radioButtons("threshold_method_test", "Threshold optimization:",
                                                                #              c("Fixed (0.5 for probabilistic models)" = "fixed",
                                                                #                "Youden (maximize sensitivity + specificity)" = "youden",
                                                                #                "Equiprobability (equal error rate)" = "equiprob"),
                                                                #              selected = "fixed"),
                                                                # helpText("Threshold calculated on TRAIN data, applied to validation. 
                                                                #          Youden: optimal balance sens/spec. 
                                                                #          Equiprobability: minimizes FP=FN. 
                                                                #          Note: Multiple testing may slightly inflate validation metrics.")
                                                         ),
                                                         column(5,
                                                                p(actionButton("tunetest",h4("Test all models"),width=200),align="center")
                                                         )
                                                       ),
                                                       dataTableOutput("tabtestparameters")%>% withSpinner(color="#0dc5c1",type = 1),
                                                       p(downloadButton("downloadtabtestparameters","Download dataset"),align="center"),
                                                       # les graphiques pour les différents paramètres
                                                       fluidRow(
                                                         column(6,
                                                                plotOutput("plottestparameterslearning")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("downloadplottestparameterslearning","Download plot"), align = 'center')
                                                         ),
                                                         column(6,
                                                                plotOutput("plottestparametersvalidation")%>% withSpinner(color="#0dc5c1",type = 1),
                                                                p(downloadButton("downloadplottestparametersvalidation","Download plot"), align = 'center')
                                                         )
                                                       ),
                                                       fluidRow(
                                                         column(
                                                           12,
                                                           plotOutput("plottestparametersboth")%>% withSpinner(color="#0dc5c1",type = 1),
                                                           p(downloadButton("downloadplottestparametersboth","Download plot"), align = 'center')
                                                         )
                                                       ),
                                                       # fluidRow(
                                                       #   column(
                                                       #     12,
                                                       #     plotOutput("plottestparametersboth")%>% withSpinner(color="#0dc5c1",type = 1),
                                                       #     p(downloadButton("downloadplottestparametersboth","Download plot"), align = 'center')
                                                       #   )
                                                       # ),
                                                       # h4("Additional Analysis", style = "margin-top: 30px; margin-bottom: 20px;"),
                                                       # fluidRow(
                                                       #   column(
                                                       #     12,
                                                       #     h5("Threshold vs Performance", style = "margin-bottom: 10px;"),
                                                       #     plotOutput("plottestparametersthreshold")%>% withSpinner(color="#0dc5c1",type = 1),
                                                       #     p(downloadButton("downloadplottestparametersthreshold","Download plot"), align = 'center'),
                                                       #     helpText("Relationship between optimal threshold and validation performance. Each point represents a parameter combination. The curve shows the trend.")
                                                       #   )
                                                       # ),
                                                       fluidRow(
                                                         column(
                                                           12,
                                                           h5("Overfitting Analysis", style = "margin-top: 20px; margin-bottom: 10px;"),
                                                           plotOutput("plottestparametersoverfitting")%>% withSpinner(color="#0dc5c1",type = 1),
                                                           p(downloadButton("downloadplottestparametersoverfitting","Download plot"), align = 'center'),
                                                           helpText("Analysis of model overfitting. Positive values indicate overfitting (learning performance > validation performance). Values close to zero indicate good generalization.")
                                                         )
                                                       )
                                              )
                                            )
                                   )
                       )
      )
    )
    )
