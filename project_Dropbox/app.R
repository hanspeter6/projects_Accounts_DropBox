###
### this app will attempt to read a list of files from a "remote" Dropbox folder
### the programme will then process them and create a single dataframe which
### will then be stored in the "remote" Dropbox folder. will try to work with reactive functions this time
### 
### input to the app will be the Dropbox details .... not too sure at this time...
### 
###
## libraries
library(tm) # text mining... not sure why anymore...
library(pdftools)
library(stringr)
library(dplyr)
library(rdrop2)

token <- drop_auth()
saveRDS(token, "droptoken.rds")

## Only run examples in interactive R sessions
if (interactive()) {
        
        ui <- fluidPage(
                sidebarLayout(
                        sidebarPanel(
                                
                                # inputs
                                helpText("Enter the directory from your root Dropbox",
                                        "Press the submit button when you ready"),
                                
                                textInput("inputDir", "Input Directory"),
                                
                                submitButton("Submit", icon = NULL)
                                
                                # br(),
                                # textInput("outputDir", "Output Directory"),
                                # br(),
                                # textInput("keys", "Key Words") 

                        ),
                        mainPanel(
                                # textOutput("contents1"),
                                # br(),
                                # textOutput("contents2"),
                                # br(),
                                # textOutput("contents3"),
                                # br(),
                                textOutput("contents4")

                        )
                )
        )
        
        
        # create list of pdf documents read from input directory from remote dropbox as reactive statement
       
        

        
        server <- function(input, output) {
                
                dataInput <- reactive( {
                        
                        req(input$inputDir)
                        # inputDir <- "inputDirectory"
                        
                        token <- readRDS("droptoken.rds")
                        
                        filesInfo <- drop_dir(as.character(input$inputDir), dtoken = token) #list of files in Dropbox /inputDir ("inputDirectory").NB also want to confirm only .pdfs
                        
                        filePaths <- filesInfo$path_display # paths to the files
                        
                        doc_list <- list()
                        for(i in 1:length(filePaths)) {#
                                
                                drop_download(filePaths[i], dtoken = token, overwrite = TRUE) # download each from remote dropbox to current wrkdir
                                
                                temp <- pdf_text(filesInfo$name[i]) # list of single character string per pdf page
                                
                                file.remove(filesInfo$name[i]) # remove the file that was added
                                
                                doc_list[[i]] <- unlist(strsplit(temp,"\n")) # create list of documents split by line
                        }
                        
                        file.remove("droptoken.rds") # cleaning up local working directory
                        
                        return(doc_list[[2]]) # seems can only return single element of a list in this case!!
                        
                })
               
                # output$contents1 <- renderText({
                # 
                #         print(input$inputDir)
                # })               

                # output$contents2 <- renderText({
                #         
                #         print(input$outputDir)
                # })
                # 
                # output$contents3 <- renderText({
                #         
                #         print(input$keys)
                # })
                # 
                output$contents4 <- renderText({

                        print(dataInput())
                })

                
                
        }
        
        shinyApp(ui, server)
}

# # Read all the files into a list
# filesInfo <- drop_dir(input$inputDir) #list of files in Dropbox /inputDir ("inputDirectory")
# filePaths <- filesInfo$path_display # paths to the files
# drop_download(filePaths[5]) # download one of them from remote dropbox to current wrkdir
# temp1 <- pdf_text(filesInfo$name[5]) # list of single string per pdf page
# doc <- unlist(strsplit(temp1,"\n"))  # by
# 
# return(doc)
# 