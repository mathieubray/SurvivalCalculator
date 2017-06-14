library(shiny)
library(dplyr)

notice<-HTML("<b>Companion Paper:</b> Ashby VB, Leichtman AB, Rees MA, Song PXK, Bray M, Wang W, Kalbfleisch JD (2017). A Kidney Graft Survival Calculator that Accounts for Mismatches in Age, Sex, HLA and Body Size. Clinical Journal of the American Society of Nephrology. 
              DOI: <a href='http://cjasn.asnjournals.org/content/early/2017/06/07/CJN.09330916'>CJN.09330916</a> (Published Online: June 2017). 
              
              <br><br>

              This work was supported in part by the National Institute of Diabetes and Digestive and Kidney Diseases (NIDDK) through grant number 1R01-DK093513.   Dr. Rees  is supported in part by NIAID grants 
              R21 AI-111579 and R01-AI090244. Drs. Rees and Leichtman are supported in part by AHRQ grant R18 HS-020610.
             
              <br><br>

              <b>Disclaimer:</b> The authors do not assume responsibility for improper use of the calculator. Calculator should be used for academic interest only.
              By accessing the calculator, you agree that the authors, the University of Michigan and the Kidney Epidemiology and Cost Center will 
              not be liable to you for any loss or injury based on the information procured from the calculator. The use of information from the 
              calculator is at the user's own risk. Users should never disregard professional medical advice or delay in seeking it because of 
              any information provided by the calculator.

              <br><br>

              <b>Updates:</b> 06/13/2017: Replaced DR Mismatch sliders (for both Donor and Comparision Donor) with dropdown menus, which automatically refresh 
              available options such that the number of DR mismatches cannot be more than the specified number of total HLA mismatches.
              
              <br><br><br>

              Mathieu Bray 2017. Estimated Kidney Graft Survival Calculator. With contributions from: Ashby VB, Leichtman AB, 
              Rees MA, Song PXK, Wang W, Kalbfleisch JD.
")

shinyUI(fluidPage(
  
  titlePanel("Estimated Kidney Graft Survival Calculator"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("outcome", label="Display output in terms of...", choices=c("Survival","Failure"), selected = "Failure"),
      checkboxInput("metric", label = "Use Metric Units", value = T),
      br(),
      
      conditionalPanel(
        condition = "input.outcome=='Survival'",
        h5("Based on a Cox proportional hazards model fitted to SRTR data from 1998-2012, this calculator gives estimates of the probability of kidney graft survival
            (ie. transplant recipient is alive with a viable graft) at five and ten years based on characteristics about the donor and intended recipient.")
      ),
      conditionalPanel(
        condition = "input.outcome=='Failure'",
        h5("Based on a Cox proportional hazards model fitted to SRTR data from 1998-2012, this calculator gives estimates of the probability of kidney graft failure
            (ie. transplant recipient experiences graft loss or death) at five and ten years based on characteristics about the donor and intended recipient.")
      ),
      
      h5("To use the calculator, enter information about the intended recipient below; information about the donor can be entered to the right.
         By checking the box at the top, a second comparison donor can also be included. Probability estimates are refreshed automatically after
         entering a new characteristic."),
      
      h5(strong("The authors do not assume responsibility for improper use of the calculator. Calculator should be used for academic interest only.")),
      br(),
      
      h4("Recipient Characteristics"),
      
      fluidRow(
        column(6,numericInput("r.age",
                                      label="Age (yrs)",
                                      min=6,max=100,value=40,step=1)
        ),
        column(6,selectInput("r.sex",
                                      label = "Sex",
                                      choices = c("Female","Male"),
                                      selected = "Female")
        )
      ),
      
      selectInput("r.insurance",
                             label = "Insurance Status",
                             choices = c("Public Primary Payer","Private Primary Payer","Other"),
                             selected = "Public Primary Payer"),
      
      conditionalPanel(
        condition = "input.metric==true",
        fluidRow(
          column(6,sliderInput("r.heightcm",
                                            label="Height (cm)",
                                            min=50,max=250,value=150,step=1)
          ),
          
          column(6,sliderInput("r.weightkg",
                                            label="Weight (kg)",
                                            min=20,max=200,value=63,step=1)
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.metric==false",
        fluidRow(
          column(6,sliderInput("r.heightin",
                                            label="Height (in)",
                                            min=20,max=100,value=60,step=1)
          ),
          
          column(6,sliderInput("r.weightlb",
                                            label="Weight (lb)",
                                            min=45,max=450,value=139,step=1)
          )
        )
      ),
      
      fluidRow(
        column(6,selectInput("r.race",
                                      label = "Race",
                                      choices = c("White","Black","Hispanic","Other"),
                                      selected = "White")
        ),
        column(6,selectInput("r.dialysis",
                                        label="Time on Dialysis",
                                        choices = c("None","< 1 Year","1-2 Years","2-3 Years","> 3 Years"),
                                        selected = "None")
        )
      ),
      
      fluidRow(
        column(6,selectInput("r.bt",
                                    label = "Blood Type",
                                    choices = c("O","A","B","AB"),
                                    selected = "O")
        )
      ),
      
      checkboxInput("r.diabetes", label = "Recipient has Diabetes", value = F),
      
      checkboxInput("r.prevTrans", label = "Recipient has had a Previous Transplant", value = F),
      
      checkboxInput("r.hepC", label = "Recipient has Hepatitis C", value = F),
      
      sliderInput("r.pra", label="Panel Reactive Antibody (PRA)", min=0, max=100, value=0)
      
    ),
      
    mainPanel(
      
      checkboxInput("comparisonDonor", label = "Show Comparison Donor", value = F),
      
      conditionalPanel(
        condition = "input.comparisonDonor==true",
        h4(textOutput("textComparison"),align="center"),
        br()
      ),
     
      h4(strong("Donor Characteristics"),style="color:blue"),
      
      fluidRow(
        column(3,numericInput("d.age",
                                      label="Age (yrs)",
                                      min=6,max=100,value=40,step=1)
        ),
        column(3,selectInput("d.sex",
                                    label = "Sex",
                                    choices = c("Female","Male"),
                                    selected = "Female")
        ),
        column(6,
          conditionalPanel(
            condition = "input.metric==true",
            fluidRow(
              column(6,sliderInput("d.heightcm",
                                                label="Height (cm)",
                                                min=50,max=250,value=150,step=1)
              ),
              column(6,sliderInput("d.weightkg",
                                                 label="Weight (kg)",
                                                min=20,max=200,value=63,step=1)
              )
            )
          ),
        
          conditionalPanel(
            condition = "input.metric==false",
            fluidRow(
              column(6,sliderInput("d.heightin",
                                                label="Height (in)",
                                                min=20,max=100,value=60,step=1)
              ),
              column(6,sliderInput("d.weightlb",
                                                label="Weight (lb)",
                                                min=45,max=450,value=139,step=1)
              )
            )
          )
          
        )
      ),
      
      fluidRow(
        column(3,selectInput("d.related",
                                      label = "Relation to Recipient",
                                      choices = c("Unrelated","Related - 1st Degree","Related - 2nd Degree"),
                                      selected = "Unrelated")
        ),
        column(3,selectInput("d.race",
                                      label = "Race",
                                      choices = c("White","Black","Hispanic","Other"),
                                      selected = "White")
        ),
        column(3,sliderInput("d.hla.mismatches",
                                              label="HLA Mismatches",
                                              min=0,max=6,value=0,step=1)
        ),
        
        column(3,selectInput("d.dr.mismatches",
                               label="DR Mismatches",
                               choices=c(0),
                               selected=0)
        )
        
      ),
      
      fluidRow(
        column(3,selectInput("d.bt",
                  label = "Blood Type",
                  choices = c("O","A","B","AB"),
                  selected = "O")),
      
        column(9,checkboxInput("d.cigarette",label = "History of Cigarette Use",value = F))
      ),
        
      h5(strong(textOutput("textDonor")),style="color:blue"),
      br(),
      
      conditionalPanel(
        condition = "input.comparisonDonor==true",
        h4(strong("Comparison Donor Characteristics"),style="color:red"),
      
        fluidRow(
          column(3,numericInput("c.age",
                                        label="Age (yrs)",
                                        min=6,max=100,value=40,step=1)
          ),
          column(3,selectInput("c.sex",
                                          label = "Sex",
                                          choices = c("Female","Male"),
                                          selected = "Female")
          ),
          column(6,
            conditionalPanel(
              condition = "input.metric==true",
              fluidRow(
                column(6,sliderInput("c.heightcm",
                                                  label="Height (cm)",
                                                  min=50,max=250,value=150,step=1)
                ),
                column(6,sliderInput("c.weightkg",
                                                  label="Weight (kg)",
                                                  min=20,max=200,value=63,step=1)
                )
              )
            ),
        
            conditionalPanel(
              condition = "input.metric==false",
              fluidRow(
                column(6,sliderInput("c.heightin",
                                                  label="Height (in)",
                                                  min=20,max=100,value=60,step=1)
                ),
                column(6,sliderInput("c.weightlb",
                                                  label="Weight (lb)",
                                                  min=45,max=450,value=139,step=1)
                )
              )
            )
            
          )
        ),
      
        fluidRow(
          column(3,selectInput("c.related",
                                        label = "Relation to Recipient",
                                        choices = c("Unrelated","Related - 1st Degree","Related - 2nd Degree"),
                                        selected = "Unrelated")
          ),
          column(3, selectInput("c.race",
                                        label = "Race",
                                        choices = c("White","Black","Hispanic","Other"),
                                        selected = "White")
          ),
          column(3,sliderInput("c.hla.mismatches",
                                                label="HLA Mismatches",
                                                min=0,max=6,value=0,step=1)
          ),
          column(3,selectInput("c.dr.mismatches",
                                                label="DR Mismatches",
                                                choices=c(0),
                                                selected=0)
          )
          
        ),
      
        fluidRow(
          column(3,selectInput("c.bt",
                                      label = "Blood Type",
                                      choices = c("O","A","B","AB"),
                                      selected = "O")
          ),
          column(9,checkboxInput("c.cigarette",label = "History of Cigarette Use",value = F))
        ),
        
        h5(strong(textOutput("textComparisonDonor")),style="color:red"),
        br()
      ),
      
      h6(notice)
    )
  )
))


