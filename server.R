library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(Unicode)

options(warn=-1)
estimate.list<-read.csv("data/Parameters.csv",header=T)

survivalCalculator<-function(fiveyear,donor,recip){
  
  linearCombination<-numeric()
  standardError<-numeric()
  
  if(fiveyear){
    est.table <- estimate.list %>% select(-TenYearEstimate,-TenYearSE)
  } else {
    est.table <- estimate.list %>% select(-FiveYearEstimate,-FiveYearSE)
  }
  
  #Relation
  
  est.related <- est.table %>% filter(Characteristic == "Living Donor Type")
  if (donor$d.related=="Related - 1st Degree"){
    linearCombination <- c(linearCombination,est.related[2,3])
    standardError <- c(standardError,est.related[2,4])
  } else {
    if (donor$d.related=="Related - 2nd Degree"){
      linearCombination <- c(linearCombination,est.related[3,3])
      standardError <- c(standardError,est.related[3,4])
    }
  }
  
  #Age
  
  donorage<-donor$d.age
  index<-1
  ages<-c(30,10,10,10)
  
  est.r.age <- est.table %>% filter(Characteristic == "Recipient Age")
  if (recip$r.age < 12){
    linearCombination <- c(linearCombination,est.r.age[1,3])
    standardError <- c(standardError,est.r.age[1,4])
    
    est.d.age = est.table %>% filter(Characteristic == "Among Recipients Age < 12")
    while (donorage > 0 & index < 4){
      linearCombination <- c(linearCombination,donorage*est.d.age[index,3])
      standardError <- c(standardError,donorage*est.d.age[index,4])
      
      donorage<-donorage-ages[index]
      index<-index+1
    }
    
  } else if (recip$r.age >= 13 & recip$r.age <= 17){
    linearCombination <- c(linearCombination,est.r.age[2,3])
    standardError <- c(standardError,est.r.age[2,4])
    
    est.d.age = est.table %>% filter(Characteristic == "Among Recipients 13-17")
    while (donorage > 0 & index < 4){
      linearCombination <- c(linearCombination,donorage*est.d.age[index,3])
      standardError <- c(standardError,donorage*est.d.age[index,4])
      
      donorage<-donorage-ages[index]
      index<-index+1
    }
    
  } else if (recip$r.age >= 18 & recip$r.age <= 29){
    linearCombination <- c(linearCombination,est.r.age[3,3])
    standardError <- c(standardError,est.r.age[3,4])
    
    est.d.age = est.table %>% filter(Characteristic == "Among Recipients 18-29")
    while (donorage > 0 & index < 5){
      linearCombination <- c(linearCombination,donorage*est.d.age[index,3])
      standardError <- c(standardError,donorage*est.d.age[index,4])
      
      donorage<-donorage-ages[index]
      index<-index+1
    }
    
  } else if (recip$r.age >= 30 & recip$r.age <= 39){
    linearCombination <- c(linearCombination,est.r.age[4,3])
    standardError <- c(standardError,est.r.age[4,4])
    
    est.d.age = est.table %>% filter(Characteristic == "Among Recipients 30-39")
    while (donorage > 0 & index < 5){
      linearCombination <- c(linearCombination,donorage*est.d.age[index,3])
      standardError <- c(standardError,donorage*est.d.age[index,4])
      
      donorage<-donorage-ages[index]
      index<-index+1
    }
    
  } else if (recip$r.age >=40 & recip$r.age <=49){
    
    est.d.age = est.table %>% filter(Characteristic == "Among Recipients 40-49")
    while (donorage > 0 & index < 5){
      linearCombination <- c(linearCombination,donorage*est.d.age[index,3])
      standardError <- c(standardError,donorage*est.d.age[index,4])
      
      donorage<-donorage-ages[index]
      index<-index+1
    }
    
  } else if (recip$r.age >= 50 & recip$r.age <=59){
    
    linearCombination <- c(linearCombination,est.r.age[6,3])
    standardError <- c(standardError,est.r.age[6,4])
    
    est.d.age = est.table %>% filter(Characteristic == "Among Recipients 50-59")
    while (donorage > 0 & index < 5){
      linearCombination <- c(linearCombination,donorage*est.d.age[index,3])
      standardError <- c(standardError,donorage*est.d.age[index,4])
      
      donorage<-donorage-ages[index]
      index<-index+1
    }
    
  } else if (recip$r.age >= 60){
    linearCombination <- c(linearCombination,est.r.age[7,3])
    standardError <- c(standardError,est.r.age[7,4])
    
    est.d.age = est.table %>% filter(Characteristic == "Among Recipients 60+")
    while (donorage > 0 & index < 5){
      linearCombination <- c(linearCombination,donorage*est.d.age[index,3])
      standardError <- c(standardError,donorage*est.d.age[index,4])
      
      donorage<-donorage-ages[index]
      index<-index+1
    }
  }
  
  # Sex
  
  est.sex <- est.table %>% filter(Characteristic == "Donor-Recipient Sex")
  if (donor$d.sex == "Female" & recip$r.sex == "Female"){
    linearCombination <- c(linearCombination,est.sex[1,3])
    standardError <- c(standardError,est.sex[1,4])
  } else if (donor$d.sex == "Male" & recip$r.sex == "Female"){
    linearCombination <- c(linearCombination,est.sex[2,3])
    standardError <- c(standardError,est.sex[2,4])
  } else if (donor$d.sex == "Male" & recip$r.sex == "Male"){
    linearCombination <- c(linearCombination,est.sex[4,3])
    standardError <- c(standardError,est.sex[4,4])
  }
  
  # ABDR Mismatch
  
  est.abdr <- est.table %>% filter(Characteristic == "HLA ABDR Mismatch")
  if (donor$d.hla.mismatches == 0){
    linearCombination <- c(linearCombination,est.abdr[1,3])
    standardError <- c(standardError,est.abdr[1,4])
  } else if (donor$d.hla.mismatches>=1 & donor$d.hla.mismatches<=2 & donor$d.dr.mismatches==0){
    linearCombination <- c(linearCombination,est.abdr[2,3])
    standardError <- c(standardError,est.abdr[2,4])
  } else if (donor$d.hla.mismatches>=1 & donor$d.hla.mismatches<=2){
    linearCombination <- c(linearCombination,est.abdr[3,3])
    standardError <- c(standardError,est.abdr[3,4])
  } else if (donor$d.hla.mismatches>=3 & donor$d.hla.mismatches<=4 & donor$d.dr.mismatches==0){
    linearCombination <- c(linearCombination,est.abdr[4,3])
    standardError <- c(standardError,est.abdr[4,4])
  } else if (donor$d.hla.mismatches>=5){
    linearCombination <- c(linearCombination,est.abdr[6,3])
    standardError <- c(standardError,est.abdr[6,4])
  }
  
  # Obesity
  
  if (recip$metric==T){
    r.bmi <- recip$r.weight/((recip$r.height/100) * (recip$r.height/100))
    d.bmi <- donor$d.weight/((donor$d.height/100) * (donor$d.height/100))
    
  } else {
    r.bmi <- 703*recip$r.weight/(recip$r.height * recip$r.height)
    d.bmi <- 703*donor$d.weight/(donor$d.height * donor$d.height)
  }
  
  est.r.obese  <- est.table %>% filter(Characteristic == "Recipient BMI")
  if (r.bmi > 30){
    linearCombination <- c(linearCombination,est.r.obese[2,3])
    standardError <- c(standardError,est.r.obese[2,4])
  }
  
  est.d.obese <- est.table %>% filter(Characteristic == "Donor BMI")
  if (d.bmi > 30){
    linearCombination <- c(linearCombination,est.d.obese[2,3])
    standardError <- c(standardError,est.d.obese[2,4])
  }
  
  # Weight Ratio
  
  weightRatio <- donor$d.weight/recip$r.weight
  
  est.weightRatio <- est.table %>% filter(Characteristic == "Donor-Recipient Weight Ratio")
  if (weightRatio < 0.75){
    linearCombination <- c(linearCombination,est.weightRatio[1,3])
    standardError <- c(standardError,est.weightRatio[1,4])
  } else if (weightRatio >= 0.75 & weightRatio < 0.9){
    linearCombination <- c(linearCombination,est.weightRatio[2,3])
    standardError <- c(standardError,est.weightRatio[2,4])
  } else if (weightRatio > 1.15){
    linearCombination <- c(linearCombination,est.weightRatio[4,3])
    standardError <- c(standardError,est.weightRatio[4,4])
  }
  
  # Height Ratio
  
  heightRatio <- donor$d.height/recip$r.height
  
  est.heightRatio <- est.table %>% filter(Characteristic == "Donor-Recipient Height Ratio")
  if (heightRatio < 0.94){
    linearCombination <- c(linearCombination,est.heightRatio[1,3])
    standardError <- c(standardError,est.heightRatio[1,4])
  } else if (heightRatio >= 0.94 & heightRatio < 1){
    linearCombination <- c(linearCombination,est.heightRatio[2,3])
    standardError <- c(standardError,est.heightRatio[2,4])
  } else if (heightRatio > 1.06){
    linearCombination <- c(linearCombination,est.heightRatio[4,3])
    standardError <- c(standardError,est.heightRatio[4,4])
  }
  
  # Recipient Race
  
  est.r.race <- est.table %>% filter(Characteristic == "Recipient Race")
  if (recip$r.race == "Black"){
    linearCombination <- c(linearCombination,est.r.race[2,3])
    standardError <- c(standardError,est.r.race[2,4])
  } else if (recip$r.race == "Hispanic"){
    linearCombination <- c(linearCombination,est.r.race[3,3])
    standardError <- c(standardError,est.r.race[3,4])
  } else if (recip$r.race == "Other"){
    linearCombination <- c(linearCombination,est.r.race[4,3])
    standardError <- c(standardError,est.r.race[4,4])
  }
  
  # Donor Race
  
  est.d.race <- est.table %>% filter(Characteristic == "Donor Race")
  if (donor$d.race == "Black"){
    linearCombination <- c(linearCombination,est.d.race[2,3])
    standardError <- c(standardError,est.d.race[2,4])
  } else if (donor$d.race == "Hispanic"){
    linearCombination <- c(linearCombination,est.d.race[3,3])
    standardError <- c(standardError,est.d.race[3,4])
  }
  
  # Donor Cigarette Use
  
  est.d.cigarettes <- est.table %>% filter(Characteristic == "Donor Cigarette Use")
  if (donor$d.cigarette){
    linearCombination <- c(linearCombination,est.d.cigarettes[2,3])
    standardError <- c(standardError,est.d.cigarettes[2,4])
  }
  
  # ABO Incompatibility
  
  est.incompatibility <- est.table %>% filter(Characteristic == "ABO Incompatible")
  
  incompatible<-T
  
  if (donor$d.bt =="O"){
    incompatible<-F
  } else if (donor$d.bt=="A"){
    if (recip$r.bt=="A" | recip$r.bt=="AB"){
      incompatible<-F
    }
  } else if (donor$d.bt=="B"){
    if (recip$r.bt=="B" | recip$r.bt=="AB"){
      incompatible<-F
    }
  } else {
    if (recip$r.bt=="AB"){
      incompatible<-F
    }
  }
  
  if (incompatible==T){
    linearCombination <- c(linearCombination,est.incompatibility[2,3])
    standardError <- c(standardError,est.incompatibility[2,4])
  }
  
  # PRA
  
  est.pra <- est.table %>% filter(Characteristic == "PRA")
  if (recip$r.pra >= 10 & recip$r.pra <=79){
    linearCombination <- c(linearCombination,est.pra[2,3])
    standardError <- c(standardError,est.pra[2,4])
  } else if (recip$r.pra >= 80){
    linearCombination <- c(linearCombination,est.pra[3,3])
    standardError <- c(standardError,est.pra[3,4])
  }
  
  # Diabetes
  
  est.diabetes <- est.table %>% filter(Characteristic == "Recipient Diabetes Status")
  if (recip$r.diabetes == T){
    linearCombination <- c(linearCombination,est.diabetes[2,3])
    standardError <- c(standardError,est.diabetes[2,4])
  }
  
  # Previous Transplant
  
  est.transplant <- est.table %>% filter(Characteristic == "Previous Transplant")
  if (recip$r.prevTrans == T){
    linearCombination <- c(linearCombination,est.transplant[2,3])
    standardError <- c(standardError,est.transplant[2,4])
  }
  
  # Dialysis
  
  est.tod <- est.table %>% filter(Characteristic == "Time on Dialysis")
  if (recip$r.dialysis == "None"){
    linearCombination<-c(linearCombination,est.tod[1,3])
    standardError <- c(standardError,est.tod[1,4])
  } else if (recip$r.dialysis == "< 1 Year"){
    linearCombination<-c(linearCombination,est.tod[2,3])
    standardError <- c(standardError,est.tod[2,4])
  } else if (recip$r.dialysis == "2-3 Years"){
    linearCombination<-c(linearCombination,est.tod[4,3])
    standardError <- c(standardError,est.tod[4,4])
  } else if (recip$r.dialysis == "> 3 Years"){
    linearCombination<-c(linearCombination,est.tod[5,3])
    standardError <- c(standardError,est.tod[5,4])
  }
  
  # Hep C
  
  est.hepC <- est.table %>% filter(Characteristic == "Recipient Hepatitis C Seriology")
  if (recip$r.hepC == T){
    linearCombination <- c(linearCombination,est.hepC[2,3])
    standardError <- c(standardError,est.hepC[2,4])
  }
  
  # Insurance
  
  est.insurance <- est.table %>% filter(Characteristic == "Recipient Insurance")
  if (recip$r.insurance == "Private Primary Payer"){
    linearCombination <- c(linearCombination,est.insurance[2,3])
    standardError <- c(standardError,est.insurance[2,4])
  } else if (recip$r.insurance == "Other"){
    linearCombination <- c(linearCombination,est.insurance[3,3])
    standardError <- c(standardError,est.insurance[3,4])
  }
  
  # Year
  est.year <- est.table %>% filter(Characteristic == "Transplant Year")
  linearCombination <- c(linearCombination,est.year[3,3])
  standardError <- c(standardError,est.year[3,4])
  
  # Baseline
  est.baseline <- est.table %>% filter(Characteristic == "Baseline")
  base<-est.baseline[1,3]
  
  #print(linearCombination)
  
  
  survival<-base^(exp(sum(linearCombination)))
  lowerbound<-base^(exp(sum(linearCombination)+1.96*sum(standardError)))
  upperbound<-base^(exp(sum(linearCombination)-1.96*sum(standardError)))
  
  return(list(survival=survival,lowerbound=lowerbound,upperbound=upperbound))
  
}

shinyServer(function(input, output, session) {
  
  observeEvent(input$d.hla.mismatches, {
    updateSelectInput(session, 'd.dr.mismatches', choices=0:min(2,input$d.hla.mismatches), selected=ifelse(input$d.dr.mismatches <= input$d.hla.mismatches,input$d.dr.mismatches,input$d.hla.mismatches))
  })
  
  observeEvent(input$c.hla.mismatches, {
    updateSelectInput(session, 'c.dr.mismatches', choices=0:min(2,input$c.hla.mismatches), selected=ifelse(input$c.dr.mismatches <= input$c.hla.mismatches,input$c.dr.mismatches,input$c.hla.mismatches))
  })
  
  updateDonor<-reactive({
    
    donor.char<-list(
      d.age=input$d.age,
      d.sex=input$d.sex,
      d.height=ifelse(input$metric==T,input$d.heightcm,input$d.heightin),
      d.weight=ifelse(input$metric==T,input$d.weightkg,input$d.weightlb),
      d.related=input$d.related,
      d.race=input$d.race,
      d.hla.mismatches=input$d.hla.mismatches,
      d.dr.mismatches=input$d.dr.mismatches,
      d.bt=input$d.bt,
      d.cigarette=input$d.cigarette
      
    )
    
    return(donor.char)
    
  })
  
  updateComparisonDonor<-reactive({
    
    comp.char<-list(
      d.age=input$c.age,
      d.sex=input$c.sex,
      d.height=ifelse(input$metric==T,input$c.heightcm,input$c.heightin),
      d.weight=ifelse(input$metric==T,input$c.weightkg,input$c.weightlb),
      d.related=input$c.related,
      d.race=input$c.race,
      d.hla.mismatches=input$c.hla.mismatches,
      d.dr.mismatches=input$c.dr.mismatches,
      d.bt=input$c.bt,
      d.cigarette=input$c.cigarette
      
    )
    
    return(comp.char)
    
  })
  
  updateRecipient<-reactive({
    
    recip.char<-list(
      metric=input$metric,
      r.age=input$r.age,
      r.sex=input$r.sex,
      r.insurance=input$r.insurance,
      r.height=ifelse(input$metric==T,input$r.heightcm,input$r.heightin),
      r.weight=ifelse(input$metric==T,input$r.weightkg,input$r.weightlb),
      r.race=input$r.race,
      r.dialysis=input$r.dialysis,
      r.bt=input$r.bt,
      r.diabetes=input$r.diabetes,
      r.prevTrans=input$r.prevTrans,
      r.hepC=input$r.hepC,
      r.pra=input$r.pra
      
    )
    
    return(recip.char)
  })
  
  output$textDonor <- renderText({
    
    donor<-updateDonor()
    recip<-updateRecipient()
    
    if (input$comparisonDonor==T){
      tag<-"the original donor"
    } else {
      tag<-"the donor"
    }
    
    five.year.values<-survivalCalculator(T,donor,recip)
    ten.year.values<-survivalCalculator(F,donor,recip)
    
    survival.five.years <- five.year.values$survival
    survival.ten.years.cond <- ten.year.values$survival
    
    if (input$outcome == "Failure"){
      
      failure.five.years <- 1 - survival.five.years
      failure.ten.years <- 1 - survival.five.years*survival.ten.years.cond
      
      lowerbound.five.years <- 1 - five.year.values$upperbound
      upperbound.five.years <- 1 - five.year.values$lowerbound
      
      lowerbound.ten.years <- 1 - ten.year.values$upperbound
      upperbound.ten.years <- 1 - ten.year.values$lowerbound
      
      #pct.five.years <- paste0(round(failure.five.years*100,1),"% (CI: ", round(lowerbound.five.years*100,1),"-",round(upperbound.five.years*100,1),")")
      #pct.ten.years <- paste0(round(failure.ten.years*100,1),"% (CI: ", round(lowerbound.ten.years*100,1),"-",round(upperbound.ten.years*100,1),")")
      
      pct.five.years <- paste0(round(failure.five.years*100,1),"%")
      pct.ten.years <- paste0(round(failure.ten.years*100,1),"%")
      
      
      suffix.five.years <- "graft failure within 5 years"
      suffix.ten.years <- "graft failure within 10 years"
    
    } else {
      
      survival.ten.years <- survival.five.years*survival.ten.years.cond
      
      lowerbound.five.years <- five.year.values$lowerbound
      upperbound.five.years <- five.year.values$upperbound
      
      lowerbound.ten.years <- ten.year.values$lowerbound
      upperbound.ten.years <- ten.year.values$upperbound
      
      #pct.five.years <- paste0(round(survival.five.years*100,1),"% (CI: ", round(lowerbound.five.years*100,1),"-",round(upperbound.five.years*100,1),")")
      #pct.ten.years <- paste0(round(survival.ten.years*100,1),"% (CI: ", round(lowerbound.ten.years*100,1),"-",round(upperbound.ten.years*100,1),")")
      
      pct.five.years <- paste0(round(survival.five.years*100,1),"%")
      pct.ten.years <- paste0(round(survival.ten.years*100,1),"%")
      
      suffix.five.years <- "five-year graft survival"
      suffix.ten.years <- "ten-year graft survival"
      
    }
    
    outputText<-paste0("The recipient is estimated to have a ",pct.five.years," chance of ",suffix.five.years," and a ",
                       pct.ten.years," chance of ",suffix.ten.years," with ",tag)
    
    return(outputText)
    
  })
  
  output$textComparisonDonor <- renderText({
    
    comp<-updateComparisonDonor()
    recip<-updateRecipient()
    
    five.year.values <- survivalCalculator(T,comp,recip)
    ten.year.values <- survivalCalculator(F,comp,recip)
    
    survival.five.years <- five.year.values$survival
    survival.ten.years.cond <- ten.year.values$survival
    
    if (input$outcome == "Failure"){
      
      failure.five.years <- 1 - survival.five.years
      failure.ten.years <- 1 - survival.five.years*survival.ten.years.cond
      
      lowerbound.five.years <- 1 - five.year.values$upperbound
      upperbound.five.years <- 1 - five.year.values$lowerbound
      
      lowerbound.ten.years <- 1 - ten.year.values$upperbound
      upperbound.ten.years <- 1 - ten.year.values$lowerbound
      
      #pct.five.years <- paste0(round(failure.five.years*100,1),"% (CI: ", round(lowerbound.five.years*100,1),"-",round(upperbound.five.years*100,1),")")
      #pct.ten.years <- paste0(round(failure.ten.years*100,1),"% (CI: ", round(lowerbound.ten.years*100,1),"-",round(upperbound.ten.years*100,1),")")
      
      pct.five.years <- paste0(round(failure.five.years*100,1),"%")
      pct.ten.years <- paste0(round(failure.ten.years*100,1),"%")
      
      suffix.five.years <- "graft failure within 5 years"
      suffix.ten.years <- "graft failure within 10 years"
      
    } else {

      survival.ten.years <- survival.five.years*survival.ten.years.cond
      
      lowerbound.five.years <- five.year.values$lowerbound
      upperbound.five.years <- five.year.values$upperbound
      
      lowerbound.ten.years <- ten.year.values$lowerbound
      upperbound.ten.years <- ten.year.values$upperbound
      
      #pct.five.years <- paste0(round(survival.five.years*100,1),"% (CI: ", round(lowerbound.five.years*100,1),"-",round(upperbound.five.years*100,1),")")
      #pct.ten.years <- paste0(round(survival.ten.years*100,1),"% (CI: ", round(lowerbound.ten.years*100,1),"-",round(upperbound.ten.years*100,1),")")
      
      pct.five.years <- paste0(round(survival.five.years*100,1),"%")
      pct.ten.years <- paste0(round(survival.ten.years*100,1),"%")
      
      suffix.five.years <- "five-year graft survival"
      suffix.ten.years <- "ten-year graft survival"
      
    }
    
    outputText <- paste0("The recipient is estimated to have a ",pct.five.years," chance of ",suffix.five.years,", and a ",
                         pct.ten.years," chance of ",suffix.ten.years," with the comparison donor")
    
    return(outputText)
    
  })
  
  output$textComparison <- renderText({
    
    donor<-updateDonor()
    comp<-updateComparisonDonor()
    recip<-updateRecipient()
    
    survival.five.years <- survivalCalculator(T,donor,recip)$survival
    survival.five.years.comp <- survivalCalculator(T,comp,recip)$survival
    survival.ten.years.cond <- survivalCalculator(F,donor,recip)$survival
    survival.ten.years.cond.comp <- survivalCalculator(F,comp,recip)$survival
    
    survival.ten.years <- survival.five.years*survival.ten.years.cond
    survival.ten.years.comp <- survival.five.years.comp*survival.ten.years.cond.comp
    
    if (input$outcome == "Survival"){
      
      ratio.five.years <- survival.five.years/survival.five.years.comp
      ratio.ten.years <- survival.ten.years/survival.ten.years.comp
      
      if (ratio.five.years > 1){ # Original donor has higher survival
        
        improvement.five.years <- round(ratio.five.years,3)
        improvement.ten.years <- round(ratio.ten.years,3)
        
        outputText <- paste0("The original donor is preferable; the recipient is estimated to have ",improvement.five.years,"\U00D7 the 5-year graft survival probability and ",
                            improvement.ten.years,"\U00D7 the 10-year graft survival probability compared to the comparison donor")
        
      } else if (ratio.five.years < 1){ # Comparison donor has higher survival
        
        improvement.five.years <- round((1/ratio.five.years),3)
        improvement.ten.years <- round((1/ratio.ten.years),3)
        
        outputText <- paste0("The comparison donor is preferable; the recipient is estimated to have ",improvement.five.years,"\U00D7 the 5-year graft survival probability and ",
                            improvement.ten.years,"\U00D7 the 10-year graft survival probability compared to the original donor")
      } else {
        
        outputText <- paste0("Each donor provides the same graft survival probability estimates")
        
      }
    
      
    } else {
      
      failure.five.years <- 1-survival.five.years
      failure.five.years.comp <- 1-survival.five.years.comp
      
      failure.ten.years <- 1-survival.ten.years
      failure.ten.years.comp <- 1-survival.ten.years.comp
      
      ratio.five.years <- failure.five.years/failure.five.years.comp
      ratio.ten.years <- failure.ten.years/failure.ten.years.comp
      
      if (ratio.five.years < 1){ # Original donor has lower failure
        
        improvement.five.years <- round((1/ratio.five.years),3)
        improvement.ten.years <- round((1/ratio.ten.years),3)
        
        outputText <- paste0("The original donor is preferable; the recipient is estimated to have ",improvement.five.years,"\U00D7 the 5-year graft failure probability and ",
                            improvement.ten.years,"\U00D7 the 10-year graft failure probability with the comparison donor")
        
      } else if (ratio.five.years > 1){
        
        improvement.five.years <- round(ratio.five.years,2)
        improvement.ten.years <- round(ratio.ten.years,2)
        
        outputText<-paste0("The comparison donor is preferable; the recipient is estimated to have ",improvement.five.years,"\U00D7 the 5-year graft failure probability and ",
                            improvement.ten.years,"\U00D7 the 10-year graft failure probability with the original donor")
      } else {
        
        outputText<-paste0("Each donor provides the same graft failure probability estimates")
        
      }
      
    }
    
    return (outputText)
    
  })
 
})

