library(tidyverse)
library(shiny)
library(bslib)
library(viridis)
library(RColorBrewer)

ticket <- readRDS("data/ticket")
depC <- ticket |> count(Department) |> select(Department) |> arrange(Department) 
responseG <- ticket |> count(ResponseGroup) |> select(ResponseGroup) |> arrange(ResponseGroup)
type <- ticket |> count(Type) |> arrange(Type) |> select(Type)
depC <- deframe(depC)
responseG <- deframe(responseG)
type <- deframe(type)


ui <- navset_pill(
  nav_panel(title = "By Department", page_sidebar(# 1 panel#
    sidebar=sidebar(
      selectInput(
        inputId = "x0",
        label = "Choose a Department of Ticket Requestor",
        choices = depC,
        selected = "Villanova",
        multiple = T
      ),
      selectInput(
        inputId = "deptCh",
        label = "Order Requestor By:",
        choices = c("Alphabetical", "Top Requestor", "Bottom Requestor"),
        selected = "Alphabetical"
      )
      
    ),
    card(
      plotOutput(
        outputId = "barplot"
      )
    ),
    
    card(
      selectInput(
        inputId = "fillType1",
        label = "Choose the fill type of the bar chart.",
        choices = c("Response Group","Ticket Type"),
        selected = "Response Group"
      ),
      selectInput(
        inputId = "rgroup_nav1",
        label = "Choose the Response Group/Ticket Type of the fill",
        choices = c("Top 10", sort(responseG)),
        selected = "Top 10",
        multiple = T
      ),
      plotOutput(
        outputId = "barplot1"
      )
    ),
    
    card(
      plotOutput(
        outputId = "sourcePlot0"
      )
    )
  )
    
  ),#2 panel#
  
  nav_panel(title = "By Ticket Type", page_sidebar(# 2 panel#
    sidebar=sidebar(
      selectInput(
        inputId = "x1",
        label = "Choose a Type of Ticket Requestor",
        choices = type,
        selected = type[1],
        multiple = T
      ),
      selectInput(
        inputId = "ticketCH",
        label = "Choose how to order the ticket type",
        choices = c("Top Requestor","Lowest Requestor","Alphabetical"),
        selected = "Top  Requestor"
      )
    ),
    card(
      plotOutput(
        outputId = "barplot2"
      )
    ),
    
    card(selectInput(
      inputId = "rgroup_nav2",
      label = "Choose the response group/department fill for the requestor.",
      choices = c("Top 10" ,unlist(responseG)),
      selected = "Top 10",
      multiple = T
    ),
    selectInput(
      inputId = "fillType2",
      label = "Choose the fill type",
      choices = c("Response Group", "Department"),
      selected = "Response Group"
    ),
      plotOutput(
        outputId = "barplot3"
      )
    ),
    card(
      plotOutput(
        outputId = "sourcePlot1"
      )
    )
  )
  
  ),
  
  #3rd panel#
  nav_panel(
    title = "By response group",
    page_sidebar(
    sidebar = sidebar(
      selectInput(
        inputId = "x2",
        label = "Choose a response group",
        choices = responseG,
        multiple = T,
        selected = responseG[1]
      ),
      selectInput(
        inputId = "order3by",
        label = "Order Response Group By",
        choices = c("Most Tickets", "Alphabetical","Least Tickets"),
        selected = "Most Tickets"
      )
    ),
    card(
      plotOutput(
        outputId = "barplot4"
      )
    ),
    card(selectInput(
      inputId = "w0",
      label = "Choose fill type",
      choices = c("Ticket", "Department"),
      selected = "Ticket"
    ),
    selectInput(
      inputId = "fillItems",
      label = "Choose Ticket/Department to fill",
      choices = c("Top 10", unlist(type)),
      selected = "Top 10",
      multiple = T
    ),
      plotOutput(
        outputId = "barplot5"
      )
    ),
    card(plotOutput(
      outputId = "sourcePlot2"
    ))
    )
  ),
#############################
##UI nav 4: Time Series Plot#
#############################
  nav_panel(
    title = "Time Series Plotting",
    page_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "selectCategory",
          label = "Select what category of the ticket to filter by",
          choices = c("Department","Response Group","Type","Source","Incident Priority", "None"),
          selected = "None"
        )
      ),
      dateRangeInput(
        inputId = "nav4_daterange",
        label = "Select the date range of the plot",
        start = min(ticket$Created),
        end = max(ticket$Created)
      ),
      selectInput(
        inputId = "nav4_select",
        label = "Select items in category",
        choices = "None",
        multiple = T
      )
      ,
      card(
        plotOutput(
          outputId = "timeSeriesPlot1"
        ),
        plotOutput(
          outputId= "tSP2_EX"
        ),
        plotOutput(
          outputId = "tsP3_DoW"
        )
      )
    )
    
  )###end nav_panel
  
  )
  
  


###Note Source Fill bar charts are at the bottom####
server <- function(input,output,session){
  
  #reactive values
  observe({ 
    #Nav1
    updateSelectInput(inputId = "x0",
                              choices = if (input$deptCh == "Alphabetical"){
                                ticket |> count(Department) |> arrange(Department) |> select(Department)
                              }else if (input$deptCh == "Top Requestor"){
                                ticket |> count(Department) |> arrange(desc(n)) |> select(Department)
                              } else {
                                ticket |> count(Department) |> arrange(n) |> select(Department)
                                
                              })
    
    updateSelectInput(inputId = "rgroup_nav1",
                      choices = if (input$fillType1 == "Response Group"){
                        c("Top 10", responseG)
                      } else{
                        c("Top 10", type)
                      } )
    
    #Nav2 Ticket Type
    updateSelectInput(inputId = "x1",
                      choices = if (input$ticketCH == "Alphabetical"){
                        ticket |> count(Type) |> arrange(Type) |> select(Type)
                      }else if (input$ticketCH == "Top Requestor"){
                        ticket |> count(Type) |> arrange(desc(n)) |> select(Type)
                      } else {
                        ticket |> count(Type) |> arrange(n) |> select(Type)
                        
                      })
    updateSelectInput(
      inputId = "rgroup_nav2",
      choices = if (input$fillType2 == "Response Group"){
        c("Top 10", responseG)
      }else{
        c("Top 10", depC)
      }
    )
    
    #Nav 3
    updateSelectInput(inputId = "fillItems",
                      choices = if (input$w0 == "Ticket"){
                        a<-ticket |> count(Type) |> arrange(desc(n)) |> select(Type)
                        c("Top 10", a)
                      }else{
                        a<-ticket |> count(Department) |> arrange(desc(n)) |> select(Department)
                        c("Top 10", a)
                      })
    
    updateSelectInput(inputId = "x2",
                      choices = if (input$order3by == "Alphabetical"){
                        ticket |> count(ResponseGroup) |> arrange(ResponseGroup) |> select(ResponseGroup)
                      }else if (input$order3by == "Most Tickets"){
                        ticket |> count(ResponseGroup) |> arrange(desc(n)) |> select(ResponseGroup)
                      } else {
                        ticket |> count(ResponseGroup) |> arrange(n) |> select(ResponseGroup)
                      })
    
    updateSelectInput(
      inputId = "nav4_select",
      choices = if (input$selectCategory == "Department"){
        depC
      } else if (input$selectCategory == "Response Group"){
        responseG
      } else if (input$selectCategory == "Type"){
        type
      } else if (input$selectCategory == "Source"){
        ticket |> count(Source) |> arrange(Source) |> select(Source) |> deframe()
      } else if (input$selectCategory == "Incident Priority"){
        ticket |> count(SLA) |> arrange(SLA) |> select(SLA) |> deframe()
        }else{
          "None"
        }
      
    )

  })
  
  
  resG<-reactive({
    if ("Top 10" %in% input$rgroup_nav1){
      if (input$fillType1 == "Response Group"){
        a<-ticket |> count(ResponseGroup) |> arrange(desc(n)) |> select(ResponseGroup) |> slice(1:10)
      }else{
        a<-ticket |> count(Type) |> arrange(desc(n)) |> select(Type) |> slice(1:10)
      }
      unique(c(unlist(a),input$rgroup_nav1[input$rgroup_nav1 != "Top 10"]))
    }else{
      input$rgroup_nav1
    }})
    
    resG2<-reactive({
      if ("Top 10" %in% input$rgroup_nav2){
        if (input$fillType2 == "Response Group"){
          a<-ticket |> count(ResponseGroup) |> arrange(desc(n)) |> select(ResponseGroup) |> slice(1:10)
          unique(c(unlist(a),input$rgroup_nav2[input$rgroup_nav2 != "Top 10"]))
        }else{
          a<-ticket |> count(Department) |> arrange(desc(n)) |> select(Department) |> slice(1:10)
          unique(c(unlist(a),input$rgroup_nav2[input$rgroup_nav2 != "Top 10"]))
        }
        
      }else{
        input$rgroup_nav2
      }
    
  })
    
    nav3_fillp2 <-reactive({
      if (input$w0 == "Ticket"){
        if ("Top 10" %in% input$fillItems){
          a<-ticket |> count(Type) |> arrange(desc(n)) |> select(Type) |> slice(1:10)
          unique(c(unlist(a),input$fillItems[input$fillItems != "Top 10"]))
        }else{
          input$fillItems
        }
      }else{
        if ("Top 10" %in% input$fillItems){
          a<-ticket |> count(Department) |> arrange(desc(n)) |> select(Department) |> slice(1:10)
          unique(c(unlist(a),input$fillItems[input$fillItems != "Top 10"]))
        }else{
          input$fillItems
        }
      }
    })
    
    nav4_select <- reactive(
      {
        if (input$selectCategory == "Department"){
          ticket$Department
        } else if (input$selectCategory == "Response Group"){
          ticket$ResponseGroup
        } else if (input$selectCategory == "Type"){
          ticket$Type
        } else if (input$selectCategory == "Source"){
          ticket$Source
        } else{
          ticket$SLA
        }
      }
    )

  
  #########end reactive values###############
  
  output$barplot <- renderPlot({
    ticket |> filter(Department %in% input$x0) |> 
      ggplot(aes(y=reorder(Department,Department,length)))+geom_bar()+
      theme(legend.position="none")+labs(y="Department",title="Ticket count by department")
  })
  
  output$barplot1 <- renderPlot({
    if (input$fillType1 == "Response Group"){
      temp<-ticket |> filter(Department %in% input$x0) |>
        mutate(ResponseGroup = if_else(ResponseGroup %in% resG(), ResponseGroup, "Other"))
      tempu<-unique(temp$ResponseGroup)
      tempu<-append(tempu[tempu != "Other"],"Other")
      mycol <- brewer.pal(length(unique(tempu)),"Paired")
      mycol[match("Other",tempu)]<-"gray50"
      plot<-ggplot(temp,aes(y=Department,fill=ResponseGroup))+geom_bar(position = "fill")+labs(x="Percent")+scale_x_continuous(label = scales::percent)+
        scale_fill_manual(values=mycol,limits=tempu)
    } else{
      temp<-ticket |> filter(Department %in% input$x0) |>
        mutate(Type = if_else(Type %in% resG(), Type, "Other"))
      tempu<-unique(temp$Type)
      tempu<-append(tempu[tempu != "Other"],"Other")
      mycol <- brewer.pal(length(unique(tempu)),"Paired")
      mycol[match("Other",tempu)]<-"gray50"
      plot<-ggplot(temp,aes(y=Department,fill=Type))+geom_bar(position = "fill")+labs(x="Percent")+scale_x_continuous(label = scales::percent)+
        scale_fill_manual(values=mycol,limits=tempu)
    }
   
    plot
  }
  )
  
  
  #2nd nav panel by ticket type
  output$barplot2 <- renderPlot({
    ticket |> filter(Type==input$x1) |> 
      ggplot(aes(y=reorder(Type,Type,length)))+geom_bar()+
      theme(legend.position="none")+labs(y="Ticket Type")
  })
  
  output$barplot3 <- renderPlot({
    if (input$fillType2 == "Response Group"){
      temp<-ticket |> filter(Type==input$x1) |>
        mutate(ResponseGroup = if_else(ResponseGroup %in% resG2(), ResponseGroup, "Other"))
      tempu<-unique(temp$ResponseGroup)
      tempu<-append(tempu[tempu !="Other"],"Other")
      mycol <- brewer.pal(length(unique(tempu)),"Paired")
      mycol[match("Other",tempu)]<-"gray50"
      ggplot(temp,aes(y=Type,fill=ResponseGroup))+geom_bar(position = "fill")+labs(x="Percent",y="Response Group")+scale_x_continuous(label = scales::percent)+
        scale_fill_manual(values=mycol, limits=tempu)
    }else{
      temp<-ticket |> filter(Type==input$x1) |>
        mutate(Department = if_else(Department %in% resG2(), Department, "Other"))
      tempu<-unique(temp$Department)
      tempu<-append(tempu[tempu !="Other"],"Other")
      mycol <- brewer.pal(length(unique(tempu)),"Paired")
      mycol[match("Other",tempu)]<-"gray50"
      ggplot(temp,aes(y=Type,fill=Department))+geom_bar(position = "fill")+labs(x="Percent",y="Response Group")+scale_x_continuous(label = scales::percent)+
        scale_fill_manual(values=mycol, limits=tempu)
    }
     
  }
  )
  
  #3rd nav panel by Response Group
  output$barplot4 <- renderPlot({
    ticket |> filter(ResponseGroup %in% input$x2) |> 
      ggplot(aes(y=reorder(ResponseGroup, ResponseGroup, length)))+geom_bar()+
      theme(legend.position="none")+labs(y="Response Group")
  })
  
  
  output$barplot5 <- renderPlot(
    {
      if (input$w0=="Ticket"){
        temp <- ticket |> filter(ResponseGroup == input$x2) |> 
        mutate(Type = if_else(Type %in% nav3_fillp2(), Type, "Other"))
        tempu <- unique(temp$Type)
        tempu<- append(tempu[tempu!="Other"],"Other")
        mycol <- brewer.pal(length(unique(tempu)),"Paired")
        mycol[match("Other",tempu)]<-"gray50"
        plot0 <-ggplot(temp,aes(y=reorder(ResponseGroup,ResponseGroup,length),fill=Type))+labs(y="Response Group")+
          scale_fill_manual(values = mycol, limits = tempu)
      } else { #Fill Department
        temp <- ticket |>  filter(ResponseGroup == input$x2) |> 
          mutate(Department = if_else(Department %in% nav3_fillp2(),Department,"Other"))
        tempu <- unique(temp$Department)
        tempu<- append(tempu[tempu!="Other"],"Other")
        mycol <- brewer.pal(length(unique(tempu)),"Paired")
        mycol[match("Other",tempu)]<-"gray50"
        plot0 <-ggplot(temp,aes(y=reorder(ResponseGroup,ResponseGroup, length),fill=Department))+labs(y="Response Group")+
          scale_fill_manual(values = mycol, limits = tempu)
      }
      
      plot0+geom_bar(position = "fill")+labs(x="Percent")+scale_x_continuous(label = scales::percent)
    }
  )
  
  #source plots
  output$sourcePlot0 <- renderPlot({
    ticket |> filter(Department==input$x0) |> 
      ggplot(aes(y=Department,fill = Source))+geom_bar(position = "fill")+labs(x="Percent")+scale_x_continuous(label = scales::percent)
  })
  
  output$sourcePlot1 <- renderPlot({
    ticket |> filter(Type==input$x1) |> 
      ggplot(aes(y=Type,fill = Source))+geom_bar(position = "fill")+labs(x="Percent")+scale_x_continuous(label = scales::percent)
  })
  
  output$sourcePlot2 <- renderPlot({
    ticket |> filter(ResponseGroup==input$x2) |> 
      ggplot(aes(y=ResponseGroup,fill = Source))+geom_bar(position = "fill")+labs(x="Percent")+scale_x_continuous(label = scales::percent)
  })
  
###############################  
#Nav Panel 4: Time Series Plot#
###############################
  output$timeSeriesPlot1 <- renderPlot({
    if (input$selectCategory == "Department") {
      temp <- ticket |> filter(Department == input$nav4_select)
      
    } else if (input$selectCategory == "Response Group") {
      temp <- ticket |> filter(ResponseGroup == input$nav4_select)
      
    } else if (input$selectCategory == "Type") {
      temp <- ticket |> filter(Type == input$nav4_select)
      
    } else if (input$selectCategory == "Source") {
      temp <- ticket |> filter(Source == input$nav4_select)
      
    } else if (input$selectCategory == "Incident Priority") {
      temp <- ticket |> filter(SLA == input$nav4_select)
      
    } else {
      temp <- ticket
    }
    
    
    temp |> filter(Created >= input$nav4_daterange[1] & Created <=input$nav4_daterange[2])|> group_by(year_Created,month_Created,day_Created) |>
      summarize(number_of_tickets=n()) |> mutate(date = mdy(paste(month_Created,day_Created,year_Created,sep="-"))) |> 
      ggplot(aes(x=date,y=number_of_tickets))+geom_point()+labs(y="Number of Tickets",title="Time series plot of tickets created",x="date")+
      scale_y_continuous(breaks=seq(0,700,by=50))+geom_smooth(method="loess",se=F,span=0.2)
  })
  
  
  output$tSP2_EX <-renderPlot({
    if (input$selectCategory == "Department") {
      temp <- ticket |> filter(Department == input$nav4_select)
      
    } else if (input$selectCategory == "Response Group") {
      temp <- ticket |> filter(ResponseGroup == input$nav4_select)
      
    } else if (input$selectCategory == "Type") {
      temp <- ticket |> filter(Type == input$nav4_select)
      
    } else if (input$selectCategory == "Source") {
      temp <- ticket |> filter(Source == input$nav4_select)
      
    } else if (input$selectCategory == "Incident Priority") {
      temp <- ticket |> filter(SLA == input$nav4_select)
      
    } else {
      temp <- ticket
    }
    
    temp |> filter(Created >= input$nav4_daterange[1] & Created <=input$nav4_daterange[2])|>
      mutate(date = mdy_h(paste(month_Created,day_Created,year_Created,hour(Created),sep="-"))) |> 
      group_by(date) |> count() |> 
      ungroup() |> mutate(dayOfWeek = weekdays(date)) |> 
      group_by(dayOfWeek) |> summarize(avg_n = mean(n),sd=sd(n))|> ungroup() |> 
      ggplot(aes(x=factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday",
                                                "Thursday", "Friday", "Saturday",
                                                "Sunday")),y=avg_n))+
      geom_point(size=4)+
      geom_errorbar(aes(ymin=avg_n-sd,ymax=avg_n+sd))+
      labs(x="Day of Week",y="Ticket Count")
    
  })
  
  output$tsP3_DoW <- renderPlot({
    if (input$selectCategory == "Department") {
      temp <- ticket |> filter(Department == input$nav4_select)
      
    } else if (input$selectCategory == "Response Group") {
      temp <- ticket |> filter(ResponseGroup == input$nav4_select)
      
    } else if (input$selectCategory == "Type") {
      temp <- ticket |> filter(Type == input$nav4_select)
      
    } else if (input$selectCategory == "Source") {
      temp <- ticket |> filter(Source == input$nav4_select)
      
    } else if (input$selectCategory == "Incident Priority") {
      temp <- ticket |> filter(SLA == input$nav4_select)
      
    } else {
      temp <- ticket
    }
    
    temp |> filter(Created >= input$nav4_daterange[1] & Created <=input$nav4_daterange[2])|>
      mutate(date = mdy_h(paste(month_Created,day_Created,year_Created,hour(Created),sep="-"))) |> 
      group_by(date) |> count() |> 
      ungroup() |> mutate(dayOfWeek = weekdays(date),hour=hour(date)) |> 
      group_by(dayOfWeek,hour) |> summarize(avg_n=mean(n)) |> ungroup() |> 
      ggplot(aes(x=hour,y=avg_n))+
      labs(x="Hour of Day",y="Average Ticket Count")+
      facet_wrap(~factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday", "Saturday",
                                               "Sunday")))+
      geom_smooth(method="loess",span = 0.25,alpha=0.5)+geom_point()
    
  })

  
}

shinyApp(ui = ui, server= server)

