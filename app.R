library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
# rm(list =ls())
setwd("/Users/mohinivembu/Documents/mutual-funds-and-etfs")

source("Analysis Set Up.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Mutual Fund Peer Comparison"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
         selectizeInput("fund", 
                     "Mutual Fund to Analyze:",
                     choices = NULL,
                     selected = NULL)
                  ),
      mainPanel(
        tabsetPanel(type = c("pills"),
                    tabPanel("Asset Allocation", 
                             tabsetPanel(type = c("pills"),
                               tabPanel("Plot",
                                        fluidRow(
                                          plotlyOutput("alloc_plot1"),
                                          plotlyOutput("alloc_plot2")
                                        )
                                      ),
                               tabPanel("Peer Table",
                                        fluidRow(
                                          dataTableOutput("alloc_tbl", width = "500px")  
                                        )
                                      )
                             )
                    ),
                    tabPanel("Performance",
                             tabsetPanel(type = c("pills"),
                                         tabPanel("Spaghetti",
                                                  plotOutput("perf_plot")),
                                         tabPanel("Summary Table",
                                                  dataTableOutput("perf_tbl"))
                             )
                    ),
                    tabPanel("Expenses",
                             tabsetPanel(type = c("pills"),
                                         tabPanel("Table",
                                                  dataTableOutput("fee_tbl"))
                             )
                    )
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ### Add Names for the Fund Choices
  updateSelectizeInput(session, 'fund',
                       choices = sort(unique(alloc$port_name)),
                       server = TRUE)
   ## Asset Allocation Plot
   output$alloc_plot1 <- renderPlotly({
     ### Asset Allocation Calculations
     ai_chars <- alloc %>%
       filter(port_name == input$fund) %>% 
       arrange(alloc_cat_grp,alloc_cat, desc(percent)) %>% 
       distinct(alloc_cat_grp,alloc_cat, .keep_all = T)
     peers <-ai_chars %>%
       left_join(alloc%>%
                   select(alloc_cat_grp,port_name, fund_extended_name, size, category,investment, fund_family,net_assets, percent,
                          alloc_cat, port_name, currency),
                 by = c("size", "category", "investment","alloc_cat", "currency","alloc_cat_grp"),
                 suffix = c("", "_peer")) %>%
       filter(fund_family != fund_family_peer) %>%
       arrange(port_name_peer, alloc_cat_grp,alloc_cat, desc(percent_peer)) %>%
       distinct(port_name_peer, alloc_cat_grp,alloc_cat, .keep_all = T)
     peers_avg <- peers %>%
       mutate(grp = "Peer Group",
              total_peers = n_distinct(port_name_peer)) %>%
       group_by(alloc_cat_grp,alloc_cat, grp,total_peers) %>%
       summarise(peer_cat = sum(percent_peer, na.rm = T)) %>%
       mutate(percent_peer = peer_cat/total_peers)
     df <- ai_chars %>%
       select(port_name, alloc_cat_grp,alloc_cat, percent) %>%
       mutate(grp = port_name) %>%
       bind_rows(peers_avg %>%
                   rename(percent = percent_peer)) %>% 
       rename(Category = alloc_cat,
              Percent = percent,
              `Fund Group` = grp)
     p <- ggplot(data = df %>% 
                   filter(alloc_cat_grp == "Industry"), mapping = aes(Category, Percent, fill = `Fund Group`)) +
       geom_bar(stat = "identity", position = "dodge") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                        size = 12, face = "bold")) +
       theme(axis.text.y = element_text(size = 12, face = "bold")) +
       theme(axis.title.x = element_text(size = 12, face = "bold")) +
       theme(axis.title.y = element_text(size = 12, face = "bold")) +
       theme(plot.title = element_text(size = 16, face = "bold")) +
       theme(legend.title = element_text(size = 12, face ="bold")) +
       labs(title="Industry Asset Allocation Comparison Against Peers",
            x ="Industry", y = "Percent Allocation", fill = "Fund Group")
     ggplotly(p)
   })
   output$alloc_plot2<- renderPlotly({
     ### Asset Allocation Calculations
     ai_chars <- alloc %>%
       filter(port_name == input$fund) %>% 
       arrange(alloc_cat_grp,alloc_cat, desc(percent)) %>% 
       distinct(alloc_cat_grp,alloc_cat, .keep_all = T)
     peers <-ai_chars %>%
       left_join(alloc%>%
                   select(alloc_cat_grp,port_name, fund_extended_name, size, category,investment, fund_family,net_assets, percent,
                          alloc_cat, port_name, currency),
                 by = c("size", "category", "investment","alloc_cat", "currency","alloc_cat_grp"),
                 suffix = c("", "_peer")) %>%
       filter(fund_family != fund_family_peer) %>%
       arrange(port_name_peer, alloc_cat_grp,alloc_cat, desc(percent_peer)) %>%
       distinct(port_name_peer, alloc_cat_grp,alloc_cat, .keep_all = T)
     peers_avg <- peers %>%
       mutate(grp = "Peer Group",
              total_peers = n_distinct(port_name_peer)) %>%
       group_by(alloc_cat_grp,alloc_cat, grp,total_peers) %>%
       summarise(peer_cat = sum(percent_peer, na.rm = T)) %>%
       mutate(percent_peer = peer_cat/total_peers)
     df <- ai_chars %>%
       select(port_name, alloc_cat_grp,alloc_cat, percent) %>%
       mutate(grp = port_name) %>%
       bind_rows(peers_avg %>%
                   rename(percent = percent_peer)) %>% 
       rename(Category = alloc_cat,
              Percent = percent,
              `Fund Group` = grp)
     p <- ggplot(data = df %>% 
                   filter(alloc_cat_grp == "Asset Class"), mapping = aes(Category, Percent, fill = `Fund Group`)) +
       geom_bar(stat = "identity", position = "dodge") +
       theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                        size = 12, face = "bold")) +
       theme(axis.text.y = element_text(size = 12, face = "bold")) +
       theme(axis.title.x = element_text(size = 12, face = "bold")) +
       theme(axis.title.y = element_text(size = 12, face = "bold")) +
       theme(plot.title = element_text(size = 16, face = "bold")) +
       theme(legend.title = element_text(size = 12, face ="bold")) +
       labs(title="Asset Class Allocation Comparison Against Peers",
            x ="Asset Class", y = "Percent Allocation", fill = "Fund Group")
     ggplotly(p)
   })
   ## Asset Allocation Table
   output$alloc_tbl <- renderDataTable({
     ai_chars <- alloc %>%
       filter(port_name == input$fund) %>% 
       arrange(alloc_cat_grp,alloc_cat, desc(percent)) %>% 
       distinct(alloc_cat_grp,alloc_cat, .keep_all = T)
     peers <-ai_chars %>%
       left_join(alloc%>%
                   select(port_name, fund_extended_name, size, category,investment, fund_family,net_assets, percent,
                          alloc_cat,alloc_cat_grp, port_name, currency),
                 by = c("size", "category", "investment","currency","alloc_cat_grp", "alloc_cat"),
                 suffix = c("", "_peer")) %>%
       filter(fund_family != fund_family_peer) %>%
       arrange(port_name_peer, alloc_cat_grp,alloc_cat, desc(percent_peer)) %>%
       distinct(port_name_peer, alloc_cat_grp,alloc_cat, .keep_all = T)
     tbl_setup <- peers %>%
       select(alloc_cat,port_name_peer,percent_peer) %>%
       rename(`Peer Fund` = port_name_peer) %>% 
       distinct() %>%
       mutate(percent_peer = percent_peer/100) %>% 
       spread(alloc_cat, percent_peer)
     datatable(tbl_setup, options = list(columnDefs = list(list(className='dt-center', targets = 0:ncol(tbl_setup))))) %>% 
       formatPercentage(2:ncol(tbl_setup),digits = 1)
   })
   ### Performance Spaghetti
   output$perf_plot <- renderPlot({
     ai_rets <- spaghetti_rets %>% 
       ungroup() %>% 
       filter(port_name == input$fund) %>% 
       arrange(port_name, date, desc(value)) %>% 
       distinct(port_name, date, .keep_all = T)
     peer_rets <- ai_rets %>% 
       left_join(spaghetti_rets %>% ungroup(), by =
                 c("start_year", "size", "investment", "category", "currency","date"),
                 suffix = c("", "_peer")) %>% 
       arrange(port_name_peer, date, desc(value_peer)) %>% 
       filter(fund_family != fund_family_peer) %>% 
       distinct(port_name_peer, date, .keep_all = T) %>% 
       select(port_name_peer,date, value_peer) %>% 
       rename(port_name = port_name_peer,
              value = value_peer) %>% 
       spread(port_name, value) 
    peer_funds <- ncol(peer_rets) - 1
    for (i in 1:peer_funds){
      names(peer_rets)[i+1] <- paste0("Peer Fund ", i)
    }
     df <- ai_rets %>% 
       select(port_name, date, value) %>% 
       spread(port_name, value) %>% 
       left_join(peer_rets)
     names(df)[2] <- "AI"
     plt <-ggplot(df, mapping = aes(x = date, y = AI)) +
       geom_line(colour = "blue") +
       labs(title = "Cumulative Value of $10000",
            x = "Year",
            y = "Value ($)")
     for(i in 1:peer_funds){
       temp <- data.frame(df[,c(1,i+2)])
       names(temp)[2] <- "Value"
       plt <- plt +
         geom_line(data = temp,aes(x = date, y = Value), colour = "grey")
     }
     plt +
       theme(axis.text.x = element_text(size = 12, face = "bold")) +
       theme(axis.text.y = element_text(size = 12, face = "bold")) +
       theme(axis.title.x = element_text(size = 12, face = "bold")) +
       theme(axis.title.y = element_text(size = 12, face = "bold")) +
       theme(plot.title = element_text(size = 16, face = "bold")) 
   })
   ### Performance Table
   output$perf_tbl <- renderDataTable({
     ai_rets <- returns_tbl %>% 
       filter(port_name == input$fund) 
     peer_rets <- ai_rets %>% 
       left_join(returns_tbl, by =
                   c("period", "end_date","start_date","start_year", "end_year",
                     "size", "investment", "category", "currency"),
                 suffix = c("", "_peer")) %>% 
       filter(fund_family != fund_family_peer)  %>% 
       select(port_name_peer,start_date,end_date, return_peer, return) %>% 
       mutate(fund_grp = "Peer")
     peer_summary1 <- peer_rets %>% 
       group_by(start_date, end_date) %>% 
       summarise(n_peers_worse = sum(return_peer <= return))
     peer_summary2 <- peer_rets %>% 
       group_by(start_date, end_date,fund_grp, return) %>% 
       summarise(n_peers = n_distinct(port_name_peer),
                 avg_peer_ret = mean(return_peer, na.rm = T),
                 med_peer_ret = median(return_peer, na.rm = T)) %>% 
       ungroup() %>% 
       mutate(n_days = end_date - start_date + 1,
         time_period = case_when(abs((365 - n_days)) <10 ~ "1-Year Return",
                                      abs((365*3 - n_days)) <10 ~ "3-Year Return",
                                      abs((365*5 - n_days)) <10 ~ "5-Year Return",
                                      abs((365*10 - n_days)) <10 ~ "10-Year Return")) %>% 
       left_join(peer_summary1) %>% 
       select(time_period,return,avg_peer_ret, med_peer_ret, n_peers_worse, n_peers) %>% 
       mutate(avg_peer_ret = paste0(round(avg_peer_ret,1), "%"),
              med_peer_ret = paste0(round(med_peer_ret,1), "%"),
              return = paste0(round(return,1), "%")) %>% 
       rename(`Time Period` = time_period,
              `Fund Return` = return,
              `Number of Peers` = n_peers,
              `Average Peer Return` = avg_peer_ret,
              `Median Peer Return` = med_peer_ret,
              `Performed Better Than` = n_peers_worse)
     datatable(peer_summary2,
               options = list(columnDefs = list(list(className  = 'dt-center', targets = 0:6))))
   })
   ### Expenses Table
   output$fee_tbl <- renderDataTable({
     ai_fee <- fees %>% 
       filter(port_name == input$fund) %>% 
       arrange(port_name, net_annual_expense_ratio_fund) %>% 
       distinct(port_name, .keep_all = T)
     peer_fees  <- ai_fee %>% 
       left_join(fees, by = c("size", "investment", "currency", "category"),
                 suffix = c("", "_peer")) %>% 
       filter(fund_family != fund_family_peer) %>% 
       arrange(port_name_peer, net_annual_expense_ratio_fund_peer) %>% 
       distinct(port_name_peer, .keep_all = T) %>% 
       select(port_name, net_annual_expense_ratio_fund, net_annual_expense_ratio_fund_peer)
     fee_summary <- peer_fees %>% 
       group_by(port_name, net_annual_expense_ratio_fund) %>% 
       summarise(avg_fee_peer = paste0(round(mean(net_annual_expense_ratio_fund_peer, na.rm = T),1), "%"),
                 med_fee_peer = paste0(round(mean(net_annual_expense_ratio_fund_peer, na.rm = T),1), "%"),
                 n_peer_worse = paste0(sum(net_annual_expense_ratio_fund < net_annual_expense_ratio_fund_peer)),
                 n_peers = paste0(n())) %>% 
       mutate(Year = 2018,
              net_annual_expense_ratio_fund = paste0(format(round(net_annual_expense_ratio_fund,1),nsmall = 1),"%")) %>% 
       rename(`Fund Net Expense Ratio` = net_annual_expense_ratio_fund,
              `Number of Peers` = n_peers,
              `Average Peer Expense Ratio` = avg_fee_peer,
              `Median Peer Expense Ratio` = med_fee_peer,
              `Cheaper Than` = n_peer_worse,
              `Fund Name` = port_name) %>% 
       gather(Statistic, Value, -c(`Fund Name`, Year))
     datatable(fee_summary,
               options = list(columnDefs = list(list(className  = 'dt-center', targets = 1:4))))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

