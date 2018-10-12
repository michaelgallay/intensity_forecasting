
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

library(ggplot2)

library(gbm)

load("models.rda") #models for blended teams - current models are using few parameters ("Agent_Hours", "Daily_Download", "Intensity", "AHT", "Ready.Connection",     "Ready_Time","NotReady"), some of them won't be available for modification to the user

#seasonality from text file (to update everymonth


vol_season <-  unlist(read.table("season.txt"))

vol <- 10

aht <- 111

compliance <- 0.95

help <- 0.07

shrink_me <- 0.33

shrink_mt <- 0.45

staff_me <- 41.7

staff_mt <- 25.6

ntr_me <- 0

#test git

ntr_mt <- 0

net_me <- staff_me - ntr_me

net_mt <- staff_mt - ntr_mt

net_shrink_staff <- net_me*(1-shrink_me) + net_mt*(1-shrink_mt)


net_comp_staff <- net_shrink_staff*compliance

net_help_staff <- net_comp_staff*(1-help)

ready <- 17.2

connect <- net_help_staff/1.03*3600*37.5/(aht+ready)/vol

intensity <- 175/22*connect

server <- function(input, output, session) {
    
    # Reactive expression to create MXELO intensity forecast
    elo_built <- reactive({
        
        #inputs from app
        
        vol <- input$volume_elo
        
        aht <- input$aht_elo
        
        shrink_me <- input$shrink_me
        
        shrink_mt <- input$shrink_mt
        
        staff_me <- input$staff_me
        
        staff_mt <- input$staff_mt
        
        ntr_me <- input$ntr_me
        
        ntr_mt <- input$ntr_mt
        
        #calculated inputs
        
        
        net_me <- staff_me - ntr_me
        
        net_mt <- staff_mt - ntr_mt
        
        
        net_shrink_staff <- net_me*(1-shrink_me/100) + net_mt*(1-shrink_mt/100)
        
        
        net_comp_staff <- net_shrink_staff*compliance
        
        net_help_staff <- net_comp_staff*(1-help)
        
        
        connect <- net_help_staff/1.03*3600*37.5/(aht+ready)/vol
        
        intensity_jul <- 175/22*connect
        
        
        # volume input from seasonal factors tab
        
        m_1 <- input$elo_1
        m_2 <- input$elo_2
        m_3 <- input$elo_3
        m_4 <- input$elo_4
        m_5 <- input$elo_5
        
        vol_season <-  c(m_1, m_2, m_3, m_4, m_5)
        
        
        #future volume
        
        
        fut_vol <-  vol
        
        for(i in 1:length(vol_season)){
            fut_vol[i+1] <- fut_vol[i]*vol_season[i]
        }
        
        fut_vol <- round(fut_vol, 0)
        #staff for future month will considere productive hence not remove from staff
        
        
        fnet_shrink_staff <- staff_me*(1-shrink_me/100) + staff_mt*(1-shrink_mt/100)
        
        fnet_comp_staff <- fnet_shrink_staff*compliance
        
        fnet_help_staff <- fnet_comp_staff*(1-help)
        
        fconnect <- fnet_help_staff/1.03*3600*37.5/(aht+ready)/fut_vol
        
        fintensity <- 175/22*fconnect
        
        fintensity[1] <- intensity_jul
        
        fram = as.data.frame(fintensity*100)
        
        fram$month = c("m", "m+1", "m+2", "m+3", "m+4", "m+5")
        
        fram$month <-factor(fram$month, 
                            levels = c("m", "m+1", "m+2", "m+3", "m+4", "m+5"
                                       ))
        
        fram$volume <- fut_vol
        
        fram <- fram[,c(2,3,1)]
        
        names(fram) <- c("month", "volume", "intensity")
        
        fram$volume <- as.character(fram$volume)
        
        #intensity calculator for future month only based on change in volume
        #need to give a table + and reative hist + staff has to be current staff + NTR for each sites
        
        
        
        
        return(fram)
    })
    
    
    
    #table outpu MXELO
    
    

    output$values <- renderTable({elo_built()})
    
    
    
    # plot output MXELO
    
    output$elo_plot <- renderPlot({
        
        fram <- elo_built()
        
        c4 <- c("current", "fut", "fut", "fut", "fut", "fut")
        
        fram = cbind(fram, c4)
        
        p <- ggplot(fram, aes(x = month, y = as.numeric(intensity), fill = c4)) + scale_y_continuous(limits=c(0, 190))
        
        p <- p + geom_bar(stat = "identity", colour = "black") + ggtitle("MXELO Intensity") + xlab("Month") + ylab("Intensity") 
        
        #+ theme(plot.title = element_text(size = rel(2), face = "bold"))
        
        p <- p + scale_fill_manual("legend", values = c("current" = "deepskyblue2", "fut" = "dodgerblue2"))
        
        p + geom_text(data = fram, aes(x = month, y = intensity/5, label = paste0(round(intensity, 1), "%")), size = 6, colour = "white")
        
        
          })
    
    
    
    ###############need to build output for blended lobs
    
    emb_built <- reactive({
        
        vol_emb <- input$volume_emb
        aht_emb <- input$aht_emb
        ah_emb <- input$ah_emb
        
        ready_emb <- 23.42
        not_ready_emb <- 13
        read_pc_emb <- 17.2
        intensity <- 1
        
        # volume input from seasonal factors tab
        
        emb_1 <- input$emb_1
        emb_2 <- input$emb_2
        emb_3 <- input$emb_3
        emb_4 <- input$emb_4
        emb_5 <- input$emb_5
        
        vol_season_emb <-  c(emb_1, emb_2, emb_3, emb_4, emb_5)
        
        
        #future volume
        
        
        fut_embvol <-  vol_emb
        
        for(i in 1:length(vol_season_emb)){
            fut_embvol[i+1] <- fut_embvol[i]*vol_season_emb[i]
        }
        
        fut_embvol <- round(fut_embvol, 0)
        
        
        dat_emb <- data.frame()
        
        dat_emb <- c(ah_emb, vol_emb, 1, aht_emb, 11.2, ready_emb, not_ready_emb)
        
        
        
        
        
        names(dat_emb) <- c("Agent_Hours", "Daily_Download", "Intensity", "AHT", "Ready.Connection","Ready_Time","NotReady")
        
        dat_emb <- as.data.frame(t(dat_emb))
        
        dat_emb[1:6, ] <- dat_emb[1, ]
        
        dat_emb[, 2] <- fut_embvol
        
        
        
        
        pred <- predict(gbm_emb, newdata= (dat_emb), n.trees = 10000)
        
        
        fram_emb <- as.data.frame(pred*100)
        
        
        
        fram_emb$month <-  c("m", "m+1", "m+2", "m+3", "m+4", "m+5")
        
        fram_emb$month <-factor(fram_emb$month, 
                            levels = c("m", "m+1", "m+2", "m+3", "m+4", "m+5"
                            ))
        
        fram_emb$volume <- dat_emb$Daily_Download
        
        fram_emb <- fram_emb[,c(2,3,1)]
        
        names(fram_emb) <- c("month", "volume", "intensity")
        
        fram_emb$volume <- as.character(fram_emb$volume)
        
        
        
        return(fram_emb)
        
        
        
        
        
        })
    
    
    #output of data table
    
    output$emb_pred <- renderTable({emb_built()})
    
    
    # plot output MXEMB
    
    output$emb_plot <- renderPlot({
        
        fram <- emb_built()
        
        c4 <- c("current", "fut", "fut", "fut", "fut", "fut")
        
        fram = cbind(fram, c4)
        
        p <- ggplot(fram, aes(x = month, y = as.numeric(intensity), fill = c4)) + scale_y_continuous(limits=c(0, 190))
        
        p <- p + geom_bar(stat = "identity", colour = "black") + ggtitle("MXEMB Intensity") + xlab("Month") + ylab("Intensity") 
        
        #+ theme(plot.title = element_text(size = rel(2), face = "bold"))
        
        p <- p + scale_fill_manual("legend", values = c("current" = "deepskyblue2", "fut" = "dodgerblue2"))
        
        p + geom_text(data = fram, aes(x = month, y = intensity/5, label = paste0(round(intensity, 1), "%")), size = 6, colour = "white")
        
        
    })
 
    ########################## EHB #######################   
    
    ehb_built <- reactive({
        
        vol_ehb <- input$volume_ehb
        aht_ehb <- input$aht_ehb
        ah_ehb <- input$ah_ehb
        
        ready_ehb <- 7.8
        not_ready_ehb <- 5
        read_pc_ehb <- 11.3
        intensity <- 1
        
        # volume input from seasonal factors tab
        
        ehb_1 <- input$ehb_1
        ehb_2 <- input$ehb_2
        ehb_3 <- input$ehb_3
        ehb_4 <- input$ehb_4
        ehb_5 <- input$ehb_5
        
        vol_season_ehb <-  c(ehb_1, ehb_2, ehb_3, ehb_4, ehb_5)
        
        
        #future volume
        
        
        fut_ehbvol <-  vol_ehb
        
        for(i in 1:length(vol_season_ehb)){
            fut_ehbvol[i+1] <- fut_ehbvol[i]*vol_season_ehb[i]
        }
        
        fut_ehbvol <- round(fut_ehbvol, 0)
        
        
        dat_ehb <- data.frame()
        
        dat_ehb <- c(ah_ehb, vol_ehb, 1, aht_ehb, 11.2, ready_ehb, not_ready_ehb)
        
        
        
        
        
        names(dat_ehb) <- c("Agent_Hours", "Daily_Download", "Intensity", "AHT", "Ready.Connection","Ready_Time","NotReady")
        
        dat_ehb <- as.data.frame(t(dat_ehb))
        
        dat_ehb[1:6, ] <- dat_ehb[1, ]
        
        dat_ehb[, 2] <- fut_ehbvol
        
        
        
        
        pred <- predict(gbm_ehb, newdata= (dat_ehb), n.trees = 10000)
        
        
        fram_ehb <- as.data.frame(pred*100)
        
        
        
        fram_ehb$month <-  c("m", "m+1", "m+2", "m+3", "m+4", "m+5")
        
        fram_ehb$month <-factor(fram_ehb$month, 
                                levels = c("m", "m+1", "m+2", "m+3", "m+4", "m+5"
                                ))
        
        fram_ehb$volume <- dat_ehb$Daily_Download
        
        fram_ehb <- fram_ehb[,c(2,3,1)]
        
        names(fram_ehb) <- c("month", "volume", "intensity")
        
        fram_ehb$volume <- as.character(fram_ehb$volume)
        
        return(fram_ehb)
        
        
        
        
        
    })
    
    
    #output of data table
    
    output$ehb_pred <- renderTable({ehb_built()})
    
    
    # plot output MXEHB
    
    output$ehb_plot <- renderPlot({
        
        fram <- ehb_built()
        
        c4 <- c("current", "fut", "fut", "fut", "fut", "fut")
        
        fram = cbind(fram, c4)
        
        p <- ggplot(fram, aes(x = month, y = as.numeric(intensity), fill = c4)) + scale_y_continuous(limits=c(0, 190))
        
        p <- p + geom_bar(stat = "identity", colour = "black") + ggtitle("MXEHB Intensity") + xlab("Month") + ylab("Intensity") 
        
        #+ theme(plot.title = element_text(size = rel(2), face = "bold"))
        
        p <- p + scale_fill_manual("legend", values = c("current" = "deepskyblue2", "fut" = "dodgerblue2"))
        
        p + geom_text(data = fram, aes(x = month, y = intensity/5, label = paste0(round(intensity, 1), "%")), size = 6, colour = "white")
        
        
    })
    
    
    ########################## LLB #######################   
    
    llb_built <- reactive({
        
        vol_llb <- input$volume_llb
        aht_llb <- input$aht_llb
        ah_llb <- input$ah_llb
        
        ready_llb <- 37
        not_ready_llb <- 20
        read_pc_llb <- 15
        intensity <- 1
        
        # volume input from seasonal factors tab
        
        llb_1 <- input$llb_1
        llb_2 <- input$llb_2
        llb_3 <- input$llb_3
        llb_4 <- input$llb_4
        llb_5 <- input$llb_5
        
        vol_season_llb <-  c(llb_1, llb_2, llb_3, llb_4, llb_5)
        
        
        #future volume
        
        
        fut_llbvol <-  vol_llb
        
        for(i in 1:length(vol_season_llb)){
            fut_llbvol[i+1] <- fut_llbvol[i]*vol_season_llb[i]
        }
        
        fut_llbvol <- round(fut_llbvol, 0)
        
        
        dat_llb <- data.frame()
        
        dat_llb <- c(ah_llb, vol_llb, 1, aht_llb, 16, ready_llb, not_ready_llb)
        
        
        
        
        
        names(dat_llb) <- c("Agent_Hours", "Daily_Download", "Intensity", "AHT", "Ready.Connection","Ready_Time","NotReady")
        
        dat_llb <- as.data.frame(t(dat_llb))
        
        dat_llb[1:6, ] <- dat_llb[1, ]
        
        dat_llb[, 2] <- fut_llbvol
        
        
        
        
        pred <- predict(gbm_llb, newdata= (dat_llb), n.trees = 10000)
        
        
        fram_llb <- as.data.frame((pred+0.1)*100)
        
        
        
        fram_llb$month <-  c("m", "m+1", "m+2", "m+3", "m+4", "m+5")
        
        fram_llb$month <-factor(fram_llb$month, 
                                levels = c("m", "m+1", "m+2", "m+3", "m+4", "m+5"
                                ))
        
        fram_llb$volume <- dat_llb$Daily_Download
        
        fram_llb <- fram_llb[,c(2,3,1)]
        
        names(fram_llb) <- c("month", "volume", "intensity")
        
        fram_llb$volume <- as.character(fram_llb$volume)
        
        
        
        return(fram_llb)
        
        
        
        
        
    })
    
    
    #output of data table
    
    output$llb_pred <- renderTable({llb_built()})
    
    
    # plot output MXLLB
    
    output$llb_plot <- renderPlot({
        
        fram <- llb_built()
        
        c4 <- c("current", "fut", "fut", "fut", "fut", "fut")
        
        fram = cbind(fram, c4)
        
        p <- ggplot(fram, aes(x = month, y = as.numeric(intensity), fill = c4)) + scale_y_continuous(limits=c(0, 190))
        
        p <- p + geom_bar(stat = "identity", colour = "black") + ggtitle("MXLLB Intensity") + xlab("Month") + ylab("Intensity") 
        
        #+ theme(plot.title = element_text(size = rel(2), face = "bold"))
        
        p <- p + scale_fill_manual("legend", values = c("current" = "deepskyblue2", "fut" = "dodgerblue2"))
        
        p + geom_text(data = fram, aes(x = month, y = intensity/5, label = paste0(round(intensity, 1), "%")), size = 6, colour = "white")
        
        
    })
    
    
    
    
    
    ########################## LMB #######################   
    
    lmb_built <- reactive({
        
        vol_lmb <- input$volume_lmb
        aht_lmb <- input$aht_lmb
        ah_lmb <- input$ah_lmb
        
        ready_lmb <- 24
        not_ready_lmb <- 13
        read_pc_lmb <- 14
        intensity <- 1
        
        # volume input from seasonal factors tab
        
        lmb_1 <- input$lmb_1
        lmb_2 <- input$lmb_2
        lmb_3 <- input$lmb_3
        lmb_4 <- input$lmb_4
        lmb_5 <- input$lmb_5
        
        vol_season_lmb <-  c(lmb_1, lmb_2, lmb_3, lmb_4, lmb_5)
        
        
        #future volume
        
        
        fut_lmbvol <-  vol_lmb
        
        for(i in 1:length(vol_season_lmb)){
            fut_lmbvol[i+1] <- fut_lmbvol[i]*vol_season_lmb[i]
        }
        
        fut_lmbvol <- round(fut_lmbvol, 0)
        
        
        dat_lmb <- data.frame()
        
        dat_lmb <- c(ah_lmb, vol_lmb, 1, aht_lmb, 13.2, ready_lmb, not_ready_lmb)
        
        
        
        
        
        names(dat_lmb) <- c("Agent_Hours", "Daily_Download", "Intensity", "AHT", "Ready.Connection","Ready_Time","NotReady")
        
        dat_lmb <- as.data.frame(t(dat_lmb))
        
        dat_lmb[1:6, ] <- dat_lmb[1, ]
        
        dat_lmb[, 2] <- fut_lmbvol
        
        
        
        
        pred <- predict(gbm_lmb, newdata= (dat_lmb), n.trees = 10000)
        
        
        fram_lmb <- as.data.frame((pred-0.2)*100)
        
        
        
        fram_lmb$month <-  c("m", "m+1", "m+2", "m+3", "m+4", "m+5")
        
        fram_lmb$month <-factor(fram_lmb$month, 
                                levels = c("m", "m+1", "m+2", "m+3", "m+4", "m+5"
                                ))
        
        fram_lmb$volume <- dat_lmb$Daily_Download
        
        fram_lmb <- fram_lmb[,c(2,3,1)]
        
        names(fram_lmb) <- c("month", "volume", "intensity")
        
        fram_lmb$volume <- as.character(fram_lmb$volume)
        
        
        
        return(fram_lmb)
        
        
        
        
        
    })
    
    
    #output of data table
    
    output$lmb_pred <- renderTable({lmb_built()})
    
    
    # plot output MXLLB
    
    output$lmb_plot <- renderPlot({
        
        fram <- lmb_built()
        
        c4 <- c("current", "fut", "fut", "fut", "fut", "fut")
        
        fram = cbind(fram, c4)
        
        p <- ggplot(fram, aes(x = month, y = as.numeric(intensity), fill = c4)) + scale_y_continuous(limits=c(0, 190))
        
        p <- p + geom_bar(stat = "identity", colour = "black") + ggtitle("MXLMB Intensity") + xlab("Month") + ylab("Intensity") 
        
        #+ theme(plot.title = element_text(size = rel(2), face = "bold"))
        
        p <- p + scale_fill_manual("legend", values = c("current" = "deepskyblue2", "fut" = "dodgerblue2"))
        
        p + geom_text(data = fram, aes(x = month, y = intensity/5, label = paste0(round(intensity, 1), "%")), size = 6, colour = "white")
        
        
    })
    
    ########################## LHB #######################   
    
    lhb_built <- reactive({
        
        vol_lhb <- input$volume_lhb
        aht_lhb <- input$aht_lhb
        ah_lhb <- input$ah_lhb
        
        ready_lhb <- 7.37
        not_ready_lhb <- 8.9
        read_pc_lhb <- 17
        intensity <- 1
        
        # volume input from seasonal factors tab
        
        lhb_1 <- input$lhb_1
        lhb_2 <- input$lhb_2
        lhb_3 <- input$lhb_3
        lhb_4 <- input$lhb_4
        lhb_5 <- input$lhb_5
        
        vol_season_lhb <-  c(lhb_1, lhb_2, lhb_3, lhb_4, lhb_5)
        
        
        #future volume
        
        
        fut_lhbvol <-  vol_lhb
        
        for(i in 1:length(vol_season_lhb)){
            fut_lhbvol[i+1] <- fut_lhbvol[i]*vol_season_lhb[i]
        }
        
        fut_lhbvol <- round(fut_lhbvol, 0)
        
        
        dat_lhb <- data.frame()
        
        dat_lhb <- c(ah_lhb, vol_lhb, 1, aht_lhb, 17, ready_lhb, not_ready_lhb)
        
        
        
        
        
        names(dat_lhb) <- c("Agent_Hours", "Daily_Download", "Intensity", "AHT", "Ready.Connection","Ready_Time","NotReady")
        
        dat_lhb <- as.data.frame(t(dat_lhb))
        
        dat_lhb[1:6, ] <- dat_lhb[1, ]
        
        dat_lhb[, 2] <- fut_lhbvol
        
        
        
        
        pred <- predict(gbm_lhb, newdata= (dat_lhb), n.trees = 10000)
        
        
        fram_lhb <- as.data.frame((pred-0.1)*100)
        
        
        
        fram_lhb$month <-  c("m", "m+1", "m+2", "m+3", "m+4", "m+5")
        
        fram_lhb$month <-factor(fram_lhb$month, 
                                levels = c("m", "m+1", "m+2", "m+3", "m+4", "m+5"
                                ))
        
        fram_lhb$volume <- dat_lhb$Daily_Download
        
        fram_lhb <- fram_lhb[,c(2,3,1)]
        
        names(fram_lhb) <- c("month", "volume", "intensity")
        
        fram_lhb$volume <- as.character(fram_lhb$volume)
        
        return(fram_lhb)
        
        
        
        
        
    })
    
    
    #output of data table
    
    output$lhb_pred <- renderTable({lhb_built()})
    
    
    # plot output MXLLB
    
    output$lhb_plot <- renderPlot({
        
        fram <- lhb_built()
        
        c4 <- c("current", "fut", "fut", "fut", "fut", "fut")
        
        fram  <-  cbind(fram, c4)
        
        p <- ggplot(fram, aes(x = month, y = as.numeric(intensity), fill = c4)) + scale_y_continuous(limits=c(0, 190))
        
        p <- p + geom_bar(stat = "identity", colour = "black") + ggtitle("MXLHB Intensity") + xlab("Month") + ylab("Intensity") 
        
        #+ theme(plot.title = element_text(size = rel(2), face = "bold"))
        
        p <- p + scale_fill_manual("legend", values = c("current" = "deepskyblue2", "fut" = "dodgerblue2"))
        
        p + geom_text(data = fram, aes(x = month, y = intensity/5, label = paste0(round(intensity, 1), "%")), size = 6, colour = "white")
        
        
    })
    
    
    
    #
################################   summary collection table #####################################
    
    


    
    col_summar <- reactive({
        
        #team data based on inputs from each tab
        
        elo <- elo_built()
        emb <- emb_built()
        ehb <- ehb_built()
        llb <- llb_built()
        lmb <- lmb_built()
        lhb <- lhb_built()
     
        
        #total volume for intensity weighted by volume i calculation
        
        col_vol <- as.numeric(elo[,2]) + as.numeric(emb[,2]) + as.numeric(ehb[,2]) +as.numeric(llb[,2]) +as.numeric(lmb[,2]) + as.numeric(lhb[,2])
        
        
        
        # volume weighted intensity for each team
        
        elo_int <- as.numeric(elo[,2]) * as.numeric(elo[,3]) / col_vol
        
        emb_int <- as.numeric(emb[,2]) * as.numeric(emb[,3]) / col_vol
        
        ehb_int <- as.numeric(ehb[,2]) * as.numeric(ehb[,3]) / col_vol
        
        llb_int <- as.numeric(llb[,2]) * as.numeric(llb[,3]) / col_vol
        
        lmb_int <- as.numeric(lmb[,2]) * as.numeric(lmb[,3]) / col_vol
        
        lhb_int <- as.numeric(lhb[,2]) * as.numeric(lhb[,3]) / col_vol
        
        
        
        
        #total collection intensity 
        
        col_m <- elo[,1]
        
            
     
        
        col_int <- elo_int + emb_int + ehb_int + llb_int + lmb_int + lhb_int
        
        col_sum <- cbind.data.frame("month" = col_m, "volume" = col_vol, "intensity" = col_int)
        
        col_sum$volume <- as.character(col_sum$volume)
        
        return(col_sum)
               
        
    })

    output$summar <- renderTable({col_summar()})


    output$col_plot <- renderPlot({
        
        fram <- col_summar()
        
        c4 <- c("current", "fut", "fut", "fut", "fut", "fut")
        
        fram  <- cbind(fram, c4)
        
        p <- ggplot(fram, aes(x = month, y =as.numeric(intensity), fill = c4)) + scale_y_continuous(limits = c(0, 190))
        
        p <- p + geom_bar(stat = "identity", colour = "black")  + xlab("Month") + ylab("Intensity")
        
        p <- p + scale_fill_manual("legend", values = c("current" = "deepskyblue2", "fut" = "dodgerblue2"))
        
        p + geom_text(data = fram, aes(x = month, y = intensity/5, label = paste0(round(intensity, 1), "%")), size = 6, colour = "white")
        
        
               
        
    })



##################################### save option for collection summary side panel #########################


    save_table <- data.frame(

        vol = NA,
        AHT = NA, 
        agent_hours = NA,
        shrink_me = NA,
        shrink_mt = NA,
        staff_me = NA,
        staff_mt = NA,
        ntr_me = NA,
        ntr_mt = NA,
        m1 = NA,
        m2 = NA,
        m3 = NA, 
        m4 = NA, 
        m5 = NA
        
        )
    
    row.names(save_table)  <-  "MXELO"



   

    observeEvent( input$save,
                  
                
        {
          
        #MXELO
        save_table["MXELO", "vol"] <- input$volume_elo
        save_table["MXELO", "AHT"] <- input$aht_elo
        save_table["MXELO", "shrink_me"] <- input$shrink_me
        save_table["MXELO", "shrink_mt"] <- input$shrink_mt
        save_table["MXELO", "staff_me"] <- input$staff_me
        save_table["MXELO", "staff_mt"] <- input$staff_mt        
        save_table["MXELO", "ntr_me"] <- input$ntr_me
        save_table["MXELO", "ntr_mt"] <- input$ntr_mt
        save_table["MXELO", "m1"] <- input$elo_1
        save_table["MXELO", "m2"] <- input$elo_2
        save_table["MXELO", "m3"] <- input$elo_3
        save_table["MXELO", "m4"] <- input$elo_4
        save_table["MXELO", "m5"] <- input$elo_5
        
        #MXEMB
        save_table["MXEMB", "vol"] <- input$volume_emb
        save_table["MXEMB", "AHT"] <- input$aht_emb
        save_table["MXEMB", "agent_hours"] <- input$ah_emb
        save_table["MXEMB", "m1"] <- input$emb_1
        save_table["MXEMB", "m2"] <- input$emb_2
        save_table["MXEMB", "m3"] <- input$emb_3
        save_table["MXEMB", "m4"] <- input$emb_4
        save_table["MXEMB", "m5"] <- input$emb_5
        
        #MXEHB
        save_table["MXEHB", "vol"] <- input$volume_ehb
        save_table["MXEHB", "AHT"] <- input$aht_ehb
        save_table["MXEHB", "agent_hours"] <- input$ah_ehb
        save_table["MXEHB", "m1"] <- input$ehb_1
        save_table["MXEHB", "m2"] <- input$ehb_2
        save_table["MXEHB", "m3"] <- input$ehb_3
        save_table["MXEHB", "m4"] <- input$ehb_4
        save_table["MXEHB", "m5"] <- input$ehb_5
        
        #MXLLB
        save_table["MXLLB", "vol"] <- input$volume_llb
        save_table["MXLLB", "AHT"] <- input$aht_llb
        save_table["MXLLB", "agent_hours"] <- input$ah_llb
        save_table["MXLLB", "m1"] <- input$llb_1
        save_table["MXLLB", "m2"] <- input$llb_2
        save_table["MXLLB", "m3"] <- input$llb_3
        save_table["MXLLB", "m4"] <- input$llb_4
        save_table["MXLLB", "m5"] <- input$llb_5
        
        #MXLLB
        save_table["MXLMB", "vol"] <- input$volume_lmb
        save_table["MXLMB", "AHT"] <- input$aht_lmb
        save_table["MXLMB", "agent_hours"] <- input$ah_lmb
        save_table["MXLMB", "m1"] <- input$lmb_1
        save_table["MXLMB", "m2"] <- input$lmb_2
        save_table["MXLMB", "m3"] <- input$lmb_3
        save_table["MXLMB", "m4"] <- input$lmb_4
        save_table["MXLMB", "m5"] <- input$lmb_5
        
        #MXLLB
        save_table["MXLHB", "vol"] <- input$volume_lhb
        save_table["MXLHB", "AHT"] <- input$aht_lhb
        save_table["MXLHB", "agent_hours"] <- input$ah_lhb
        save_table["MXLHB", "m1"] <- input$lhb_1
        save_table["MXLHB", "m2"] <- input$lhb_2
        save_table["MXLHB", "m3"] <- input$lhb_3
        save_table["MXLHB", "m4"] <- input$lhb_4
        save_table["MXLHB", "m5"] <- input$lhb_5
        
        
        filen = paste0("param/",input$save_file_name, ".csv")
        
        write.csv(save_table, file = filen, row.names = F)
        
        
        #print(save_table)
        
        #print(filen)
        
        #updateSelectInput to refresh drop down menu with file just saved
        
        updateSelectInput(session, "selectl", label = "", choices  = dir("param"))
                     
            
        })


######################## load option for colletion summary side panel ########

    
    observeEvent( input$load, {
        
        
        
        loaded = TRUE
        
        file <- paste0("param/", input$selectl)
        
        dat <- read.csv(file, header = TRUE,row.names = c("MXELO", "MXEMB", "MXEHB", "MXLLB", "MXLMB", "MXLHB"), sep = ",",  stringsAsFactors = F)
        
        
    
        #print(row.names(dat))
        #print(names(dat))
        print(dat)
        
        #update MXELO data based on loaded file
        
        updateSliderInput(session, "volume_elo", value = dat["MXELO", "vol"])
        updateSliderInput(session, "aht_elo", value = dat["MXELO", "AHT"])
        updateSliderInput(session, "shrink_me", value = dat["MXELO", "shrink_me"])
        updateSliderInput(session, "shrink_mt", value = dat["MXELO", "shrink_mt"])
        
        updateNumericInput(session, "staff_me", value = dat["MXELO", "staff_me"])
        updateNumericInput(session, "staff_mt", value = dat["MXELO", "staff_mt"])
        
        updateNumericInput(session, "ntr_me", value = dat["MXELO", "ntr_me"])
        updateNumericInput(session, "ntr_mt", value = dat["MXELO", "ntr_mt"])
        
        #update MXEMB data based on loaded file
        
        updateSliderInput(session, "volume_emb", value = dat["MXEMB", "vol"])
        updateSliderInput(session, "aht_emb", value = dat["MXEMB", "AHT"])
        updateSliderInput(session, "ah_emb", value = dat["MXEMB", "agent_hours"])
        
        #update MXEHB data based on loaded file
        
        updateSliderInput(session, "volume_ehb", value = dat["MXEHB", "vol"])
        updateSliderInput(session, "aht_ehb", value = dat["MXEHB", "AHT"])
        updateSliderInput(session, "ah_ehb", value = dat["MXEHB", "agent_hours"])
        
        #update MXLLB data based on loaded file
        
        updateSliderInput(session, "volume_llb", value = dat["MXLLB", "vol"])
        updateSliderInput(session, "aht_llb", value = dat["MXLLB", "AHT"])
        updateSliderInput(session, "ah_llb", value = dat["MXLLB", "agent_hours"])
        
        #update MXLMB data based on loaded file
        
        updateSliderInput(session, "volume_lmb", value = dat["MXLMB", "vol"])
        updateSliderInput(session, "aht_lmb", value = dat["MXLMB", "AHT"])
        updateSliderInput(session, "ah_lmb", value = dat["MXLMB", "agent_hours"])
        
        #update MXLHB data based on loaded file
        
        updateSliderInput(session, "volume_lhb", value = dat["MXLHB", "vol"])
        updateSliderInput(session, "aht_lhb", value = dat["MXLHB", "AHT"])
        updateSliderInput(session, "ah_lhb", value = dat["MXLHB", "agent_hours"])
        
        
        #update month over month factor
        #loop to update all seasonal factors based on loaded file
        
        
        teams <- c("MXELO", "MXEMB", "MXEHB", "MXLLB", "MXLMB", "MXLHB")
        factm <- c("elo", "emb", "ehb", "llb", "lmb", "lhb")
        month_ovm <- c("m1", "m2", "m3", "m4", "m5")
        
        for(i in 1:6){
            for(j in 1:6){
                
                #factn is name of each seasonal factor from the ui side all of them starting by 3 letters of a team + a number example elo_1 is month over month factor number 1
            
                factn <- paste0(factm[i], "_", j)
                
                updateNumericInput(session, factn, value = dat[teams[i], month_ovm[j]])                
                }
            
            
            
            
            
        }
        
        
        
        
        
        })






        
        
      
}
           
        
        
        
        

    
    










