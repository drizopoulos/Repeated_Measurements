shinyServer(function(input, output) {
    
    output$section_choice <- renderUI({
        if (!is.null(input$chapter)) {
            chs <- switch(input$chapter,
                "Chapter 1" = paste("Section", c("1.1")),
                "Chapter 2" = paste("Section", c("2.2", "2.4", "2.7", "2.9", "2.12")),
                "Chapter 3" = paste("Section", c("3.2", "3.3", "3.4", "3.6*", "3.7", "3.8*", "3.10", "3.11")),
                "Chapter 4" = paste("Section", c("4.1", "4.3", "4.5", "4.6")),
                "Chapter 5" = paste("Section", c("5.2", "5.3", "5.6")),
                "Chapter 6" = paste("Section", c("6.3")),
                "Practicals" = paste("Practical", 1:4),
                "Quizzes" = paste("Quiz", 1:5),
                "Advanced Topics" = c("Count Data 1", "Count Data 2", "Ordinal Data",
                                      "Dynamic Predictions")
            )
            selectInput("section", "Select section:", chs, chs[1])
        }
    })

    ######################################################################################
    ######################################################################################
    
    ##############
    # start page #
    ##############
    
    output$start_page <- renderText({
        if (input$chapter == 'Chapter 0')
            includeHTML("./html/start_page.Rhtml")
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 1.1 #
    ###############
    
    output$s11_data_choice <- renderUI({
        if (input$chapter == "Chapter 1" && input$section == "Section 1.1")
            fluidRow(column(4, radioButtons("data", "Data set:", c("AIDS", "PBC", "Prothro", "Glaucoma"))),
                     column(6, checkboxInput("s11_loess", "Include loess/spline"), 
                            checkboxInput("s11_sample", "Show sample patients")))
    })
    
    output$s11_glaucoma_choice <- renderUI({
        if (input$chapter == "Chapter 1" && input$section == "Section 1.1"
            && naf(input$data) && input$data == "Glaucoma")
            fluidRow(column(4, numericInput("id", "Patient:", 1, min = 1, max = 139)),
                     column(4, radioButtons("eye", "Eye:", c("right", "left"))))
    })
    
    output$s11_code_aids <- renderText({
        if (input$chapter == "Chapter 1" && input$section == "Section 1.1") {
            if (naf(input$data) && input$data == "AIDS") {
                if (input$s11_loess && !input$s11_sample) {
                    includeMarkdown("./md/s11_code_aids_loess.Rmd")
                } else if (input$s11_sample && !input$s11_loess) {
                    includeMarkdown("./md/s11_code_aids_sample.Rmd")
                } else if (input$s11_sample && input$s11_loess) {
                    includeMarkdown("./md/s11_code_aids_loess_sample.Rmd")
                } else {
                    includeMarkdown("./md/s11_code_aids.Rmd")
                }
            }
        }
    })
    
    output$s11_code_prothro <- renderText({  
        if (input$chapter == "Chapter 1" && input$section == "Section 1.1") {
            if (naf(input$data) && input$data == "Prothro") {
                if (input$s11_loess && !input$s11_sample) {
                    includeMarkdown("./md/s11_code_prothro_loess.Rmd")
                } else if (input$s11_sample && !input$s11_loess) {
                    includeMarkdown("./md/s11_code_prothro_sample.Rmd")
                } else if (input$s11_sample && input$s11_loess) {
                    includeMarkdown("./md/s11_code_prothro_loess_sample.Rmd")
                } else {
                    includeMarkdown("./md/s11_code_prothro.Rmd")
                }
            }
        }
    })
    
    output$s11_code_pbc <- renderText({
        if (input$chapter == "Chapter 1" && input$section == "Section 1.1") {
            if (naf(input$data) && input$data == "PBC") {
                if (input$s11_loess && !input$s11_sample) {
                    includeMarkdown("./md/s11_code_pbc_loess.Rmd")
                } else if (input$s11_sample && !input$s11_loess) {
                    includeMarkdown("./md/s11_code_pbc_sample.Rmd")
                } else if (input$s11_sample && input$s11_loess) {
                    includeMarkdown("./md/s11_code_pbc_loess_sample.Rmd")
                } else {
                    includeMarkdown("./md/s11_code_pbc.Rmd")
                }
            }
        }
    })
    
    output$s11_code_glaucoma <- renderText({  
        if (input$chapter == "Chapter 1" && input$section == "Section 1.1") {
            if (naf(input$data) && input$data == "Glaucoma") {
                if (input$s11_loess) {
                    includeMarkdown("./md/s11_code_glaucoma_loess.Rmd")
                } else {
                    includeMarkdown("./md/s11_code_glaucoma.Rmd")
                }
            }
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 2.2 #
    ###############
    
    output$s22_code_lm <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.2") {
            includeMarkdown("./md/s22_code_lm.Rmd")
        }
    })
    
    output$s22_Routput_lm <- renderPrint({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.2") {
            fm_s22 <- lm(log(serBilir) ~ age + drug, data = pbc2.id)
            htmlPrint(summary(fm_s22))
        }
    })

    ###############
    # Section 2.4 #
    ###############
    
    output$s24_choice <- renderUI({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.4")
            radioButtons("fit_effPlt", "Select:", 
                         c("Model fit (AIDS data)", "Effect plot (PBC data)"))
    })
   
    output$s24_Agechoice <- renderUI({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.4"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot (PBC data)")
            sliderInput("age_select_pbc", "Age",
                        min = 30, max = 65, value = 49, animate = TRUE, step = 5)
    })
    
    output$s24_code_gls <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.4"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Model fit (AIDS data)") {
            includeMarkdown("./md/s24_code_gls.Rmd")
        }
    })
    
    output$s24_Routput_gls <- renderPrint({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.4"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Model fit (AIDS data)") {
            fm_s24_aids <- gls(CD4 ~ obstime + obstime:drug,
                          correlation = corCompSymm(form = ~ obstime | patient),
                          data = aids)
            htmlPrint2(
                '# summarize model fit\n',
                summary(fm_s24_aids),
                '\n# Marginal covariance matrix',
                getVarCov(fm_s24_aids, individual = 450),
                '\n# Induced correlation matrix',
                cov2cor(getVarCov(fm_s24_aids, individual = 450))
            )
        }
    })
    
    output$s24_code_gls_effectPlot <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.4"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot (PBC data)") {
            includeMarkdown("./md/s24_code_gls_effectPlot.Rmd")
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 2.7 #
    ###############
    
    output$s27_cs_choice <- renderUI({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.7")
            fluidRow(column(7, radioButtons("corrStr", "Select correlation structure:", 
                                            c("Compound Symmetry", "AR1", "continuous AR1",
                                              "exponential", "linear", "Gaussian"))), 
                     column(4, checkboxInput("corr_plot", "Correlation plot")))
    })

    output$s27_cs_choice_slider <- renderUI({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.7") {
            if (length(input$corrStr) && 
                       input$corrStr %in% c("Compound Symmetry", "AR1", "continuous AR1")) {
                max <- 0.95
                stp <- 0.1
            } else {
                max <- 20
                stp <- 2
            }
            min <- if (length(input$corrStr) && input$corrStr == "linear") {
                0.3
            } else {
                0.05
            }
            sliderInput("corrStr_param", "parameter value",
                        min = min, max = max, value = 0.05, animate = TRUE, step = stp)
        }
    })
    
        
    output$s27_check_tab <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.7")
            includeHTML("./html/check_tab.Rhtml")
    })
    
    output$s27_Routput_gls <- renderPrint({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.7" 
            && naf(input$corr_plot) && !input$corr_plot) {
            Fun <- switch(input$corrStr,
                          "Compound Symmetry" = corCompSymm,
                          "AR1" = corAR1, 
                          "continuous AR1" = corCAR1,
                          "exponential" = corExp,
                          "linear" = corLin,
                          "Gaussian" = corGaus)
            htmlPrint(testCS(Fun, input$corrStr_param), comment = "# Marginal covariance matrix")
        }
    })
    
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 2.9 #
    ###############
    
    output$s29_test_choice <- renderUI({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9")
            radioButtons("test_gls", "Select test:", 
                         c("LRT covariance matrix", "AIC/BIC covariance matrix", 
                           "t-test regression coefficients",
                           "F-test regression coefficients",
                           "LRT regression coefficients"))
    })
    
    output$s29_code_anova_gls <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9" 
            && naf(input$test_gls) && input$test_gls == "LRT covariance matrix") {
            includeMarkdown("./md/s29_code_anova_gls.Rmd")
        }
    })
    
    output$s29_code_anova_gls2 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9"
            && naf(input$test_gls) && input$test_gls == "AIC/BIC covariance matrix") {
            includeMarkdown("./md/s29_code_anova_gls2.Rmd")
        }
    })
    
    output$s29_code_anova_gls3 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9"
            && naf(input$test_gls) && input$test_gls == "t-test regression coefficients") {
            includeMarkdown("./md/s29_code_anova_gls3.Rmd")
        }
    })
    
    output$s29_code_anova_gls4 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9"
            && naf(input$test_gls) && input$test_gls == "F-test regression coefficients") {
            includeMarkdown("./md/s29_code_anova_gls4.Rmd")
        }
    })
    
    output$s29_code_anova_gls5 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9"
            && naf(input$test_gls) && input$test_gls == "LRT regression coefficients") {
            includeMarkdown("./md/s29_code_anova_gls5.Rmd")
        }
    })
    
    output$s29_Routput_gls <- renderPrint({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9"
            && naf(input$test_gls) && input$test_gls == "LRT covariance matrix") {
            fm_s29_aids1 <- gls(CD4 ~ obstime + obstime:drug, data = aids,
                                correlation = corCompSymm(form = ~ obstime | patient))
            if (!exists("fm_s29_aids2")) {
                withProgress({
                    fm <- gls(CD4 ~ obstime + obstime:drug, data = aids,
                                     correlation = corSymm(form = ~ 1 | patient),
                                     weights = varIdent(form = ~ 1 | obstime))
                }, message = "Fitting the model...")
                fm_s29_aids2 <<- fm
                fm_s212_aids <<- fm
            }
            htmlPrint(anova(fm_s29_aids1, fm_s29_aids2))
        }
    })
    
    output$s29_Routput_gls2 <- renderPrint({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9"
            && naf(input$test_gls) && input$test_gls == "AIC/BIC covariance matrix") {
            fm_s29_pro1 <- gls(pro ~ time + time:treat, data = prothro,
                               correlation = corExp(form = ~ time | id))
            fm_s29_pro2 <- gls(pro ~ time + time:treat, data = prothro,
                               correlation = corGaus(form = ~ time | id))
            fm_s29_pro3 <- gls(pro ~ time + time:treat, data = prothro,
                               correlation = corExp(form = ~ time | id),
                               weights = varExp(form = ~ time))
            fm_s29_pro4 <- gls(pro ~ time + time:treat, data = prothro,
                               correlation = corGaus(form = ~ time | id),
                               weights = varExp(form = ~ time))
            htmlPrint(anova(fm_s29_pro1, fm_s29_pro2, fm_s29_pro3, 
                            fm_s29_pro4, test = FALSE), comment = '# AIC and BIC all models')
            htmlPrint(anova(fm_s29_pro1, fm_s29_pro3), 
                      comment = '# LRT exponential correlation with & without variance function')
            htmlPrint(anova(fm_s29_pro2, fm_s29_pro4), 
                      comment = '# LRT Gaussian correlation with & without variance function')
        }
    })
    
    output$s29_Routput_gls3 <- renderPrint({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9"
            && naf(input$test_gls) && input$test_gls == "t-test regression coefficients") {
            fm_s29_pbc <- gls(log(serBilir) ~ year + year:drug + year * sex + age, data = pbc2,
                              correlation = corCAR1(form = ~ year | id))
            htmlPrint(summary(fm_s29_pbc))
        }
    })
    
    output$s29_Routput_gls4 <- renderPrint({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9"
            && naf(input$test_gls) && input$test_gls == "F-test regression coefficients") {
            fm_s29_pbc <- gls(log(serBilir) ~ year + year:drug + year * sex + age, data = pbc2,
                              correlation = corCAR1(form = ~ year | id))
            L_sex <- rbind(c(0,0,1,0,0,0), c(0,0,0,0,0,1))
            
            htmlPrint2(
                "# marginal F-tests for each term",
                anova(fm_s29_pbc, type = "marginal"),
                "\n# F-test for specific terms",
                anova(fm_s29_pbc, Terms = c("sex", "year:sex")),
                paste0("\n # the same as above but with a contrast matrix;\n",
                       " # first we see how many coefficients we have"),
                as.matrix(coef(fm_s29_pbc)),
                paste0("\n # we want to test the coefficients of that ",
                       "include 'sex'. We have two\n # the 3rd and 6th. ",
                       "Hence, the contrast matrix is:"),
                L_sex,
                "\n# we give it to anova()",
                anova(fm_s29_pbc, L = L_sex)
            )
        }
    })
    
    output$s29_Routput_gls5 <- renderPrint({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.9"
            && naf(input$test_gls) && input$test_gls == "LRT regression coefficients") {
            fm_s29_pbc1 <- gls(log(serBilir) ~ year + year:drug + year * sex + age, data = pbc2,
                               correlation = corCAR1(form = ~ year | id), method = "ML")
            fm_s29_pbc2 <- gls(log(serBilir) ~ year + year:drug + age, data = pbc2,
                               correlation = corCAR1(form = ~ year | id), method = "ML")
            htmlPrint(anova(fm_s29_pbc2, fm_s29_pbc1))
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    #################
    # Section 2.12 #
    #################
    
    output$s212_plot_datachoice <- renderUI({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12") {
            radioButtons("s212_datachoice", "Select dataset:", c("AIDS", "PBC"))
        }
    })
    
    output$s212_plot_choice <- renderUI({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12") {
            chs <- if (naf(input$s212_datachoice) && input$s212_datachoice == "AIDS") "fitted" 
            else c("fitted", "year", "age")
            cond_var <- if (naf(input$s212_datachoice) && input$s212_datachoice == "AIDS") "Condition on drug"
            else "Condition on sex"
            fluidRow(column(4, radioButtons("s212_typePlot", "Type of plot:", c("Scatterplot", "QQnorm"))),
                     column(4, radioButtons("s212_type_res", "Residuals type:", c("Pearson", "normalized"))),
                     column(4, selectInput("s212_var_res", "Select variable:", chs, chs[1]),
                            checkboxInput("s212_sex", cond_var)))
        }
    })
    
    output$s212_code_plot1 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_var_res == "fitted" && !input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResPFitt.Rmd")
    })

    output$s212_code_plot2 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_var_res == "fitted" && !input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResNFitt.Rmd")
    })

    output$s212_code_plot3 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_var_res == "fitted" && input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResPFitt-sex.Rmd")
    })

    output$s212_code_plot4 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_var_res == "fitted" && input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResNFitt-sex.Rmd")
    })
    
    output$s212_code_plot5 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_var_res == "year" && !input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResPYear.Rmd")
    })
    
    output$s212_code_plot6 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_var_res == "year" && !input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResNYear.Rmd")
    })
    
    output$s212_code_plot7 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_var_res == "year" && input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResPYear-sex.Rmd")
    })
    
    output$s212_code_plot8 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_var_res == "year" && input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResNYear-sex.Rmd")
    })
    
    output$s212_code_plot9 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_var_res == "age" && !input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResPAge.Rmd")
    })
    
    output$s212_code_plot10 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_var_res == "age" && !input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResNAge.Rmd")
    })
    
    output$s212_code_plot11 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_var_res == "age" && input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResPAge-sex.Rmd")
    })
    
    output$s212_code_plot12 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_var_res == "age" && input$s212_sex)
            includeMarkdown("./md/s212_code_plot_ResNAge-sex.Rmd")
    })
    
    output$s212_code_plot13 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "QQnorm"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_sex)
            includeMarkdown("./md/s212_code_QQplot_ResN-sex.Rmd")
    })
    
    output$s212_code_plot14 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "QQnorm"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && !input$s212_sex)
            includeMarkdown("./md/s212_code_QQplot_ResN.Rmd")
    })
    
    output$s212_code_plot15 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "QQnorm"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_sex)
            includeMarkdown("./md/s212_code_QQplot_ResP-sex.Rmd")
    })
    
    output$s212_code_plot16 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "PBC"
            && naf(input$s212_typePlot) && input$s212_typePlot == "QQnorm"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && !input$s212_sex)
            includeMarkdown("./md/s212_code_QQplot_ResP.Rmd")
    })
    
    output$s212_code_plot17 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "AIDS"
            && naf(input$s212_typePlot) && input$s212_typePlot == "QQnorm"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_sex)
            includeMarkdown("./md/s212_code_QQplotAIDS_ResN-sex.Rmd")
    })
    
    output$s212_code_plot18 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "AIDS"
            && naf(input$s212_typePlot) && input$s212_typePlot == "QQnorm"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && !input$s212_sex)
            includeMarkdown("./md/s212_code_QQplotAIDS_ResN.Rmd")
    })
    
    output$s212_code_plot19 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "AIDS"
            && naf(input$s212_typePlot) && input$s212_typePlot == "QQnorm"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_sex)
            includeMarkdown("./md/s212_code_QQplotAIDS_ResP-sex.Rmd")
    })

    output$s212_code_plot20 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "AIDS"
            && naf(input$s212_typePlot) && input$s212_typePlot == "QQnorm"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && !input$s212_sex)
            includeMarkdown("./md/s212_code_QQplotAIDS_ResP.Rmd")
    })
    
    output$s212_code_plot21 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "AIDS"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_var_res == "fitted" && !input$s212_sex)
            includeMarkdown("./md/s212_code_plotAIDS_ResPFitt.Rmd")
    })
    
    output$s212_code_plot22 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "AIDS"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_var_res == "fitted" && !input$s212_sex)
            includeMarkdown("./md/s212_code_plotAIDS_ResNFitt.Rmd")
    })
    
    output$s212_code_plot23 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "AIDS"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "Pearson"
            && naf(input$s212_var_res) && input$s212_var_res == "fitted" && input$s212_sex)
            includeMarkdown("./md/s212_code_plotAIDS_ResPFitt-sex.Rmd")
    })
    
    output$s212_code_plot24 <- renderText({
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12"
            && naf(input$s212_datachoice) && input$s212_datachoice == "AIDS"
            && naf(input$s212_typePlot) && input$s212_typePlot == "Scatterplot"
            && naf(input$s212_type_res) && input$s212_type_res == "normalized"
            && naf(input$s212_var_res) && input$s212_var_res == "fitted" && input$s212_sex)
            includeMarkdown("./md/s212_code_plotAIDS_ResNFitt-sex.Rmd")
    })
    
    ######################################################################################
    ######################################################################################

    ###############
    # Section 3.2 #
    ###############
    
    output$s32_choice <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.2")
            radioButtons("fit_effPlt", "Select:", 
                         c("Model fit (AIDS data)", "Effect plot (PBC data)"))
    })
    
    output$s32_Agechoice <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.2"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot (PBC data)")
            sliderInput("age_select_pbc", "Age",
                        min = 30, max = 65, value = 49, animate = TRUE, step = 5)
    })
    
    output$s32_Prochoice <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.2"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot (PBC data)")
            sliderInput("pro_select_pbc", "Prothrombin",
                        min = 9.5, max = 13, value = 10.6, step = 0.5, animate = TRUE)
    })

    output$s32_code_lme <- renderText({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.2"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Model fit (AIDS data)") {
            includeMarkdown("./md/s32_code_lme.Rmd")
        }
    })
    
    output$s32_Routput_lme <- renderPrint({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.2"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Model fit (AIDS data)") {
            fm_s32_aids1 <- lme(CD4 ~ obstime + obstime:drug, random = ~ obstime | patient,
                               data = aids)
            fm_s32_aids2 <- lmer(CD4 ~ obstime + obstime:drug + (obstime | patient),
                               data = aids)
            
            htmlPrint2(
                '###########################',
                '# Mixed Model using lme() #',
                '###########################',
                '\n# summarized model fit',
                summary(fm_s32_aids1),
                '\nmarginal covariance matrix',
                getVarCov(fm_s32_aids1, individuals = 450, type = "marginal"),
                '\n# corresponding correlation matrix',
                cov2cor(getVarCov(fm_s32_aids1, individuals = 450, type = "marginal")[[1]]),
                '\n\n',
                '############################',
                '# Mixed Model using lmer() #',
                '############################',
                '\n# summarized model fit',
                summary(fm_s32_aids2),
                '\n# covariance matrix of the random effects',
                VarCorr(fm_s32_aids2)
            )
        }
    })
    
    output$s32_code_lme_effectPlot <- renderText({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.2"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot (PBC data)") {
            includeMarkdown("./md/s32_code_lme_effectPlot.Rmd")
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 3.3 #
    ###############
    
    output$s33_res_choice <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3")
            fluidRow(column(7, radioButtons("reStr", "Random effects structure:", 
                                            c("intercepts", 
                                              "intercepts & slopes", 
                                              "intercepts, slopes & slopes^2")),
                            checkboxInput("diag_covMat", "Diagonal matrix random effects")),
                     column(4, radioButtons("covMatrix", "Type matrix:", 
                                            c("covariance", "correlation")),
                            checkboxInput("corr_plot", "Correlation plot")))
    })
    
    output$s33_slider_sigma2_b0 <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3") {
            sliderInput("sigma2_b0", "intercepts variance", min = 0.05, max = 10, value = 1,
                        animate = TRUE, step = 1)
        }
    })
    
    output$s33_slider_sigma2_b1 <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3"
            && naf(input$reStr) && input$reStr != "intercepts") {
            sliderInput("sigma2_b1", "slopes variance", min = 0.05, max = 10, value = 1,
                        animate = TRUE, step = 1)
        }
    })

    output$s33_slider_rho_b0b1 <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3"
            && naf(input$reStr) && input$reStr != "intercepts"
            && naf(input$diag_covMat) && !input$diag_covMat) {
            sliderInput("rho_b0b1", "correlation intercepts & slopes", min = -0.99, max = 0.99, value = 0,
                        animate = TRUE, step = 0.1)
        }
    })
    
    output$s33_slider_sigma2_b2 <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3"
            && naf(input$reStr) && input$reStr == "intercepts, slopes & slopes^2") {
            sliderInput("sigma2_b2", "slopes^2 variance", min = 0.05, max = 10, value = 1,
                        animate = TRUE, step = 1)
        }
    })
    
    output$s33_slider_rho_b0b2 <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3"
            && naf(input$reStr) && input$reStr == "intercepts, slopes & slopes^2"
            && naf(input$diag_covMat) && !input$diag_covMat) {
            sliderInput("rho_b0b2", "correlation intercepts & slopes^2", min = -0.99, 
                        max = 0.99, value = 0, animate = TRUE, step = 0.1)
        }
    })
    
    output$s33_slider_rho_b1b2 <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3"
            && naf(input$reStr) && input$reStr == "intercepts, slopes & slopes^2"
            && naf(input$diag_covMat) && !input$diag_covMat) {
            sliderInput("rho_b1b2", "correlation slopes & slopes^2", min = -0.99, 
                        max = 0.99, value = 0, animate = TRUE, step = 0.1)
        }
    })
    
    output$s33_slider_sigma2 <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3") {
            sliderInput("sigma2", "error variance", min = 0.05, max = 10, value = 1,
                        animate = TRUE, step = 1)
        }
    })
    
    output$s33_check_tab <- renderText({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3")
            includeHTML("./html/check_tab.Rhtml")
    })
    
    output$s33_Routput_lme <- renderPrint({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3" 
            && naf(input$reStr) && naf(input$corr_plot) && !input$corr_plot) {
            params <- if (naf(input$diag_covMat) && !input$diag_covMat) {
                switch(input$reStr,
                       "intercepts" = list(sigma2 = input$sigma2, D = as.matrix(input$sigma2_b0)),
                       "intercepts & slopes" = {
                           cov01 <- sqrt(input$sigma2_b0) * sqrt(input$sigma2_b1) * input$rho_b0b1
                           DD <- matrix(c(input$sigma2_b0, cov01, cov01, input$sigma2_b1), 2, 2)
                           list(sigma2 = input$sigma2, D = DD)
                       },
                       "intercepts, slopes & slopes^2" = {
                           cov01 <- sqrt(input$sigma2_b0) * sqrt(input$sigma2_b1) * input$rho_b0b1
                           cov02 <- sqrt(input$sigma2_b0) * sqrt(input$sigma2_b2) * input$rho_b0b2
                           cov12 <- sqrt(input$sigma2_b1) * sqrt(input$sigma2_b2) * input$rho_b1b2
                           DD <- matrix(c(input$sigma2_b0, cov01, cov02, cov01, input$sigma2_b1,
                                          cov12, cov02, cov12, input$sigma2_b2), 3, 3)
                           list(sigma2 = input$sigma2, D = DD)
                       })
            } else { 
                switch(input$reStr,
                       "intercepts" = list(sigma2 = input$sigma2, D = as.matrix(input$sigma2_b0)),
                       "intercepts & slopes" = {
                           DD <- matrix(c(input$sigma2_b0, 0, 0, input$sigma2_b1), 2, 2)
                           list(sigma2 = input$sigma2, D = DD)
                       },
                       "intercepts, slopes & slopes^2" = {
                           DD <- matrix(c(input$sigma2_b0, 0, 0, 0, input$sigma2_b1,
                                          0, 0, 0, input$sigma2_b2), 3, 3)
                           list(sigma2 = input$sigma2, D = DD)
                       })
            }
            if (naf(input$covMatrix) && input$covMatrix == "covariance") {
                htmlPrint(testRES(input$reStr, params), comment = "# Marginal covariance matrix")
            } else {
                htmlPrint(testRES(input$reStr, params, cor = TRUE), comment = "# Marginal correlation matrix")
            }
            
        }
    })

    output$s33_Routput_lme2 <- renderPrint({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3" 
            && naf(input$reStr) && naf(input$corr_plot) && !input$corr_plot) {
            params <- if (naf(input$diag_covMat) && !input$diag_covMat) {
                switch(input$reStr,
                       "intercepts" = list(sigma2 = input$sigma2, D = as.matrix(input$sigma2_b0)),
                       "intercepts & slopes" = {
                           cov01 <- sqrt(input$sigma2_b0) * sqrt(input$sigma2_b1) * input$rho_b0b1
                           DD <- matrix(c(input$sigma2_b0, cov01, cov01, input$sigma2_b1), 2, 2)
                           list(sigma2 = input$sigma2, D = DD)
                       },
                       "intercepts, slopes & slopes^2" = {
                           cov01 <- sqrt(input$sigma2_b0) * sqrt(input$sigma2_b1) * input$rho_b0b1
                           cov02 <- sqrt(input$sigma2_b0) * sqrt(input$sigma2_b2) * input$rho_b0b2
                           cov12 <- sqrt(input$sigma2_b1) * sqrt(input$sigma2_b2) * input$rho_b1b2
                           DD <- matrix(c(input$sigma2_b0, cov01, cov02, cov01, input$sigma2_b1,
                                          cov12, cov02, cov12, input$sigma2_b2), 3, 3)
                           list(sigma2 = input$sigma2, D = DD)
                       })
            } else { 
                switch(input$reStr,
                       "intercepts" = list(sigma2 = input$sigma2, D = as.matrix(input$sigma2_b0)),
                       "intercepts & slopes" = {
                           DD <- matrix(c(input$sigma2_b0, 0, 0, input$sigma2_b1), 2, 2)
                           list(sigma2 = input$sigma2, D = DD)
                       },
                       "intercepts, slopes & slopes^2" = {
                           DD <- matrix(c(input$sigma2_b0, 0, 0, 0, input$sigma2_b1,
                                          0, 0, 0, input$sigma2_b2), 3, 3)
                           list(sigma2 = input$sigma2, D = DD)
                       })
            }
            htmlPrint(nearPD(params$D), comment = "# Covariance matrix of the random effects")
        }
    })
    
    output$s33_Routput_ws <- renderPrint({
        if(input$chapter == "Chapter 3" && input$section == "Section 3.3" 
           && naf(input$reStr) 
           && input$reStr %in% c("intercepts")) {
            includeHTML("./html/white_space.Rhtml")
        }
    })
    
    output$s33_Routput_ws2 <- renderPrint({
        if(input$chapter == "Chapter 3" && input$section == "Section 3.3" 
           && naf(input$reStr) && input$reStr %in% c("intercepts & slopes", 
                                                     "intercepts, slopes & slopes^2")) {
            includeHTML("./html/white_space_long.Rhtml")
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 3.4 #
    ###############
    
    output$s34_choice <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.4") {
            fluidRow(column(12, checkboxInput("s34_data", "Data", TRUE),
                            checkboxInput("s34_marg", "Marginal Predictions"),
                            checkboxInput("s34_subj", "Subject-specific Predictions")))
        }
    })
    
    output$s34_code_lme <- renderText({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.4") {
                if (input$s34_data && input$s34_marg && input$s34_subj) {
                    includeMarkdown("./md/s34_code_lme_preds.Rmd")
                } else if (!input$s34_data && input$s34_marg && input$s34_subj) {
                    includeMarkdown("./md/s34_code_lme_preds_nodata.Rmd")
                } else if (input$s34_data && !input$s34_marg && input$s34_subj) {
                    includeMarkdown("./md/s34_code_lme_preds_nomarg.Rmd")
                } else if (!input$s34_data && !input$s34_marg && input$s34_subj) {
                    includeMarkdown("./md/s34_code_lme_preds_nodatanomarg.Rmd")
                } else if (input$s34_data && input$s34_marg && !input$s34_subj) {
                    includeMarkdown("./md/s34_code_lme_preds_nosubj.Rmd")
                } else if (!input$s34_data && input$s34_marg && !input$s34_subj) {
                    includeMarkdown("./md/s34_code_lme_preds_nodatanosubj.Rmd")
                } else if (input$s34_data && !input$s34_marg && !input$s34_subj) {
                    includeMarkdown("./md/s34_code_lme_preds_nomargnosubj.Rmd")
                }
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 3.6 #
    ###############
    
    output$s36_choice <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.6*")
            radioButtons("neste_lvl", "Select:", 
                         c("nested intercepts", "nested slopes", "crossed intercepts"))
    })
    
    output$s36_code_lme <- renderText({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.6*") {
            if (naf(input$neste_lvl) && input$neste_lvl == "nested intercepts") {
                includeMarkdown("./md/s36_code_lme.Rmd")
            } else if (naf(input$neste_lvl) && input$neste_lvl == "nested slopes") {
                includeMarkdown("./md/s36_code_lme2.Rmd")
            } else if (naf(input$neste_lvl) && input$neste_lvl == "crossed intercepts") {
                includeMarkdown("./md/s36_code_lme3.Rmd")
            }
        }
    })
    
    output$s36_Routput_lme <- renderPrint({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.6*") {
            if (naf(input$neste_lvl) && input$neste_lvl == "nested intercepts") {
                if (!exists("fm_s36_glaucoma1")) {
                    withProgress({
                        fm_s36_glaucoma1 <<- lme(thres ~ years, data = glaucoma,
                                        random = ~ 1 | id / eye)
                    }, message = "Fitting the model...")
                }
                if (!exists("fm_s36_glaucoma2")) {
                    withProgress({
                        fm_s36_glaucoma2 <<- lmer(thres ~ years + (1 | id / eye), data = glaucoma)
                    }, message = "Fitting the model...")
                }
                htmlPrint2(
                    '#####################################',
                    '# nested random effects using lme() #',
                    '#####################################',
                    '\n# summarized model fit',
                    summary(fm_s36_glaucoma1),
                    '\n\n',
                    '######################################',
                    '# nested random effects using lmer() #',
                    '######################################',
                    '\n# summarized model fit',
                    summary(fm_s36_glaucoma2)
                )
            } else if (naf(input$neste_lvl) && input$neste_lvl == "nested slopes") {
                if (!exists("fm_s36_glaucoma3")) {
                    withProgress({
                        fm_s36_glaucoma3 <<- lme(thres ~ years, data = glaucoma,
                                        random = list(id = pdSymm(form = ~ years), 
                                                      eye = pdSymm(form = ~ 1)))
                    }, message = "Fitting the model...")
                }
                if (!exists('fm_s36_glaucoma4')) {
                    withProgress({
                        fm_s36_glaucoma4 <<- lmer(thres ~ years + (years | id) + (1 | id:eye), 
                                             data = glaucoma)
                    }, message = "Fitting the model...")
                }
                
                htmlPrint2(
                    '#####################################',
                    '# nested random effects using lme() #',
                    '#####################################',
                    '\n# summarized model fit',
                    summary(fm_s36_glaucoma3),
                    '\n\n',
                    '######################################',
                    '# nested random effects using lmer() #',
                    '######################################',
                    '\n# summarized model fit',
                    summary(fm_s36_glaucoma4)
                )
            } else if (naf(input$neste_lvl) && input$neste_lvl == "crossed intercepts") {
                if (!exists('fm_s36_glaucoma5')) {
                    withProgress({
                        fm_s36_glaucoma5 <<- lmer(thres ~ years + (1 | id) + (1 |field), 
                                         data = glaucoma)
                    }, message = "Fitting the model...")
                }
                htmlPrint2(
                    '#######################################',
                    '# crossed random effects using lmer() #',
                    '#######################################',
                    '\n# summarized model fit',
                    summary(fm_s36_glaucoma5)
                )
            }
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 3.7 #
    ###############
    
    output$s37_code_lme <- renderText({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.7") {
            includeMarkdown("./md/s37_code_lme.Rmd")
        }
    })
    
    output$s37_Routput_lme <- renderPrint({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.7") {
            fm_s37_aids1 <- lme(CD4 ~ obstime + I(obstime^2) + (obstime + I(obstime^2)):drug, data = aids,
                                random = ~ 1 | patient, 
                                correlation = corExp(form = ~ obstime | patient))
            fm_s37_aids2 <- lme(CD4 ~ obstime + I(obstime^2) + (obstime + I(obstime^2)):drug, data = aids,
                                random = ~ obstime | patient, 
                                correlation = corExp(form = ~ obstime | patient))

            htmlPrint2(
                '# fixed effects per model',
                cbind("Int" = fixef(fm_s37_aids1), 
                      "Linear Slp" = fixef(fm_s37_aids2)),
                '\n# 95% CI for correlation parameter per model',
                intervals(fm_s37_aids1, which = "var-cov")$corStruct,
                intervals(fm_s37_aids2, which = "var-cov")$corStruct
            )
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 3.8 #
    ###############
    
    output$s38_choice <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.8*")
            radioButtons("param_choice", "Select formulation:", 
                         c("current value", "cumulative effect"))
    })
    
    output$s38_code_lme <- renderText({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.8*") {
            if (naf(input$param_choice) && input$param_choice == "current value") {
                includeMarkdown("./md/s38_code_lme1.Rmd")
            } else if (naf(input$param_choice) && input$param_choice == "cumulative effect") {
                includeMarkdown("./md/s38_code_lme2.Rmd")
            }
        }
    })
    
    output$s38_Routput_lme <- renderPrint({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.8*") {
            if (naf(input$param_choice) && input$param_choice == "current value") {
                fm_s38_pbc1 <- lme(log(serBilir) ~ ns(year, 2) + sex + age + prothrombin, 
                                   data = pbc2, random = list(id = pdDiag(form = ~ ns(year, 2))))
                htmlPrint2(
                    '# time-varying prothrombin -- current value formulation',
                    summary(fm_s38_pbc1)
                )
                
            } else if (naf(input$param_choice) && input$param_choice == "cumulative effect") {
                area <- function (y, x, i) {
                    x.i <- x[1:i]
                    y.i <- y[1:i]
                    if (length(y.i) == 1)
                        return(0)
                    f <- approxfun(x.i, y.i, rule = 2, ties = "ordered")
                    integrate(f, lower = 0, upper = max(x))$value
                }
                spl <- split(pbc2[c("id", "prothrombin", "year")], pbc2$id)
                pbc2$area_prothr <- unlist(lapply(spl, function (d) {
                    sapply(seq_len(nrow(d)), function (i) area(d$year, d$prothrombin, i))
                }), use.names = FALSE)
                fm_s38_pbc2 <- lme(log(serBilir) ~ ns(year, 2) + sex + age + area_prothr, 
                                   data = pbc2, random = list(id = pdDiag(form = ~ ns(year, 2))))
                htmlPrint2(
                    '# time-varying prothrombin -- area formulation',
                    summary(fm_s38_pbc2)
                )
                
            }
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ################
    # Section 3.10 #
    ################
    
    output$s310_test_choice <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.10")
            radioButtons("test_lme", "Select test:", 
                         c("LRT random effects", "AIC/BIC random effects", 
                           "t-test regression coefficients",
                           "F-test regression coefficients",
                           "LRT regression coefficients"))
    })
    
    output$s310_code_lme <- renderText({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.10") {
            if (naf(input$test_lme) && input$test_lme == "LRT random effects") {
                includeMarkdown("./md/s310_code_lme1.Rmd")
            } else if (naf(input$test_lme) && input$test_lme == "AIC/BIC random effects") {
                includeMarkdown("./md/s310_code_lme2.Rmd")
            } else if (naf(input$test_lme) && input$test_lme == "t-test regression coefficients") {
                includeMarkdown("./md/s310_code_lme3.Rmd")
            } else if (naf(input$test_lme) && input$test_lme == "F-test regression coefficients") {
                includeMarkdown("./md/s310_code_lme4.Rmd")
            } else if (naf(input$test_lme) && input$test_lme == "LRT regression coefficients") {
                includeMarkdown("./md/s310_code_lme5.Rmd")
            }
        }
    })
    
    output$s310_Routput_lme <- renderPrint({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.10") {
            if (naf(input$test_lme) && input$test_lme == "LRT random effects") {
                if (!exists('fm_s310_aids1')) {
                    withProgress({
                        fm_s310_aids1 <<- lme(CD4 ~ obstime + I(obstime^2) + (obstime + I(obstime^2)):drug, 
                                              data = aids, random = ~ obstime | patient)
                    }, message = "Fitting the model...")
                }
                if (!exists('fm_s310_aids2')) {
                    withProgress({
                    fm_s310_aids2 <<- lme(CD4 ~ obstime + I(obstime^2) + (obstime + I(obstime^2)):drug, data = aids,
                                         random = ~ obstime + I(obstime^2) | patient,
                                         control = lmeControl(opt = "optim"))
                    }, message = "Fitting the model...")
                }
                htmlPrint2(
                    '# classical LRT with default chi-squared distribution',
                    anova(fm_s310_aids1, fm_s310_aids2),
                    '\n',
                    '# p-value from mixture of chi-squared distsributions',
                    mean(pchisq(6.343359, df = c(2,3), lower.tail = FALSE))
                )
            } else if (naf(input$test_lme) && input$test_lme == "AIC/BIC random effects") {
                fm_s310_pbc1 <- lme(log(serBilir) ~ ns(year, 2) * sex, data = pbc2, 
                                    random = ~ year | id)
                fm_s310_pbc2 <- lme(log(serBilir) ~ ns(year, 2) * sex, data = pbc2, 
                                    random = list(id = pdDiag(form = ~ ns(year, 2))))
                htmlPrint2(
                    '# AIC and BIC values',
                    anova(fm_s310_pbc1, fm_s310_pbc2)
                )
            } else if (naf(input$test_lme) && input$test_lme == "t-test regression coefficients") {
                if (!exists('fm_s310_pro1')) {
                    withProgress({
                        fm_s310_pro1 <<- lme(pro ~ ns(time, 3) * treat, data = prothro,
                                         random = list(id = pdDiag(form = ~ ns(time, 3))))
                    }, message = "Fitting the model...")
                }
                htmlPrint(summary(fm_s310_pro1))
            } else if (naf(input$test_lme) && input$test_lme == "F-test regression coefficients") {
                if (!exists('fm_s310_pro1')) {
                    withProgress({
                        fm_s310_pro1 <<- lme(pro ~ ns(time, 3) * treat, data = prothro,
                                         random = list(id = pdDiag(form = ~ ns(time, 3))))
                    }, message = "Fitting the model...")
                }
                htmlPrint2(
                    '# marginal F-tests',
                    anova(fm_s310_pro1, type = "marginal"),
                    "\n",
                    "# F-test for the overal time effect",
                    anova(fm_s310_pro1, Terms = c('ns(time, 3)', 'ns(time, 3):treat')),
                    "\n",
                    '# F-test for the overal treatment effect',
                    "# anova(fm_s310_pro1, Terms = c('treat', 'ns(time, 3):treat'))",
                    '# produces an error because all terms must have the same denominator DF'
                )
            } else if (naf(input$test_lme) && input$test_lme == "LRT regression coefficients") {
                if (!exists('fm_s310_pro1_ML')) {
                    withProgress({
                        fm_s310_pro1_ML <<- lme(pro ~ ns(time, 3) * treat, data = prothro,
                                    random = list(id = pdDiag(form = ~ ns(time, 3))),
                                    method = "ML")
                    }, message = "Fitting the model...")
                }
                if (!exists('fm_s310_pro2_ML')) {
                    withProgress({
                        fm_s310_pro2_ML <<- lme(pro ~ ns(time, 3), data = prothro,
                                    random = list(id = pdDiag(form = ~ ns(time, 3))),
                                    method = "ML")
                    }, message = "Fitting the model...")
                }
                htmlPrint2(
                    '# likelihood ratio test for the overal treatment effect',
                    anova(fm_s310_pro2_ML, fm_s310_pro1_ML)
                )
            }
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    #################
    # Section 3.11 #
    #################
    
    output$s311_plot_choice <- renderUI({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.11") {
            chs <- c("fitted", "time")
            cond_var <- "Condition on drug"
            fluidRow(column(4, radioButtons("s311_typePlot", "Type of plot:", c("Scatterplot", "QQnorm"))),
                     column(4, radioButtons("s311_type_res", "Residuals type:", c("Subject", "Marginal"))),
                     column(4, selectInput("s311_var_res", "Select variable:", chs, chs[1]),
                            checkboxInput("s311_drug", cond_var)))
        }
    })
    
    output$s311_code_plot <- renderText({
        if (input$chapter == "Chapter 3" && input$section == "Section 3.11") {
            if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot"
                && naf(input$s311_type_res) && input$s311_type_res == "Subject"
                && naf(input$s311_var_res) && input$s311_var_res == "fitted" && !input$s311_drug) {
                includeMarkdown("./md/s311_code_plot_ResPFitt.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot"
                       && naf(input$s311_type_res) && input$s311_type_res == "Subject"
                       && naf(input$s311_var_res) && input$s311_var_res == "fitted" && input$s311_drug) {
                includeMarkdown("./md/s311_code_plot_ResPFitt-treat.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot"
                       && naf(input$s311_type_res) && input$s311_type_res == "Subject"
                       && naf(input$s311_var_res) && input$s311_var_res == "time" && !input$s311_drug) {
                includeMarkdown("./md/s311_code_plot_ResPTime.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot"
                       && naf(input$s311_type_res) && input$s311_type_res == "Subject"
                       && naf(input$s311_var_res) && input$s311_var_res == "time" && input$s311_drug) {
                includeMarkdown("./md/s311_code_plot_ResPTime-treat.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "QQnorm"
                       && naf(input$s311_type_res) && input$s311_type_res == "Subject" && !input$s311_drug) {
                includeMarkdown("./md/s311_code_QQplot_ResP.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "QQnorm"
                       && naf(input$s311_type_res) && input$s311_type_res == "Subject" && input$s311_drug) {
                includeMarkdown("./md/s311_code_QQplot_ResP-treat.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot"
                       && naf(input$s311_type_res) && input$s311_type_res == "Marginal"
                       && naf(input$s311_var_res) && input$s311_var_res == "fitted" && !input$s311_drug) {
                includeMarkdown("./md/s311_code_plot_ResMargFitt.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot"
                       && naf(input$s311_type_res) && input$s311_type_res == "Marginal"
                       && naf(input$s311_var_res) && input$s311_var_res == "fitted" && input$s311_drug) {
                includeMarkdown("./md/s311_code_plot_ResMargFitt-treat.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot"
                       && naf(input$s311_type_res) && input$s311_type_res == "Marginal"
                       && naf(input$s311_var_res) && input$s311_var_res == "time" && !input$s311_drug) {
                includeMarkdown("./md/s311_code_plot_ResMargTime.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot"
                       && naf(input$s311_type_res) && input$s311_type_res == "Marginal"
                       && naf(input$s311_var_res) && input$s311_var_res == "time" && input$s311_drug) {
                includeMarkdown("./md/s311_code_plot_ResMargTime-treat.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "QQnorm"
                       && naf(input$s311_type_res) && input$s311_type_res == "Marginal"
                       && !input$s311_drug) {
                includeMarkdown("./md/s311_code_QQplot_ResMarg.Rmd")
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "QQnorm"
                       && naf(input$s311_type_res) && input$s311_type_res == "Marginal"
                       && input$s311_drug) {
                includeMarkdown("./md/s311_code_QQplot_ResMarg-treat.Rmd")
            }
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 4.1 #
    ###############
    
    output$s41_code_glm <- renderText({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.1") {
            includeMarkdown("./md/s41_code_glm.Rmd")
        }
    })
    
    output$s41_Routput_glm <- renderPrint({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.1") {
            fm_s41 <- glm(serCholD ~ age + sex + drug, data = pbc2.id, family = binomial)
            htmlPrint(summary(fm_s41))
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 4.3 #
    ###############
    
    output$s43_choice <- renderUI({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.3")
            radioButtons("fit_effPlt", "Select:", 
                         c("Model fit", "Effect plot"))
    })
    
    output$s43_Agechoice <- renderUI({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.3"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot")
            fluidRow(column(7, sliderInput("age_select_pbc_gee", "Age", min = 30, max = 65, 
                                           value = 49, animate = TRUE, step = 5)),
                     column(5, radioButtons('scale_s43', 'Scale', 
                                            c('log Odds', 'Probabilities'))))
    })
    
    output$s43_code_gee <- renderText({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.3") {
            if (naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot") {
                includeMarkdown("./md/s43_code_gee_effectPlot.Rmd")
            } else {
                includeMarkdown("./md/s43_code_gee.Rmd")
            }
        }
    })
    
    output$s43_Routput_gee <- renderPrint({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.3") {
            if (naf(input$fit_effPlt) && input$fit_effPlt == "Model fit") {
                fm_s43 <- geeglm(serCholD ~ year * drug, family = binomial, data = pbc2,
                                 id = id, corstr = "exchangeable")
                htmlPrint(summary(fm_s43))
            }
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 4.5 #
    ###############
    
    output$s45_choice <- renderUI({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.5")
            fluidRow(column(7, radioButtons("gee_Work_Corr", "Select:", 
                                            c("Coefficients", "Robust Standard Errors", 
                                              "Naive Standard Errors"))),
                     column(5, checkboxInput("s45_CoefPlot", "Coefficients' Plot")))
    })
    
    output$s45_code_gee <- renderText({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.5"
            && naf(input$s45_CoefPlot) && !input$s45_CoefPlot) {
            includeMarkdown("./md/s45_code_gee.Rmd")
        } else if (input$chapter == "Chapter 4" && input$section == "Section 4.5"
                      && input$s45_CoefPlot) {
            includeMarkdown("./md/s45_code_plot.Rmd")
        }
    })
    
    output$s45_Routput_gee <- renderPrint({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.5"
            && naf(input$gee_Work_Corr) && input$gee_Work_Corr == "Coefficients"
            && naf(input$s45_CoefPlot) && !input$s45_CoefPlot) {
            extractSEs <- function (model) sqrt(diag(model$geese$vbeta))
            extractSEs_naive <- function (model) sqrt(diag(model$geese$vbeta.naiv))
            aids$lowCD4 <- aids$CD4 < sqrt(150)
            aids$obstimef <- factor(aids$obstime)
            if (!exists('fm_s45_ind')) {
                withProgress({
                    fm_s45_ind <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "independence")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_exc')) {
                withProgress({
                    fm_s45_exc <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "exchangeable")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_ar1')) {
                withProgress({
                    fm_s45_ar1 <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "ar1")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_uns')) {
                withProgress({
                    fm_s45_uns <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "unstructured")
                }, message = "Fitting the model...")
            }
            htmlPrint2("# estimated coefficients",
                       round(cbind("independence" = coef(fm_s45_ind), 
                                   "exchangeable" = coef(fm_s45_exc),
                                   "AR1" = coef(fm_s45_ar1),
                                   "unstructured" = coef(fm_s45_uns)), 3))
        } else if (input$chapter == "Chapter 4" && input$section == "Section 4.5"
                   && naf(input$gee_Work_Corr) && 
                   input$gee_Work_Corr == "Robust Standard Errors"
                   && naf(input$s45_CoefPlot) && !input$s45_CoefPlot) {
            extractSEs <- function (model) sqrt(diag(model$geese$vbeta))
            extractSEs_naive <- function (model) sqrt(diag(model$geese$vbeta.naiv))
            aids$lowCD4 <- aids$CD4 < sqrt(150)
            aids$obstimef <- factor(aids$obstime)
            if (!exists('fm_s45_ind')) {
                withProgress({
                    fm_s45_ind <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "independence")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_exc')) {
                withProgress({
                    fm_s45_exc <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "exchangeable")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_ar1')) {
                withProgress({
                    fm_s45_ar1 <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "ar1")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_uns')) {
                withProgress({
                    fm_s45_uns <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "unstructured")
                }, message = "Fitting the model...")
            }
            htmlPrint2("# Sandwich/Robust Standard Errors",
                       round(cbind("independence" = extractSEs(fm_s45_ind), 
                                   "exchangeable" = extractSEs(fm_s45_exc),
                                   "AR1" = extractSEs(fm_s45_ar1),
                                   "unstructured" = extractSEs(fm_s45_uns)), 3))
        } else if (input$chapter == "Chapter 4" && input$section == "Section 4.5"
                   && naf(input$gee_Work_Corr) 
                   && input$gee_Work_Corr == "Naive Standard Errors"
                   && naf(input$s45_CoefPlot) && !input$s45_CoefPlot) {
            extractSEs <- function (model) sqrt(diag(model$geese$vbeta))
            extractSEs_naive <- function (model) sqrt(diag(model$geese$vbeta.naiv))
            aids$lowCD4 <- aids$CD4 < sqrt(150)
            aids$obstimef <- factor(aids$obstime)
            if (!exists('fm_s45_ind')) {
                withProgress({
                    fm_s45_ind <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "independence")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_exc')) {
                withProgress({
                    fm_s45_exc <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "exchangeable")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_ar1')) {
                withProgress({
                    fm_s45_ar1 <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "ar1")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_uns')) {
                withProgress({
                    fm_s45_uns <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "unstructured")
                }, message = "Fitting the model...")
            }
            htmlPrint2("# Naive/Model-based Standard Errors",
                round(cbind("independence" = extractSEs_naive(fm_s45_ind), 
                            "exchangeable" = extractSEs_naive(fm_s45_exc),
                            "AR1" = extractSEs_naive(fm_s45_ar1),
                            "unstructured" = extractSEs_naive(fm_s45_uns)), 3))
        }
     })
    
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 4.6 #
    ###############
    
    output$s46_choice <- renderUI({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.6")
            radioButtons("test_Type", "Select:", 
                         c("simple Wald test", "complex effects"))
    })
    
    
    output$s46_code_gee <- renderText({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.6"
            && naf(input$test_Type) &&input$test_Type == "simple Wald test") {
            includeMarkdown("./md/s46_code_wald.Rmd")
        } else if (input$chapter == "Chapter 4" && input$section == "Section 4.6"
                   && naf(input$test_Type) &&input$test_Type == "complex effects") {
            includeMarkdown("./md/s46_code_gee.Rmd")
        }
    })
    
    output$s46_Routput_gee <- renderPrint({
        if (input$chapter == "Chapter 4" && input$section == "Section 4.6"
            && naf(input$test_Type) && input$test_Type == "simple Wald test") {
            aids$lowCD4 <- aids$CD4 < sqrt(150)
            aids$obstimef <- factor(aids$obstime)
            fm_s46_aids1 <- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                   id = patient, corstr = "ar1")
            fm_s46_aids2 <- geeglm(lowCD4 ~ obstimef * drug, family = binomial, data = aids, 
                                   id = patient, corstr = "ar1")
            htmlPrint2("# Wald test for overall treatment effect",
                anova(fm_s46_aids1, fm_s46_aids2)
            )
        } else if (input$chapter == "Chapter 4" && input$section == "Section 4.6"
                   && naf(input$test_Type) && input$test_Type == "complex effects") {
            fm_s46_pbc <- geeglm(serCholD ~ ns(year, 3) * drug + ns(age, 3), family = binomial, 
                                 data = pbc2, id = id, corstr = "exchangeable")
            rowDiff <- function (object, newdata, orig_data, adjust.p = FALSE, ...) {
                form <- formula(object)
                namesVars <- all.vars(form)
                respVar <- namesVars[1]
                newdata[[respVar]] <- 0.01
                betas <- if (!inherits(object, "lme")) coef(object) else fixef(object)
                V <- if (inherits(object, "geeglm")) object$geese$vbeta else vcov(object)
                orig_data <- orig_data[complete.cases(orig_data[namesVars]), ]
                mfX <- model.frame(terms(form), data = orig_data)
                X <- model.matrix(attr(mfX, "terms"), newdata)
                ind <- combn(nrow(X), 2)
                k <- ncol(ind)
                out <- matrix(0, k, 5)
                for (i in seq_len(k)) {
                    XX <- X[ind[1, i], , drop = FALSE] - X[ind[2, i], , drop = FALSE]
                    est <- drop(XX %*% betas)
                    se <- sqrt(diag(XX %*% V %*% t(XX)))
                    out[i, 1] <- est
                    out[i, 2] <- se
                    out[i, 3] <- est - 1.96 * se
                    out[i, 4] <- est + 1.96 * se
                    out[i, 5] <- 2 * pnorm(abs(est / se), lower.tail = FALSE)
                }
                if (k > 2 && adjust.p) {
                    out[, 5] <- p.adjust(out[, 5], ...)
                }
                colnames(out) <- c("Diff", "Std.Err.", "95%low", "95%upp", "p-value")
                rownames(out) <- paste(ind[1, ], "-", ind[2, ])
                out
            }
            
            htmlPrint2("# parameters do not have a straightforward intepretation",
                       summary(fm_s46_pbc)
            )
            
            nDF <- with(pbc2, expand.grid(
                year = 7, drug = levels(drug), age = 49
            ))
            
            htmlPrint2("# we want to test treatment differences for 49 year old males",
                       "# at year 7 -- we create the corresponding data frame",
                       nDF,
                       "\n",
                       "# we compute the difference of the two rows;",
                       "# in this case this the log odds ratio between",
                       "# the aforementioned patients",
                       rowDiff(fm_s46_pbc, nDF, pbc2)
            )
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 5.2 #
    ###############
    
    output$s52_choice <- renderUI({
        if (input$chapter == "Chapter 5" && input$section == "Section 5.2")
            radioButtons("fit_effPlt", "Select:", 
                         c("Model fit (AIDS data)", "Effect plot (PBC data)"))
    })
    
    output$s52_parms <- renderUI({
        if (input$chapter == "Chapter 5" && input$section == "Section 5.2"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Model fit (AIDS data)")
            radioButtons('parms_s52', 'Type Coefficients', c('subject-specific', 'marginal'))
    })
    
    
    output$s52_Agechoice <- renderUI({
        if (input$chapter == "Chapter 5" && input$section == "Section 5.2"
            && naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot (PBC data)")
            fluidRow(column(7, sliderInput("age_select_pbc_glmm", "Age", min = 30, max = 65, 
                                           value = 49, animate = TRUE, step = 5)),
                     column(5, radioButtons('scale_s52', 'Scale', 
                                            c('log Odds', 'Marginal Probs'))))
    })
    
    output$s52_code_glmm <- renderText({
        if (input$chapter == "Chapter 5" && input$section == "Section 5.2") {
            if (naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot (PBC data)") {
                includeMarkdown("./md/s52_code_glmm_effectPlot.Rmd")
            } else {
                includeMarkdown("./md/s52_code_glmm.Rmd")
            }
        }
    })
    
    output$s52_Routput_glmm <- renderPrint({
        if (input$chapter == "Chapter 5" && input$section == "Section 5.2") {
            if (naf(input$fit_effPlt) && input$fit_effPlt == "Model fit (AIDS data)") {
                aids$lowCD4 <- aids$CD4 < sqrt(150)
                if (!exists("fm_s52_aids")) {
                    withProgress({
                        fm_s52_aids <<- mixed_model(lowCD4 ~ obstime * drug,
                                                    random = ~ 1 | patient, 
                                                    family = binomial(), data = aids, 
                                                    nAGQ = 13)
                    }, message = 'Fitting the model...')
                }
                if (input$parms_s52 == 'marginal' && !exists("mcoefs_fm_s52_aids")) {
                    withProgress({
                        mcoefs_fm_s52_aids <<- marginal_coefs(fm_s52_aids, 
                                                              std_errors = TRUE,
                                                              cores = 5)[["coef_table"]]
                    }, message = 'Calculating standard errors...')
                }
                switch(input$parms_s52, 
                       'subject-specific' = htmlPrint(summary(fm_s52_aids)),
                       'marginal' = htmlPrint(round(mcoefs_fm_s52_aids, 4))
                )
            }
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 5.3 #
    ###############
    
    output$s53_code_glmm <- renderText({
        if (input$chapter == "Chapter 5" && input$section == "Section 5.3") {
                includeMarkdown("./md/s53_code_glmm.Rmd")
        }
    })
    
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 5.6 #
    ###############
    
    output$s56_code_glmm <- renderText({
        if (input$chapter == "Chapter 5" && input$section == "Section 5.6") {
            includeMarkdown("./md/s56_code_glmm.Rmd")
        }
    })
    
    output$s56_Routput_glmm <- renderPrint({
        if (input$chapter == "Chapter 5" && input$section == "Section 5.6") {
            if (!exists("fm_s56_alt")) {
                withProgress({
                    fm_s56_alt <<- mixed_model(serCholD ~ year * drug + sex + drug:sex,
                                               random = ~ 1 | id, 
                                               family = binomial(), data = pbc2, nAGQ = 15)
                }, message = 'Fitting the model...')
            }
            if (!exists("fm_s56_null")) {
                withProgress({
                    fm_s56_null <<- mixed_model(serCholD ~ year + drug + sex,
                                                random = ~ 1 | id, 
                                                family = binomial(), data = pbc2, 
                                                nAGQ = 15)
                }, message = 'Fitting the model...')
            }
            htmlPrint2("# Likelihood ratio test for all interaction terms",
                       anova(fm_s56_null, fm_s56_alt)
            )
        }
    })
    
    ######################################################################################
    ######################################################################################
    
    ###############
    # Section 6.3 #
    ###############
    
    output$s63_choice <- renderUI({
        if (input$chapter == "Chapter 6" && input$section == "Section 6.3")
            fluidRow(column(6, radioButtons("imp_choice", "Syntax for:", 
                                            c("Create Data", "Complete Cases", "LOCF", 
                                              "Mean Imputation", "Analysis"))),
                     column(6, radioButtons("data_plot", "Show:", c("Data", "Dropout Patterns",
                                                                    "Boxplot CD4", 
                                                                    "Results", 
                                                                    "Coefficients' Plot"))))
    })
    
    output$s63_code <- renderText({
        if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
            && naf(input$imp_choice) &&input$imp_choice == "Create Data"
            && naf(input$data_plot) && input$data_plot %in% c("Data", "Dropout Patterns", 
                                                              "Boxplot CD4", "Results", "Coefficients' Plot")) {
            includeMarkdown("./md/s63_code_CompleteData.Rmd")
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "Complete Cases"
                   && naf(input$data_plot) && input$data_plot %in% c("Data", "Dropout Patterns", 
                                                                     "Boxplot CD4", "Results", "Coefficients' Plot")) {
            includeMarkdown("./md/s63_code_cc.Rmd")
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "LOCF"
                   && naf(input$data_plot) && input$data_plot %in% c("Data", "Dropout Patterns", 
                                                                     "Boxplot CD4", "Results", "Coefficients' Plot")) {
            includeMarkdown("./md/s63_code_locf.Rmd")
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "Mean Imputation"
                   && naf(input$data_plot) && input$data_plot %in% c("Data", "Dropout Patterns",
                                                                     "Boxplot CD4", "Results", "Coefficients' Plot")) {
            includeMarkdown("./md/s63_code_meanImp.Rmd")
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "Analysis"
                   && naf(input$data_plot) && input$data_plot %in% c("Data", "Results")) {
            includeMarkdown("./md/s63_code_analysis.Rmd")
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "Analysis"
                   && naf(input$data_plot) && input$data_plot == "Dropout Patterns") {
            includeMarkdown("./md/s63_code_patterns.Rmd")
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "Analysis"
                   && naf(input$data_plot) && input$data_plot == "Boxplot CD4") {
            includeMarkdown("./md/s63_code_boxplot.Rmd")
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "Analysis"
                   && naf(input$data_plot) && input$data_plot == "Coefficients' Plot") {
            includeMarkdown("./md/s63_code_coefplot.Rmd")
        }
    })
    
    output$s63_Routput_table <- renderDataTable({
        if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
            && naf(input$imp_choice) &&input$imp_choice == "Create Data"
            && naf(input$data_plot) && input$data_plot == "Data") {
            aids_missings <- aids[c('patient', 'CD4', 'obstime', 'AZT', 'prevOI')]
            planned_visits <- c(0, 2, 6, 12, 18)
            data_patient <- split(aids_missings, aids_missings$patient)
            aids_missings <- do.call(rbind, lapply(data_patient, function (d) {
                out <- d[rep(1, length(planned_visits)), ]
                out$CD4 <- rep(NA, nrow(out))
                out$CD4[match(d$obstime, planned_visits)] <- d$CD4
                out$obstime <- planned_visits
                out
            }))
            row.names(aids_missings) <- seq_len(nrow(aids_missings))
            aids_missings
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "Complete Cases"
                   && naf(input$data_plot) && input$data_plot == "Data") {
            aids_missings <- aids[c('patient', 'CD4', 'obstime', 'AZT', 'prevOI')]
            planned_visits <- c(0, 2, 6, 12, 18)
            data_patient <- split(aids_missings, aids_missings$patient)
            aids_missings <- do.call(rbind, lapply(data_patient, function (d) {
                out <- d[rep(1, length(planned_visits)), ]
                out$CD4 <- rep(NA, nrow(out))
                out$CD4[match(d$obstime, planned_visits)] <- d$CD4
                out$obstime <- planned_visits
                out
            }))
            row.names(aids_missings) <- seq_len(nrow(aids_missings))
            length.noNA <- function (x) sum(!is.na(x))
            index <- with(aids_missings, ave(CD4, patient, FUN = length.noNA))
            aids_missings$CD4cc <- aids_missings$CD4
            aids_missings$CD4cc[index < 5] <- NA
            aids_missings[c('patient', 'CD4', 'CD4cc', 'obstime', 'AZT', 'prevOI')]
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "LOCF"
                   && naf(input$data_plot) && input$data_plot == "Data") {
            aids_missings <- aids[c('patient', 'CD4', 'obstime', 'AZT', 'prevOI')]
            planned_visits <- c(0, 2, 6, 12, 18)
            data_patient <- split(aids_missings, aids_missings$patient)
            aids_missings <- do.call(rbind, lapply(data_patient, function (d) {
                out <- d[rep(1, length(planned_visits)), ]
                out$CD4 <- rep(NA, nrow(out))
                out$CD4[match(d$obstime, planned_visits)] <- d$CD4
                out$obstime <- planned_visits
                out
            }))
            row.names(aids_missings) <- seq_len(nrow(aids_missings))
            locf <- function (x) {
                na.ind <- is.na(x)
                noNA_x <- x[!na.ind]
                idx <- cumsum(!na.ind)
                noNA_x[idx]
            }
            aids_missings$CD4locf <- with(aids_missings, ave(CD4, patient, FUN = locf))
            aids_missings[c('patient', 'CD4', 'CD4locf', 'obstime', 'AZT', 'prevOI')]
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "Mean Imputation"
                   && naf(input$data_plot) && input$data_plot == "Data") {
            aids_missings <- aids[c('patient', 'CD4', 'obstime', 'AZT', 'prevOI')]
            planned_visits <- c(0, 2, 6, 12, 18)
            data_patient <- split(aids_missings, aids_missings$patient)
            aids_missings <- do.call(rbind, lapply(data_patient, function (d) {
                out <- d[rep(1, length(planned_visits)), ]
                out$CD4 <- rep(NA, nrow(out))
                out$CD4[match(d$obstime, planned_visits)] <- d$CD4
                out$obstime <- planned_visits
                out
            }))
            row.names(aids_missings) <- seq_len(nrow(aids_missings))
            means <- with(aids_missings, tapply(CD4, obstime, mean, na.rm = TRUE))
            mean_imp <- function (x) {
                na.ind <- is.na(x)
                x[na.ind] <- means[na.ind]
                x
            }
            aids_missings$CD4mean_imp <- with(aids_missings, ave(CD4, patient, FUN = mean_imp))
            aids_missings[c('patient', 'CD4', 'CD4mean_imp', 'obstime', 'AZT', 'prevOI')]
        } else if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
                   && naf(input$imp_choice) &&input$imp_choice == "Analysis"
                   && naf(input$data_plot) && input$data_plot == "Data") {
            aids_missings <- aids[c('patient', 'CD4', 'obstime', 'AZT', 'prevOI')]
            planned_visits <- c(0, 2, 6, 12, 18)
            data_patient <- split(aids_missings, aids_missings$patient)
            aids_missings <- do.call(rbind, lapply(data_patient, function (d) {
                out <- d[rep(1, length(planned_visits)), ]
                out$CD4 <- rep(NA, nrow(out))
                out$CD4[match(d$obstime, planned_visits)] <- d$CD4
                out$obstime <- planned_visits
                out
            }))
            row.names(aids_missings) <- seq_len(nrow(aids_missings))
            length.noNA <- function (x) sum(!is.na(x))
            index <- with(aids_missings, ave(CD4, patient, FUN = length.noNA))
            aids_missings$CD4cc <- aids_missings$CD4
            aids_missings$CD4cc[index < 5] <- NA
            locf <- function (x) {
                na.ind <- is.na(x)
                noNA_x <- x[!na.ind]
                idx <- cumsum(!na.ind)
                noNA_x[idx]
            }
            aids_missings$CD4locf <- with(aids_missings, ave(CD4, patient, FUN = locf))
            means <- with(aids_missings, tapply(CD4, obstime, mean, na.rm = TRUE))
            mean_imp <- function (x) {
                na.ind <- is.na(x)
                x[na.ind] <- means[na.ind]
                x
            }
            aids_missings$CD4mean_imp <- with(aids_missings, ave(CD4, patient, FUN = mean_imp))
            aids_missings[c('patient', 'CD4', 'CD4cc', 'CD4locf', 'CD4mean_imp', 'obstime', 
                            'AZT', 'prevOI')]
        }
    })
    
    output$s63_Routput <- renderPrint({
        if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
            && naf(input$data_plot) &&input$data_plot == "Results") {
            aids_missings <- aids[c('patient', 'CD4', 'obstime', 'AZT', 'prevOI')]
            planned_visits <- c(0, 2, 6, 12, 18)
            data_patient <- split(aids_missings, aids_missings$patient)
            aids_missings <- do.call(rbind, lapply(data_patient, function (d) {
                out <- d[rep(1, length(planned_visits)), ]
                out$CD4 <- rep(NA, nrow(out))
                out$CD4[match(d$obstime, planned_visits)] <- d$CD4
                out$obstime <- planned_visits
                out
            }))
            row.names(aids_missings) <- seq_len(nrow(aids_missings))
            ##############
            length.noNA <- function (x) sum(!is.na(x))
            index <- with(aids_missings, ave(CD4, patient, FUN = length.noNA))
            aids_missings$CD4cc <- aids_missings$CD4
            aids_missings$CD4cc[index < 5] <- NA
            ##############
            locf <- function (x) {
                na.ind <- is.na(x)
                noNA_x <- x[!na.ind]
                idx <- cumsum(!na.ind)
                noNA_x[idx]
            }
            aids_missings$CD4locf <- with(aids_missings, ave(CD4, patient, FUN = locf))
            ##############
            means <- with(aids_missings, tapply(CD4, obstime, mean, na.rm = TRUE))
            mean_imp <- function (x) {
                na.ind <- is.na(x)
                x[na.ind] <- means[na.ind]
                x
            }
            aids_missings$CD4mean_imp <- with(aids_missings, ave(CD4, patient, FUN = mean_imp))
            ##############
            if (!exists("fm_s63_aids1")) {
                withProgress({
                    fm_s63_aids1 <<- lme(CD4 ~ obstime * (AZT + prevOI), data = aids_missings,
                                         random = ~ obstime | patient, na.action = na.exclude)
                }, message = 'Fitting the model...')
            }
            if (!exists("fm_s63_aids2")) {
                withProgress({
                    fm_s63_aids2 <<- lme(CD4cc ~ obstime * (AZT + prevOI), data = aids_missings,
                                         random = ~ obstime | patient, na.action = na.exclude)
                }, message = 'Fitting the model...')
            }
            if (!exists("fm_s63_aids3")) {
                withProgress({
                    fm_s63_aids3 <<- lme(CD4locf ~ obstime * (AZT + prevOI), data = aids_missings,
                                         random = ~ obstime | patient)
                }, message = 'Fitting the model...')
            }
            if (!exists("fm_s63_aids4")) {
                withProgress({
                    fm_s63_aids4 <<- lme(CD4mean_imp ~ obstime * (AZT + prevOI), data = aids_missings,
                                         random = ~ obstime | patient, control = lmeControl(opt = "optim"))
                }, message = 'Fitting the model...')
            }
            htmlPrint2("# fixed effects from the three models",
                       cbind("Available Cases" = fixef(fm_s63_aids1),
                             "Complete Cases" = fixef(fm_s63_aids2),
                             "LOCF" = fixef(fm_s63_aids3),
                             "Mean Imputation" = fixef(fm_s63_aids4)))
        }
    })
    ######################################################################################
    ######################################################################################
    
    ###########
    # Quizzes #
    ###########
    
    show_Quiz1_Qns <- reactiveValues(Q1 = TRUE, Q2 = FALSE, Q3 = FALSE, Q4 = FALSE, Q5 = FALSE, Q6 = FALSE)
    show_Quiz2_Qns <- reactiveValues(Q1 = TRUE, Q2 = FALSE, Q3 = FALSE, Q4 = FALSE, Q5 = FALSE)
    show_Quiz3_Qns <- reactiveValues(Q1 = TRUE, Q2 = FALSE, Q3 = FALSE, Q4 = FALSE, Q5 = FALSE)
    show_Quiz4_Qns <- reactiveValues(Q1 = TRUE, Q2 = FALSE, Q3 = FALSE, Q4 = FALSE, Q5 = FALSE)
    show_Quiz5_Qns <- reactiveValues(Q1 = TRUE, Q2 = FALSE, Q3 = FALSE, Q4 = FALSE, Q5 = FALSE)
    show_Ans <- reactiveValues(show = FALSE)
    
    
    output$sQuiz_question <- renderText({
        if (input$chapter == "Quizzes" && input$section == "Quiz 1") {
            show_Ans$show <- FALSE
            if (show_Quiz1_Qns$Q1) {
                includeHTML("./html/Quiz1_Q1.Rhtml")
            } else if (show_Quiz1_Qns$Q2) {
                includeHTML("./html/Quiz1_Q2.Rhtml")
            } else if (show_Quiz1_Qns$Q3) {
                includeHTML("./html/Quiz1_Q3.Rhtml")
            } else if (show_Quiz1_Qns$Q4) {
                includeHTML("./html/Quiz1_Q4.Rhtml")
            } else if (show_Quiz1_Qns$Q5) {
                includeHTML("./html/Quiz1_Q5.Rhtml")
            } else if (show_Quiz1_Qns$Q6) {
                includeHTML("./html/Quiz1_Q6.Rhtml")
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 2") {
            show_Ans$show <- FALSE
            if (show_Quiz2_Qns$Q1) {
                includeHTML("./html/Quiz2_Q1.Rhtml")
            } else if (show_Quiz2_Qns$Q2) {
                includeHTML("./html/Quiz2_Q2.Rhtml")
            } else if (show_Quiz2_Qns$Q3) {
                includeHTML("./html/Quiz2_Q3.Rhtml")
            } else if (show_Quiz2_Qns$Q4) {
                includeHTML("./html/Quiz2_Q4.Rhtml")
            } else if (show_Quiz2_Qns$Q5) {
                includeHTML("./html/Quiz2_Q5.Rhtml")
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 3") {
            show_Ans$show <- FALSE
            if (show_Quiz3_Qns$Q1) {
                includeHTML("./html/Quiz3_Q1.Rhtml")
            } else if (show_Quiz3_Qns$Q2) {
                includeHTML("./html/Quiz3_Q2.Rhtml")
            } else if (show_Quiz3_Qns$Q3) {
                includeHTML("./html/Quiz3_Q3.Rhtml")
            } else if (show_Quiz3_Qns$Q4) {
                includeHTML("./html/Quiz3_Q4.Rhtml")
            } else if (show_Quiz3_Qns$Q5) {
                includeHTML("./html/Quiz3_Q5.Rhtml")
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 4") {
            show_Ans$show <- FALSE
            if (show_Quiz4_Qns$Q1) {
                includeHTML("./html/Quiz4_Q1.Rhtml")
            } else if (show_Quiz4_Qns$Q2) {
                includeHTML("./html/Quiz4_Q2.Rhtml")
            } else if (show_Quiz4_Qns$Q3) {
                includeHTML("./html/Quiz4_Q3.Rhtml")
            } else if (show_Quiz4_Qns$Q4) {
                includeHTML("./html/Quiz4_Q4.Rhtml")
            } else if (show_Quiz4_Qns$Q5) {
                includeHTML("./html/Quiz4_Q5.Rhtml")
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 5") {
            show_Ans$show <- FALSE
            if (show_Quiz5_Qns$Q1) {
                includeHTML("./html/Quiz5_Q1.Rhtml")
            } else if (show_Quiz5_Qns$Q2) {
                includeHTML("./html/Quiz5_Q2.Rhtml")
            } else if (show_Quiz5_Qns$Q3) {
                includeHTML("./html/Quiz5_Q3.Rhtml")
            } else if (show_Quiz5_Qns$Q4) {
                includeHTML("./html/Quiz5_Q4.Rhtml")
            } else if (show_Quiz5_Qns$Q5) {
                includeHTML("./html/Quiz5_Q5.Rhtml")
            }
        }
    })
    
    answers <- reactive({
        if (input$chapter == "Quizzes" && input$section == "Quiz 1") {
            opt1 <- c("Every unit increase in BMI decreases LDL by 0.07. This is for patient receiving the same treatment and this effect does not remain constant during follow-up.", 
                      "Every unit increase in BMI decreases LDL by 0.07. This is for patient receiving the same treatment and this effect remains constant during follow-up.",
                      "Every unit increase in BMI increases LDL by 0.07. This is for patient receiving the same treatment and this effect does not remain constant during follow-up.", 
                      "Every unit increase in BMI increases LDL by 0.07. This is for patient receiving the same treatment and this effect remains constant during follow-up.")
            
            opt2 <- c("Perform a likelihood ratio test between the two models because they are nested.", 
                      "Perform a t-test for the regression coefficients of Model I and Model II and see if the p-values differ.",
                      "Perform an F-test for the regression coefficients of Model I and Model II and see if the p-values differ.",
                      "Use AIC or BIC to compare the two models because they are not nested.")
            
            opt3 <- c("Yes, because the scores are a continuous outcome and because this model does account for the correlations between children in the same school.", 
                      "No, because even though the outcome is continuous, the linear regression model does not account for the correlations between children in the same school.",
                      "Yes, because the scores are a continuous outcome and because it is not required to account for the correlations between children in the same school.")
            
            opt4 <- c("We should include the time variable into the model, and test if it is signigicant.",
                      "We should investigate graphically the relationship between PSA and time, and decide if we should include nonlinear terms.",
                      "We should inlcude splines of time into the model and investigate if the log-likelihood increases.",
                      "We should fit two models, one with splines and the other with a linear time effect, and do an likelihood ratio test.")
            
            opt5 <- c("The error terms are normally distributed with a diagonal covariance matrix.",
                      "The error terms are normally distributed and have constant variance in time.",
                      "The error terms are normally distributed and are uncorrelated.",
                      "The error terms are normally distributed with a general covariance matrix.")
            
            opt6 <- c("Statement 1 is correct.", 
                      "Statement 2 is correct.",
                      "Statement 3 is correct.",
                      "Statements 1 and 2 are correct.",
                      "Statements 1 and 3 are correct.",
                      "Statements 2 and 3 are correct.",
                      "None of them is correct.")
            
            chs1 <- lapply(seq_along(opt1), c); names(chs1) <- opt1
            chs2 <- lapply(seq_along(opt2), c); names(chs2) <- opt2
            chs3 <- lapply(seq_along(opt3), c); names(chs3) <- opt3
            chs4 <- lapply(seq_along(opt4), c); names(chs4) <- opt4
            chs5 <- lapply(seq_along(opt5), c); names(chs5) <- opt5
            chs6 <- lapply(seq_along(opt6), c); names(chs6) <- opt6
            
            correct <- c(2, 4, 2, 4, 4, 3)
            
            cols1 <- rep("red", length(opt1)); cols1[correct[1L]] <- "green"
            cols2 <- rep("red", length(opt2)); cols2[correct[2L]] <- "green"
            cols3 <- rep("red", length(opt3)); cols3[correct[3L]] <- "green"
            cols4 <- rep("red", length(opt4)); cols4[correct[4L]] <- "green"
            cols5 <- rep("red", length(opt5)); cols5[correct[5L]] <- "green"
            cols6 <- rep("red", length(opt6)); cols6[correct[6L]] <- "green"
            
            list(chs1 = chs1, chs2 = chs2, chs3 = chs3, chs4 = chs4, chs5 = chs5, chs6 = chs6,
                 cols1 = cols1, cols2 = cols2, cols3 = cols3, cols4 = cols4, cols5 = cols5, cols6 = cols6)
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 2") {
            opt1 <- c("Linear mixed models aim only at the population level.",
                      "Linear mixed models aim only at the subjects level.",
                      "Linear mixed models aim at both the population and the subjects level.",
                      "None of the above.")
            
            opt2 <- c("As the name suggests, linear mixed models can only be used with linear time effects.",
                      "In linear mixed models we need to use a combination of linear and nonlinear time effects.",
                      "Depending on the features of the data we may or may not include nonlinear effects.",
                      " In linear mixed models we typically need to include nonlinear time effects.")
            
            opt3 <- c("Assuming conditional independence means that some observed subject characteristics explain the correlation in the measurements of each subject.",
                      "Assuming conditional independence means that we assume that some of the measurements of each subject are dependent and some independent.",
                      "Assuming conditional independence means that we assume that measurements of each subject are independent.",
                      "Assuming conditional independence means that some unobserved subject characteristics explain the correlation in the measurements of each subject.")
            
            opt4 <- c("When we only include random intercepts the variance decreases over time, whereas when include both random intercepts and random slopes it increases in time.",
                      "When we only include random intercepts the correlation between the repeated measurements remains constant in time, whereas when we include both random intercepts and random slopes it changes over time.",
                      "When we only include random intercepts the variance increases over time, whereas when include both random intercepts and random slopes it decreases in time.",
                      "When we include only random intercepts the correlation between the repeated measurements decreases over time, whereas when we include both random intercepts and random slopes it remains constant in time.")
            
            opt5 <- c("Only argument 1 is correct.", "Only argument 2 is correct.", "Only argument 3 is correct.",
                      "Only arguments 1 and 2 are correct.", "Only arguments 1 and 3 are correct.", "Only arguments 2 and 3 are correct.",
                      "All arguments are correct.", "None of the arguments is correct.")
            
            
            chs1 <- lapply(seq_along(opt1), c); names(chs1) <- opt1
            chs2 <- lapply(seq_along(opt2), c); names(chs2) <- opt2
            chs3 <- lapply(seq_along(opt3), c); names(chs3) <- opt3
            chs4 <- lapply(seq_along(opt4), c); names(chs4) <- opt4
            chs5 <- lapply(seq_along(opt5), c); names(chs5) <- opt5
            
            correct <- c(3, 3, 4, 2, 8)
            
            cols1 <- rep("red", length(opt1)); cols1[correct[1L]] <- "green"
            cols2 <- rep("red", length(opt2)); cols2[correct[2L]] <- "green"
            cols3 <- rep("red", length(opt3)); cols3[correct[3L]] <- "green"
            cols4 <- rep("red", length(opt4)); cols4[correct[4L]] <- "green"
            cols5 <- rep("red", length(opt5)); cols5[correct[5L]] <- "green"
            
            list(chs1 = chs1, chs2 = chs2, chs3 = chs3, chs4 = chs4, chs5 = chs5,
                 cols1 = cols1, cols2 = cols2, cols3 = cols3, cols4 = cols4, cols5 = cols5)
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 3") {
            opt1 <- c("GEE only work with balanced data sets.",
                      "The naive standard errors can be calculated with unbalanced data sets, but the sandwich estimator does not.",
                      "The sandwich estimator can calculated with unbalanced data sets, but the naive standard errors do not.",
                      "Both the sandwich estimator and the naive standard errors can be calculated with unbalanced data sets.")
            
            opt2 <- c("GEE can only be used with categorical repeated measurements data that follow either the Bernoulli/Binomial or the Poisson distributions.",
                      "GEE can only be used with both continuous repeated measurements data.", 
                      "GEE can be used with both continuous and categorical repeated measurements data.",
                      "GEE can only be used with categorical repeated measurements data.")
            
            opt3 <- c("A compound symmetry working correlation matrix.",
                      "An unstructured working correlation matrix.",
                      "An AR1 working correlation matrix.",
                      "An independence working correlation matrix.")
            
            opt4 <- c("The working correlation matrix does not need to be correctly specified for the Wald test to be valid when you use the naive standard errors.",
                      "The working correlation matrix needs to be correctly specified otherwise the Wald test for a GEE is not valid.",
                      "The working correlation matrix does not need to be correctly specified for the sandwich estimator to work.",
                      "The working correlation matrix needs to be correctly specified for the sandwich estimator to work.")
            
            opt5 <- c("The likelihood ratio test.", "The BIC criterion.",
                      "The AIC criterion.", "The Wald test.")
            
            chs1 <- lapply(seq_along(opt1), c); names(chs1) <- opt1
            chs2 <- lapply(seq_along(opt2), c); names(chs2) <- opt2
            chs3 <- lapply(seq_along(opt3), c); names(chs3) <- opt3
            chs4 <- lapply(seq_along(opt4), c); names(chs4) <- opt4
            chs5 <- lapply(seq_along(opt5), c); names(chs5) <- opt5
            
            correct <- c(4, 3, 3, 3, 4)
            
            cols1 <- rep("red", length(opt1)); cols1[correct[1L]] <- "green"
            cols2 <- rep("red", length(opt2)); cols2[correct[2L]] <- "green"
            cols3 <- rep("red", length(opt3)); cols3[correct[3L]] <- "green"
            cols4 <- rep("red", length(opt4)); cols4[correct[4L]] <- "green"
            cols5 <- rep("red", length(opt5)); cols5[correct[5L]] <- "green"
            
            list(chs1 = chs1, chs2 = chs2, chs3 = chs3, chs4 = chs4, chs5 = chs5,
                 cols1 = cols1, cols2 = cols2, cols3 = cols3, cols4 = cols4, cols5 = cols5)
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 4") {
            opt1 <- c("A GEE analysis with an AR1 working correlation matrix.",
                      "A GEE analysis with an independent working correlation matrix.",
                      "A GLMM analysis with random intercepts and random slopes.",
                      "A GLMM analysis with random intercepts and random slopes, and test whether quadratic random slopes would be required.")
            
            opt2 <- c("A GEE analysis with a compound symmetry working correlation matrix.",
                      "A GLMM analysis with random intercepts.",
                      "A GLMM analysis with an appropriately chosen random-effects structure.",
                      "A GEE analysis with an AR1 working correlation matrix.",
                      "A GLMM analysis with random intercepts and random slopes.",
                      "A GEE analysis with an independent working correlation matrix.")
            
            opt3 <- c("Penalized Quasi Likelihood",
                      "adaptive Gaussian quadrature",
                      " Laplace approximation",
                      "None of the above is a good option for dichotomous data.")
            
            opt4 <- c("A Markov chain Monte Carlo (MCMC) approach",
                      "Penalized Quasi Likelihood",
                      "Laplace approximation",
                      "None of the above is a good option for Poisson data with low expected counts.")
            
            opt5 <- c("Fit the models using REML, and compare them using a likelihood ratio test.",
                      "Compare the two models using a Wald test.",
                      "Select the model that has the lowest BIC value.",
                      "Fit the models using ML, and compare them using a likelihood ratio test.")
            
            chs1 <- lapply(seq_along(opt1), c); names(chs1) <- opt1
            chs2 <- lapply(seq_along(opt2), c); names(chs2) <- opt2
            chs3 <- lapply(seq_along(opt3), c); names(chs3) <- opt3
            chs4 <- lapply(seq_along(opt4), c); names(chs4) <- opt4
            chs5 <- lapply(seq_along(opt5), c); names(chs5) <- opt5
            
            correct <- c(1, 3, 2, 1, 3)
            
            cols1 <- rep("red", length(opt1)); cols1[correct[1L]] <- "green"
            cols2 <- rep("red", length(opt2)); cols2[correct[2L]] <- "green"
            cols3 <- rep("red", length(opt3)); cols3[correct[3L]] <- "green"
            cols4 <- rep("red", length(opt4)); cols4[correct[4L]] <- "green"
            cols5 <- rep("red", length(opt5)); cols5[correct[5L]] <- "green"
            
            list(chs1 = chs1, chs2 = chs2, chs3 = chs3, chs4 = chs4, chs5 = chs5,
                 cols1 = cols1, cols2 = cols2, cols3 = cols3, cols4 = cols4, cols5 = cols5)
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 5") {
            opt1 <- c("Missing Completely At Random",
                      "Missing At Random", "Missing Not At Random",
                      "None of the above.")
            
            opt2 <- c("Missing Completely At Random",
                      "Missing At Random", "Missing Not At Random",
                      "None of the above.")
            
            opt3 <- c("Only statement 1 is correct.", "Only statement 2 is correct.",
                      "Only statement 3 is correct.", "Only statements 1 & 2 are correct.",
                      "Only statements 1 & 3 are correct.", "Only statements 2 & 3 are correct.",
                      "All statements are correct.", "None of the statements is correct.")
            
            opt4 <- c("Missing Completely At Random", "Missing At Random",
                      "Missing Not At Random", "Missing Covariate Depended")
            
            opt5 <- c("Missing Completely At Random", "Missing At Random",
                      "Missing Not At Random", "Missing Covariate Depended")
            
            chs1 <- lapply(seq_along(opt1), c); names(chs1) <- opt1
            chs2 <- lapply(seq_along(opt2), c); names(chs2) <- opt2
            chs3 <- lapply(seq_along(opt3), c); names(chs3) <- opt3
            chs4 <- lapply(seq_along(opt4), c); names(chs4) <- opt4
            chs5 <- lapply(seq_along(opt5), c); names(chs5) <- opt5
            
            correct <- c(1, 1, 3, 1, 2)
            
            cols1 <- rep("red", length(opt1)); cols1[correct[1L]] <- "green"
            cols2 <- rep("red", length(opt2)); cols2[correct[2L]] <- "green"
            cols3 <- rep("red", length(opt3)); cols3[correct[3L]] <- "green"
            cols4 <- rep("red", length(opt4)); cols4[correct[4L]] <- "green"
            cols5 <- rep("red", length(opt5)); cols5[correct[5L]] <- "green"
            
            list(chs1 = chs1, chs2 = chs2, chs3 = chs3, chs4 = chs4, chs5 = chs5,
                 cols1 = cols1, cols2 = cols2, cols3 = cols3, cols4 = cols4, cols5 = cols5)
        }
    })
    
    output$sQuiz_ans <- renderUI({
        local_answers <- answers()
        if (input$chapter == "Quizzes" && input$section == "Quiz 1") {
            if (show_Quiz1_Qns$Q1 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz1_q1", label = "", 
                                                choices = local_answers$chs1, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ1", "Check your answer", "success"))
                )
            } else if (show_Quiz1_Qns$Q2 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz1_q2", label = "", 
                                                choices = local_answers$chs2, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ2", "Check your answer", "success"))
                )
            } else if (show_Quiz1_Qns$Q3 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz1_q3", label = "",
                                                choices = local_answers$chs3, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ3", "Check your answer", "success"))
                )
            } else if (show_Quiz1_Qns$Q4 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz1_q4", label = "",
                                                choices = local_answers$chs4, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ4", "Check your answer", "success"))
                )
            } else if (show_Quiz1_Qns$Q5 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz1_q5", label = "",
                                                choices = local_answers$chs5, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ5", "Check your answer", "success"))
                )
            } else if (show_Quiz1_Qns$Q6 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz1_q6", label = "",
                                                choices = local_answers$chs6, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ6", "Check your answer", "success"))
                )
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 2") {
            if (show_Quiz2_Qns$Q1 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz2_q1", label = "", 
                                                choices = local_answers$chs1, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ1", "Check your answer", "success"))
                )
            } else if (show_Quiz2_Qns$Q2 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz2_q2", label = "", 
                                                choices = local_answers$chs2, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ2", "Check your answer", "success"))
                )
            } else if (show_Quiz2_Qns$Q3 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz2_q3", label = "",
                                                choices = local_answers$chs3, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ3", "Check your answer", "success"))
                )
            } else if (show_Quiz2_Qns$Q4 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz2_q4", label = "",
                                                choices = local_answers$chs4, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ4", "Check your answer", "success"))
                )
            } else if (show_Quiz2_Qns$Q5 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz2_q5", label = "",
                                                choices = local_answers$chs5, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ5", "Check your answer", "success"))
                )
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 3") {
            if (show_Quiz3_Qns$Q1 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz3_q1", label = "", 
                                                choices = local_answers$chs1, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ1", "Check your answer", "success"))
                )
            } else if (show_Quiz3_Qns$Q2 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz3_q2", label = "", 
                                                choices = local_answers$chs2, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ2", "Check your answer", "success"))
                )
            } else if (show_Quiz3_Qns$Q3 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz3_q3", label = "",
                                                choices = local_answers$chs3, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ3", "Check your answer", "success"))
                )
            } else if (show_Quiz3_Qns$Q4 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz3_q4", label = "",
                                                choices = local_answers$chs4, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ4", "Check your answer", "success"))
                )
            } else if (show_Quiz3_Qns$Q5 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz3_q5", label = "",
                                                choices = local_answers$chs5, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ5", "Check your answer", "success"))
                )
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 4") {
            if (show_Quiz4_Qns$Q1 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz4_q1", label = "", 
                                                choices = local_answers$chs1, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ1", "Check your answer", "success"))
                )
            } else if (show_Quiz4_Qns$Q2 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz4_q2", label = "", 
                                                choices = local_answers$chs2, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ2", "Check your answer", "success"))
                )
            } else if (show_Quiz4_Qns$Q3 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz4_q3", label = "",
                                                choices = local_answers$chs3, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ3", "Check your answer", "success"))
                )
            } else if (show_Quiz4_Qns$Q4 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz4_q4", label = "",
                                                choices = local_answers$chs4, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ4", "Check your answer", "success"))
                )
            } else if (show_Quiz4_Qns$Q5 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz4_q5", label = "",
                                                choices = local_answers$chs5, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ5", "Check your answer", "success"))
                )
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 5") {
            if (show_Quiz5_Qns$Q1 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz5_q1", label = "", 
                                                choices = local_answers$chs1, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ1", "Check your answer", "success"))
                )
            } else if (show_Quiz5_Qns$Q2 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz5_q2", label = "", 
                                                choices = local_answers$chs2, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ2", "Check your answer", "success"))
                )
            } else if (show_Quiz5_Qns$Q3 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz5_q3", label = "",
                                                choices = local_answers$chs3, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ3", "Check your answer", "success"))
                )
            } else if (show_Quiz5_Qns$Q4 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz5_q4", label = "",
                                                choices = local_answers$chs4, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ4", "Check your answer", "success"))
                )
            } else if (show_Quiz5_Qns$Q5 && !show_Ans$show) {
                fluidRow(column(5, radioButtons("ans_Quiz5_q5", label = "",
                                                choices = local_answers$chs5, 
                                                selected = character(0))),
                         column(1),
                         column(4, actionButton("ansQ5", "Check your answer", "success"))
                )
            }
        }
    })
    
    observeEvent(input$ansQ1, {
        if (any(length(input$ans_Quiz1_q1), length(input$ans_Quiz2_q1), length(input$ans_Quiz3_q1),
                length(input$ans_Quiz4_q1), length(input$ans_Quiz5_q1))) 
            show_Ans$show <- TRUE
    })
    observeEvent(input$ansQ2, {
        if (any(length(input$ans_Quiz1_q2), length(input$ans_Quiz2_q2), length(input$ans_Quiz3_q2),
                length(input$ans_Quiz4_q2), length(input$ans_Quiz5_q2))) 
            show_Ans$show <- TRUE
    })
    observeEvent(input$ansQ3, {
        if (any(length(input$ans_Quiz1_q3), length(input$ans_Quiz2_q3), length(input$ans_Quiz3_q3),
                length(input$ans_Quiz4_q3), length(input$ans_Quiz5_q3)))
            show_Ans$show <- TRUE
    })
    observeEvent(input$ansQ4, {
        if (any(length(input$ans_Quiz1_q4), length(input$ans_Quiz2_q4), length(input$ans_Quiz3_q4),
                length(input$ans_Quiz4_q4), length(input$ans_Quiz5_q4)))
            show_Ans$show <- TRUE
    })
    observeEvent(input$ansQ5, {
        if (any(length(input$ans_Quiz1_q5), length(input$ans_Quiz2_q5), length(input$ans_Quiz3_q5),
                length(input$ans_Quiz4_q5), length(input$ans_Quiz5_q5)))
            show_Ans$show <- TRUE
    })
    observeEvent(input$ansQ6, {
        if (any(length(input$ans_Quiz1_q6), length(input$ans_Quiz2_q6), length(input$ans_Quiz3_q6),
                length(input$ans_Quiz4_q6), length(input$ans_Quiz5_q6)))
            show_Ans$show <- TRUE
    })
    
    output$sQuiz_ans_check <- renderUI({
        local_answers <- answers()
        if (input$chapter == "Quizzes" && input$section == "Quiz 1") {
            if (show_Quiz1_Qns$Q1 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz1_q1.", label = "", 
                                                choices = local_answers$chs1, 
                                                selected = input$ans_Quiz1_q1,
                                                colors = local_answers$cols1)),
                         column(1),
                         column(4, actionButton("prcQ1", "Next Question", "primary"))
                )
            } else if (show_Quiz1_Qns$Q2 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz1_q2.", label = "", 
                                                choices = local_answers$chs2, 
                                                selected = input$ans_Quiz1_q2,
                                                colors = local_answers$cols2)),
                         column(1),
                         column(4, actionButton("prcQ2", "Next Question", "primary"))
                )
            } else if (show_Quiz1_Qns$Q3 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz1_q3.", label = "",
                                                choices = local_answers$chs3, 
                                                selected = input$ans_Quiz1_q3,
                                                colors = local_answers$cols3)),
                         column(1),
                         column(4, actionButton("prcQ3", "Next Question", "primary"))
                )
            } else if (show_Quiz1_Qns$Q4 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz1_q4.", label = "",
                                                  choices = local_answers$chs4, 
                                                  selected = input$ans_Quiz1_q4,
                                                  colors = local_answers$cols4)),
                         column(1),
                         column(4, actionButton("prcQ4", "Next Question", "primary"))
                )
            } else if (show_Quiz1_Qns$Q5 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz1_q5.", label = "",
                                                  choices = local_answers$chs5, 
                                                  selected = input$ans_Quiz1_q5,
                                                  colors = local_answers$cols5)),
                         column(1),
                         column(4, actionButton("prcQ5", "Next Question", "primary"))
                )
            } else if (show_Quiz1_Qns$Q6 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz1_q6.", label = "",
                                                  choices = local_answers$chs6, 
                                                  selected = input$ans_Quiz1_q6,
                                                  colors = local_answers$cols6))
                )
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 2") {
            if (show_Quiz2_Qns$Q1 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz2_q1.", label = "", 
                                                  choices = local_answers$chs1, 
                                                  selected = input$ans_Quiz2_q1,
                                                  colors = local_answers$cols1)),
                         column(1),
                         column(4, actionButton("prcQ1", "Next Question", "primary"))
                )
            } else if (show_Quiz2_Qns$Q2 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz2_q2.", label = "", 
                                                  choices = local_answers$chs2, 
                                                  selected = input$ans_Quiz2_q2,
                                                  colors = local_answers$cols2)),
                         column(1),
                         column(4, actionButton("prcQ2", "Next Question", "primary"))
                )
            } else if (show_Quiz2_Qns$Q3 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz2_q3.", label = "",
                                                  choices = local_answers$chs3, 
                                                  selected = input$ans_Quiz2_q3,
                                                  colors = local_answers$cols3)),
                         column(1),
                         column(4, actionButton("prcQ3", "Next Question", "primary"))
                )
            } else if (show_Quiz2_Qns$Q4 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz2_q4.", label = "",
                                                  choices = local_answers$chs4, 
                                                  selected = input$ans_Quiz2_q4,
                                                  colors = local_answers$cols4)),
                         column(1),
                         column(4, actionButton("prcQ4", "Next Question", "primary"))
                )
            } else if (show_Quiz2_Qns$Q5 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz2_q5.", label = "",
                                                  choices = local_answers$chs5, 
                                                  selected = input$ans_Quiz2_q5,
                                                  colors = local_answers$cols5))
                )
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 3") {
            if (show_Quiz3_Qns$Q1 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz3_q1.", label = "", 
                                                  choices = local_answers$chs1, 
                                                  selected = input$ans_Quiz3_q1,
                                                  colors = local_answers$cols1)),
                         column(1),
                         column(4, actionButton("prcQ1", "Next Question", "primary"))
                )
            } else if (show_Quiz3_Qns$Q2 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz3_q2.", label = "", 
                                                  choices = local_answers$chs2, 
                                                  selected = input$ans_Quiz3_q2,
                                                  colors = local_answers$cols2)),
                         column(1),
                         column(4, actionButton("prcQ2", "Next Question", "primary"))
                )
            } else if (show_Quiz3_Qns$Q3 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz3_q3.", label = "",
                                                  choices = local_answers$chs3, 
                                                  selected = input$ans_Quiz3_q3,
                                                  colors = local_answers$cols3)),
                         column(1),
                         column(4, actionButton("prcQ3", "Next Question", "primary"))
                )
            } else if (show_Quiz3_Qns$Q4 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz3_q4.", label = "",
                                                  choices = local_answers$chs4, 
                                                  selected = input$ans_Quiz3_q4,
                                                  colors = local_answers$cols4)),
                         column(1),
                         column(4, actionButton("prcQ4", "Next Question", "primary"))
                )
            } else if (show_Quiz3_Qns$Q5 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz3_q5.", label = "",
                                                  choices = local_answers$chs5, 
                                                  selected = input$ans_Quiz3_q5,
                                                  colors = local_answers$cols5))
                )
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 4") {
            if (show_Quiz4_Qns$Q1 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz4_q1.", label = "", 
                                                  choices = local_answers$chs1, 
                                                  selected = input$ans_Quiz4_q1,
                                                  colors = local_answers$cols1)),
                         column(1),
                         column(4, actionButton("prcQ1", "Next Question", "primary"))
                )
            } else if (show_Quiz4_Qns$Q2 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz4_q2.", label = "", 
                                                  choices = local_answers$chs2, 
                                                  selected = input$ans_Quiz4_q2,
                                                  colors = local_answers$cols2)),
                         column(1),
                         column(4, actionButton("prcQ2", "Next Question", "primary"))
                )
            } else if (show_Quiz4_Qns$Q3 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz4_q3.", label = "",
                                                  choices = local_answers$chs3, 
                                                  selected = input$ans_Quiz4_q3,
                                                  colors = local_answers$cols3)),
                         column(1),
                         column(4, actionButton("prcQ3", "Next Question", "primary"))
                )
            } else if (show_Quiz4_Qns$Q4 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz4_q4.", label = "",
                                                  choices = local_answers$chs4, 
                                                  selected = input$ans_Quiz4_q4,
                                                  colors = local_answers$cols4)),
                         column(1),
                         column(4, actionButton("prcQ4", "Next Question", "primary"))
                )
            } else if (show_Quiz4_Qns$Q5 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz4_q5.", label = "",
                                                  choices = local_answers$chs5, 
                                                  selected = input$ans_Quiz4_q5,
                                                  colors = local_answers$cols5))
                )
            }
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 5") {
            if (show_Quiz5_Qns$Q1 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz5_q1.", label = "", 
                                                  choices = local_answers$chs1, 
                                                  selected = input$ans_Quiz5_q1,
                                                  colors = local_answers$cols1)),
                         column(1),
                         column(4, actionButton("prcQ1", "Next Question", "primary"))
                )
            } else if (show_Quiz5_Qns$Q2 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz5_q2.", label = "", 
                                                  choices = local_answers$chs2, 
                                                  selected = input$ans_Quiz5_q2,
                                                  colors = local_answers$cols2)),
                         column(1),
                         column(4, actionButton("prcQ2", "Next Question", "primary"))
                )
            } else if (show_Quiz5_Qns$Q3 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz5_q3.", label = "",
                                                  choices = local_answers$chs3, 
                                                  selected = input$ans_Quiz5_q3,
                                                  colors = local_answers$cols3)),
                         column(1),
                         column(4, actionButton("prcQ3", "Next Question", "primary"))
                )
            } else if (show_Quiz5_Qns$Q4 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz5_q4.", label = "",
                                                  choices = local_answers$chs4, 
                                                  selected = input$ans_Quiz5_q4,
                                                  colors = local_answers$cols4)),
                         column(1),
                         column(4, actionButton("prcQ4", "Next Question", "primary"))
                )
            } else if (show_Quiz5_Qns$Q5 && show_Ans$show) {
                fluidRow(column(5, myRadioButtons("ans_Quiz5_q5.", label = "",
                                                  choices = local_answers$chs5, 
                                                  selected = input$ans_Quiz5_q5,
                                                  colors = local_answers$cols5))
                )
            }
        }
    })
    
    observeEvent(input$prcQ1, {
        if (input$chapter == "Quizzes" && input$section == "Quiz 1") {
            show_Quiz1_Qns$Q2 <- TRUE; show_Quiz1_Qns$Q1 <- FALSE; show_Ans$show <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 2") {
            show_Quiz2_Qns$Q2 <- TRUE; show_Quiz2_Qns$Q1 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 3") {
            show_Quiz3_Qns$Q2 <- TRUE; show_Quiz3_Qns$Q1 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 4") {
            show_Quiz4_Qns$Q2 <- TRUE; show_Quiz4_Qns$Q1 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 5") {
            show_Quiz5_Qns$Q2 <- TRUE; show_Quiz5_Qns$Q1 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
        }
    })
    observeEvent(input$prcQ2, {
        if (input$chapter == "Quizzes" && input$section == "Quiz 1") {
            show_Quiz1_Qns$Q3 <- TRUE; show_Quiz1_Qns$Q2 <- FALSE; show_Ans$show <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 2") {
            show_Quiz2_Qns$Q3 <- TRUE; show_Quiz2_Qns$Q2 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 3") {
            show_Quiz3_Qns$Q3 <- TRUE; show_Quiz3_Qns$Q2 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 4") {
            show_Quiz4_Qns$Q3 <- TRUE; show_Quiz4_Qns$Q2 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 5") {
            show_Quiz5_Qns$Q3 <- TRUE; show_Quiz5_Qns$Q2 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
        }
    })
    observeEvent(input$prcQ3, {
        if (input$chapter == "Quizzes" && input$section == "Quiz 1") {
            show_Quiz1_Qns$Q4 <- TRUE; show_Quiz1_Qns$Q3 <- FALSE; show_Ans$show <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 2") {
            show_Quiz2_Qns$Q4 <- TRUE; show_Quiz2_Qns$Q3 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 3") {
            show_Quiz3_Qns$Q4 <- TRUE; show_Quiz3_Qns$Q3 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 4") {
            show_Quiz4_Qns$Q4 <- TRUE; show_Quiz4_Qns$Q3 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 5") {
            show_Quiz5_Qns$Q4 <- TRUE; show_Quiz5_Qns$Q3 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
        }
    })
    observeEvent(input$prcQ4, {
        if (input$chapter == "Quizzes" && input$section == "Quiz 1") {
            show_Quiz1_Qns$Q5 <- TRUE; show_Quiz1_Qns$Q4 <- FALSE; show_Ans$show <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 2") {
            show_Quiz2_Qns$Q5 <- TRUE; show_Quiz2_Qns$Q4 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 3") {
            show_Quiz3_Qns$Q5 <- TRUE; show_Quiz3_Qns$Q4 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 4") {
            show_Quiz4_Qns$Q5 <- TRUE; show_Quiz4_Qns$Q4 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 5") {
            show_Quiz5_Qns$Q5 <- TRUE; show_Quiz5_Qns$Q4 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
        }
    })
    observeEvent(input$prcQ5, {
        if (input$chapter == "Quizzes" && input$section == "Quiz 1") {
            show_Quiz1_Qns$Q6 <- TRUE; show_Quiz1_Qns$Q5 <- FALSE; show_Ans$show <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 2") {
            show_Quiz2_Qns$Q5 <- TRUE; show_Quiz2_Qns$Q4 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 3") {
            show_Quiz3_Qns$Q5 <- TRUE; show_Quiz3_Qns$Q4 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 4") {
            show_Quiz4_Qns$Q5 <- TRUE; show_Quiz4_Qns$Q4 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz5_Qns$Q1 <- TRUE
            show_Quiz5_Qns$Q2 <- show_Quiz5_Qns$Q3 <- show_Quiz5_Qns$Q4 <- show_Quiz5_Qns$Q5 <- FALSE
        } else if (input$chapter == "Quizzes" && input$section == "Quiz 5") {
            show_Quiz5_Qns$Q5 <- TRUE; show_Quiz5_Qns$Q4 <- FALSE; show_Ans$show <- FALSE
            show_Quiz1_Qns$Q1 <- TRUE
            show_Quiz1_Qns$Q2 <- show_Quiz1_Qns$Q3 <- show_Quiz1_Qns$Q4 <- show_Quiz1_Qns$Q5 <- show_Quiz1_Qns$Q6 <- FALSE
            show_Quiz2_Qns$Q1 <- TRUE
            show_Quiz2_Qns$Q2 <- show_Quiz2_Qns$Q3 <- show_Quiz2_Qns$Q4 <- show_Quiz2_Qns$Q5 <- FALSE
            show_Quiz3_Qns$Q1 <- TRUE
            show_Quiz3_Qns$Q2 <- show_Quiz3_Qns$Q3 <- show_Quiz3_Qns$Q4 <- show_Quiz3_Qns$Q5 <- FALSE
            show_Quiz4_Qns$Q1 <- TRUE
            show_Quiz4_Qns$Q2 <- show_Quiz4_Qns$Q3 <- show_Quiz4_Qns$Q4 <- show_Quiz4_Qns$Q5 <- FALSE
        }
    })
    
    
    
    output$sQuizzes <- renderText({
        if (input$chapter == "Quizzes")
            includeHTML("./html/quizzes_tab.Rhtml")
    })
    
    ######################################################################################
    ######################################################################################
    
    ##############
    # Practicals #
    ##############
    
    v <- reactiveValues(Pract1 = NULL, Pract2 = NULL, Pract3 = NULL, Pract4 = NULL)
    
    output$sPract_1 <- renderUI({
        if (input$chapter == "Practicals" && input$section == "Practical 1")
            fluidRow(column(4, actionButton("solutions1", "Reveal answers", "danger")),
                     column(1),
                     column(5, downloadButton('downloadP1', 'Download Report')))
    })
    output$sPract_2 <- renderUI({
        if (input$chapter == "Practicals" && input$section == "Practical 2")
            fluidRow(column(4, actionButton("solutions2", "Reveal answers", "danger")),
                     column(1),
                     column(5, downloadButton('downloadP2', 'Download Report')))
    })
    output$sPract_3 <- renderUI({
        if (input$chapter == "Practicals" && input$section == "Practical 3")
            fluidRow(column(4, actionButton("solutions3", "Reveal answers", "danger")),
                     column(1),
                     column(5, downloadButton('downloadP3', 'Download Report')))
    })
    output$sPract_4 <- renderUI({
        if (input$chapter == "Practicals" && input$section == "Practical 4")
            fluidRow(column(4, actionButton("solutions4", "Reveal answers", "danger")),
                     column(1),
                     column(5, downloadButton('downloadP4', 'Download Report')))
    })
    
    observeEvent(input$solutions1, {
        v$Pract1 <- TRUE
    })
    observeEvent(input$solutions2, {
        v$Pract2 <- TRUE
    })
    observeEvent(input$solutions3, {
        v$Pract3 <- TRUE
    })
    observeEvent(input$solutions4, {
        v$Pract4 <- TRUE
    })
    
    output$sPract_motivate <- renderText({
        if (input$chapter == "Practicals" && input$section == "Practical 1" 
            && is.null(v$Pract1)) {
            v$Pract2 <- v$Pract3 <- v$Pract4 <- NULL
            includeHTML("./html/Practical1_motivate.Rhtml")
        } else if (input$chapter == "Practicals" && input$section == "Practical 2"
                   && is.null(v$Pract2)) {
            v$Pract1 <- v$Pract3 <- v$Pract4 <- NULL
            includeHTML("./html/Practical2_motivate.Rhtml")
        } else if (input$chapter == "Practicals" && input$section == "Practical 3"
                   && is.null(v$Pract3)) {
            v$Pract1 <- v$Pract2 <- v$Pract4 <- NULL
            includeHTML("./html/Practical3_motivate.Rhtml")
        } else if (input$chapter == "Practicals" && input$section == "Practical 4"
                   && is.null(v$Pract4)) {
            v$Pract1 <- v$Pract3 <- v$Pract2 <- NULL
            includeHTML("./html/Practical4_motivate.Rhtml")
        }
    })

    output$sPract_motivate2 <- renderText({
        if (input$chapter == "Practicals" && input$section == "Practical 1" 
            && is.null(v$Pract1)) {
            v$Pract2 <- v$Pract3 <- v$Pract4 <- NULL
            includeHTML("./html/Practical1_motivate.Rhtml")
        } else if (input$chapter == "Practicals" && input$section == "Practical 2"
                   && is.null(v$Pract2)) {
            v$Pract1 <- v$Pract3 <- v$Pract4 <- NULL
            includeHTML("./html/Practical2_motivate.Rhtml")
        } else if (input$chapter == "Practicals" && input$section == "Practical 3"
                   && is.null(v$Pract3)) {
            v$Pract1 <- v$Pract2 <- v$Pract4 <- NULL
            includeHTML("./html/Practical3_motivate.Rhtml")
        } else if (input$chapter == "Practicals" && input$section == "Practical 4"
                   && is.null(v$Pract4)) {
            v$Pract1 <- v$Pract3 <- v$Pract2 <- NULL
            includeHTML("./html/Practical4_motivate.Rhtml")
        }
    })
    
    output$sPract_questions <- renderUI({
        if (input$chapter == "Practicals" && input$section == "Practical 1"
            && !is.null(v$Pract1)) {
            chs <- c(paste("Question", 1:9), "All")
            wellPanel(selectInput("pract_Q", "Select Question:", chs, chs[1]))
        } else if (input$chapter == "Practicals" && input$section == "Practical 2"
                   && !is.null(v$Pract2)) {
            chs <- c(paste("Question", 1:10), "All")
            wellPanel(selectInput("pract_Q", "Select Question:", chs, chs[1]))
        } else if (input$chapter == "Practicals" && input$section == "Practical 3"
                   && !is.null(v$Pract3)) {
            chs <- c(paste("Question", 1:9), "All")
            wellPanel(selectInput("pract_Q", "Select Question:", chs, chs[1]))
        } else if (input$chapter == "Practicals" && input$section == "Practical 4"
                   && !is.null(v$Pract4)) {
            chs <- c(paste("Question", 1:8), "All")
            wellPanel(selectInput("pract_Q", "Select Question:", chs, chs[1]))
        }
    })
    
    output$sPract_code <- renderText({
        if (input$chapter == "Practicals" && input$section == "Practical 1"
            && !is.null(v$Pract1) && naf(input$pract_Q)) {
            switch(input$pract_Q,
                   "Question 1" = includeMarkdown("./md/sPract11_code.Rmd"),
                   "Question 2" = includeMarkdown("./md/sPract12_code.Rmd"),
                   "Question 3" = includeMarkdown("./md/sPract13_code.Rmd"),
                   "Question 4" = includeMarkdown("./md/sPract14_code.Rmd"),
                   "Question 5" = includeMarkdown("./md/sPract15_code.Rmd"),
                   "Question 6" = includeMarkdown("./md/sPract16_code.Rmd"),
                   "Question 7" = includeMarkdown("./md/sPract17_code.Rmd"),
                   "Question 8" = includeMarkdown("./md/sPract18_code.Rmd"),
                   "Question 9" = includeMarkdown("./md/sPract19_code.Rmd"),
                   "All" = includeMarkdown("./md/sPract1_code.Rmd"))
        } else if (input$chapter == "Practicals" && input$section == "Practical 2"
                   && !is.null(v$Pract2) && naf(input$pract_Q)) {
            switch(input$pract_Q,
                   "Question 1" = includeMarkdown("./md/sPract21_code.Rmd"),
                   "Question 2" = includeMarkdown("./md/sPract22_code.Rmd"),
                   "Question 3" = includeMarkdown("./md/sPract23_code.Rmd"),
                   "Question 4" = includeMarkdown("./md/sPract24_code.Rmd"),
                   "Question 5" = includeMarkdown("./md/sPract25_code.Rmd"),
                   "Question 6" = includeMarkdown("./md/sPract26_code.Rmd"),
                   "Question 7" = includeMarkdown("./md/sPract27_code.Rmd"),
                   "Question 8" = includeMarkdown("./md/sPract28_code.Rmd"),
                   "Question 9" = includeMarkdown("./md/sPract29_code.Rmd"),
                   "Question 10" = includeMarkdown("./md/sPract210_code.Rmd"),
                   "All" = includeMarkdown("./md/sPract2_code.Rmd"))
        } else if (input$chapter == "Practicals" && input$section == "Practical 3"
                   && !is.null(v$Pract3) && naf(input$pract_Q)) {
            switch(input$pract_Q,
                   "Question 1" = includeMarkdown("./md/sPract31_code.Rmd"),
                   "Question 2" = includeMarkdown("./md/sPract32_code.Rmd"),
                   "Question 3" = includeMarkdown("./md/sPract33_code.Rmd"),
                   "Question 4" = includeMarkdown("./md/sPract34_code.Rmd"),
                   "Question 5" = includeMarkdown("./md/sPract35_code.Rmd"),
                   "Question 6" = includeMarkdown("./md/sPract36_code.Rmd"),
                   "Question 7" = includeMarkdown("./md/sPract37_code.Rmd"),
                   "Question 8" = includeMarkdown("./md/sPract38_code.Rmd"),
                   "Question 9" = includeMarkdown("./md/sPract39_code.Rmd"),
                   "All" = includeMarkdown("./md/sPract3_code.Rmd"))
        } else if (input$chapter == "Practicals" && input$section == "Practical 4"
                   && !is.null(v$Pract4) && naf(input$pract_Q)) {
            switch(input$pract_Q,
                   "Question 1" = includeMarkdown("./md/sPract41_code.Rmd"),
                   "Question 2" = includeMarkdown("./md/sPract42_code.Rmd"),
                   "Question 3" = includeMarkdown("./md/sPract43_code.Rmd"),
                   "Question 4" = includeMarkdown("./md/sPract44_code.Rmd"),
                   "Question 5" = includeMarkdown("./md/sPract45_code.Rmd"),
                   "Question 6" = includeMarkdown("./md/sPract46_code.Rmd"),
                   "Question 7" = includeMarkdown("./md/sPract47_code.Rmd"),
                   "Question 8" = includeMarkdown("./md/sPract48_code.Rmd"),
                   "All" = includeMarkdown("./md/sPract4_code.Rmd"))
        }
    })
    
    output$downloadP1 <- downloadHandler(
        filename = "Practical_1.html",
        content = function(file) {
            if (input$chapter == "Practicals" && input$section == "Practical 1" 
                && !is.null(v$Pract1)) {
                file.copy("./practicals/sPract1_out.html", file)
            } else {
                file.copy("./html/Practical1_motivate.Rhtml", file)
            }
        }
    )

    output$downloadP2 <- downloadHandler(
        filename = "Practical_2.html",
        content = function(file) {
            if (input$chapter == "Practicals" && input$section == "Practical 2" 
                && !is.null(v$Pract2)) {
                file.copy("./practicals/sPract2_out.html", file)
            } else {
                file.copy("./html/Practical2_motivate.Rhtml", file)
            }
        }
    )

    output$downloadP3 <- downloadHandler(
        filename = "Practical_3.html",
        content = function(file) {
            if (input$chapter == "Practicals" && input$section == "Practical 3" 
                && !is.null(v$Pract3)) {
                file.copy("./practicals/sPract3_out.html", file)
            } else {
                file.copy("./html/Practical3_motivate.Rhtml", file)
            }
        }
    )

    output$downloadP4 <- downloadHandler(
        filename = "Practical_4.html",
        content = function(file) {
            if (input$chapter == "Practicals" && input$section == "Practical 4" 
                && !is.null(v$Pract4)) {
                file.copy("./practicals/sPract4_out.html", file)
            } else {
                file.copy("./html/Practical4_motivate.Rhtml", file)
            }
        }
    )
    
    output$sPract_Routput <- renderPrint({
        if (input$chapter == "Practicals" && input$section == "Practical 1" 
            && !is.null(v$Pract1)) {
            v$Pract2 <- v$Pract3 <- v$Pract4 <- NULL
            includeHTML("./practicals/sPract1_out.html")
        } else if (input$chapter == "Practicals" && input$section == "Practical 2"
                   && !is.null(v$Pract2)) {
            v$Pract1 <- v$Pract3 <- v$Pract4 <- NULL
            includeHTML("./practicals/sPract2_out.html")
        } else if (input$chapter == "Practicals" && input$section == "Practical 3"
                   && !is.null(v$Pract3)) {
            v$Pract2 <- v$Pract1 <- v$Pract4 <- NULL
            includeHTML("./practicals/sPract3_out.html")
        } else if (input$chapter == "Practicals" && input$section == "Practical 4"
                   && !is.null(v$Pract4)) {
            v$Pract2 <- v$Pract3 <- v$Pract1 <- NULL
            includeHTML("./practicals/sPract4_out.html")
        }
    })
    
    ######################################################################################
    ######################################################################################

    ###################
    # Advanced Topics #
    ###################
    
    output$at_check_tab <- renderText({
        if (input$chapter == "Advanced Topics")
            includeHTML("./html/check_tab.Rhtml")
    })
    
    output$at_docs <- renderUI({
        if (input$chapter == "Advanced Topics" && input$section == "Dynamic Predictions") {
            withMathJax(includeHTML("./Advanced_Topics/Dynamic_Predictions.html"))
        } else if (input$chapter == "Advanced Topics" && input$section == "Ordinal Data") {
            withMathJax(includeHTML("./Advanced_Topics/Ordinal_Data.html"))
        } else if (input$chapter == "Advanced Topics" && input$section == "Count Data 1") {
            withMathJax(includeHTML("./Advanced_Topics/Count_Data.html"))
        } else if (input$chapter == "Advanced Topics" && input$section == "Count Data 2") {
            withMathJax(includeHTML("./Advanced_Topics/Count_Data_2.html"))
        }
    })
    
        
    ######################################################################################
    ######################################################################################
    
    ##############
    # Help Files #
    ##############
    
    output$Chapter0_help <- renderText({
        if (input$chapter == "Chapter 0")
            includeHTML("./html/Chapter0_help.Rhtml")
    })

    output$Chapter1_help <- renderText({
        if (input$chapter == "Chapter 1")
            includeHTML("./html/Chapter1_help.Rhtml")
    })

    output$Chapter2_help <- renderText({
        if (input$chapter == "Chapter 2")
            includeHTML("./html/Chapter2_help.Rhtml")
    })
    
    output$Chapter3_help <- renderText({
        if (input$chapter == "Chapter 3")
            includeHTML("./html/Chapter3_help.Rhtml")
    })

    output$Chapter4_help <- renderText({
        if (input$chapter == "Chapter 4")
            includeHTML("./html/Chapter4_help.Rhtml")
    })

    output$Chapter5_help <- renderText({
        if (input$chapter == "Chapter 5")
            includeHTML("./html/Chapter5_help.Rhtml")
    })

    output$Chapter6_help <- renderText({
        if (input$chapter == "Chapter 6")
            includeHTML("./html/Chapter6_help.Rhtml")
    })

    output$Chapter8_help <- renderText({
        if (input$chapter == "Quizzes")
            includeHTML("./html/quizzes_tab.Rhtml")
    })
    
    ######################################################################################
    ######################################################################################
    
    ##########
    # Slides #
    ##########
    
    output$slides <- renderText({
        if (input$chapter != "") {
            nam <- switch(input$chapter,
                          "Chapter 0" = 0,
                          "Chapter 1" = 1, "Chapter 2" = 2, "Chapter 3" = 3,
                          "Chapter 4" = 4, "Chapter 5" = 5, "Chapter 6" = 6,
                          "Practicals" = 7, "Quizzes" = 8, "Advanced Topics" = 9)
            return(paste0('<iframe style="height:600px; width:100%"', 
                          'src = "slides_chapter', nam, '.pdf"></iframe>'))
        } 
    })

    ######################################################################################
    ######################################################################################
    
    #########
    # Plots #
    #########
    
    output$plot <- renderPlot({
        if (input$chapter == "Chapter 1" && input$section == "Section 1.1") {
            if (!is.null(input$data) && input$data == "Glaucoma") {
                index <- c(rep(T, 2), rep(F, 4), rep(T, 4), rep(F, 6), rep(T, 2), rep(F, 8), T,
                           F, T, rep(F, 7), F, T, rep(F, 15), rep(T, 2), rep(F, 6), rep(T, 4),
                           rep(F, 4), rep(T, 3))
                id. <- as.character(input$id)
                eye. <- if (input$eye == "right") 1 else 2
                if (input$s11_loess) {
                    print(xyplot(thres ~ years | pos, 
                                 data = glaucoma[glaucoma$id == id. & glaucoma$eye == eye., ],
                                 panel = function (x, y, ...) {
                                     panel.xyplot(x, y, type = "l", col = 1, ...)
                                     panel.loess(x, y, col = 2, lwd = 2)
                                 }, as.table = TRUE, layout = c(9, 8), skip = index,
                                 strip = FALSE,
                                 xlab = "Time (years)", ylab = "Sensitivity Estimate (dB)"))
                } else {
                    print(xyplot(thres ~ years | pos, 
                                 data = glaucoma[glaucoma$id == id. & glaucoma$eye == eye., ],
                           type = "l", col = 1, as.table = TRUE, layout = c(9, 8), skip = index,
                           strip = FALSE,
                           xlab = "Time (years)", ylab = "Sensitivity Estimate (dB)"))
                }
            }
            
            if (!is.null(input$data) && input$data == "AIDS") {
                if (input$s11_loess && !input$s11_sample) {
                    print(xyplot(sqrt(CD4) ~ obstime | drug, group = patient, 
                                 panel = function (x, y, ...) {
                                     panel.xyplot(x, y, type = "l", col = 1, ...)
                                     panel.loess(x, y, col = 2, lwd = 2)
                                 }, data = aids, xlab = "Time (months)", 
                                 ylab = "square root CD4 cell count"))
                } else if (!input$s11_loess && input$s11_sample) {
                    ids <- c(455, 313, 345, 301, 17, 20, 208, 381, 389, 100, 254, 224, 
                             280, 288, 398, 405)
                    print(xyplot(sqrt(CD4) ~ obstime | patient, data = aids, subset = patient %in% ids, 
                           type = "l", col = 1, layout = c(4, 4), as.table = TRUE,
                           xlab = "Time (months)",  
                           ylab = "square root CD4 cell count"))
                    
                } else if (input$s11_loess && input$s11_sample) {
                    ids <- c(455, 313, 345, 301, 17, 20, 208, 381, 389, 100, 254, 224, 
                             280, 288, 398, 405)
                    print(xyplot(sqrt(CD4) ~ obstime | patient, 
                           panel = function (x, y, ...) {
                               panel.xyplot(x, y, type = "l", col = 1, ...)
                               if (length(unique(x)) > 3)
                                   panel.spline(x, y, col = 2, lwd = 2)
                           }, data = aids, subset = patient %in% ids, layout = c(4, 4), as.table = TRUE,
                           xlab = "Time (months)",  
                           ylab = "square root CD4 cell count"))
                } else {
                    print(xyplot(sqrt(CD4) ~ obstime | drug, group = patient, data = aids,
                                 type = "l", col = 1, xlab = "Time (months)", 
                                 ylab = "square root CD4 cell count"))
                }
            }
            if (!is.null(input$data) && input$data == "PBC") {
                if (input$s11_loess && !input$s11_sample) {
                    print(xyplot(log(serBilir) ~ year | drug, group = id, data = pbc2,
                                 panel = function (x, y, ...) {
                                     panel.xyplot(x, y, type = "l", col = 1, ...)
                                     panel.loess(x, y, col = 2, lwd = 2)
                                 }, xlab = "Time (years)",
                                 ylab = "log serum Bilirubin"))
                } else if (!input$s11_loess && input$s11_sample) {
                    ids <- c(102, 36, 288, 193, 177, 202, 70, 301, 88, 104, 
                             43, 209, 28, 184, 176, 157)
                    print(xyplot(log(serBilir) ~ year | id, data = pbc2, subset = id %in% ids, 
                           type = "l", col = 1, layout = c(4, 4), as.table = TRUE, 
                           xlab = "Time (years)", ylab = "log serum Bilirubin"))
                } else if (input$s11_loess && input$s11_sample) {
                    ids <- c(102, 36, 288, 193, 177, 202, 70, 301, 88, 104, 
                             43, 209, 28, 184, 176, 157)
                    print(xyplot(log(serBilir) ~ year | id,
                           panel = function (x, y, ...) {
                               panel.xyplot(x, y, type = "l", col = 1, ...)
                               if (length(unique(x)) > 5)
                                   panel.loess(x, y, col = 2, lwd = 2)
                           }, data = pbc2, subset = id %in% ids, layout = c(4, 4), as.table = TRUE, 
                           xlab = "Time (years)", ylab = "log serum Bilirubin"))
                } else {
                    print(xyplot(log(serBilir) ~ year | drug, group = id, data = pbc2,
                                 type = "l", col = 1, xlab = "Time (years)",
                                 ylab = "log serum Bilirubin"))
                }
            }
            if (!is.null(input$data) && input$data == "Prothro") {
                if (input$s11_loess && !input$s11_sample) {
                    print(xyplot(pro ~ time | treat, group = id, data = prothro,
                                 panel = function (x, y, ...) {
                                     panel.xyplot(x, y, type = "l", col = 1, ...)
                                     panel.loess(x, y, col = 2, lwd = 2)
                                 }, xlab = "Time (years)",
                                 ylab = "Prothrobin"))
                } else if (!input$s11_loess && input$s11_sample) {
                    ids <- c(171, 176, 406, 158, 133, 118, 461, 343, 207, 81, 
                             556, 250, 421, 535, 206, 262)
                    print(xyplot(pro ~ time | factor(id), data = prothro, subset = id %in% ids, 
                           type = "l", col = 1, layout = c(4, 4), as.table = TRUE, 
                           xlab = "Time (years)", ylab = "Prothrobin"))
                    
                } else if (input$s11_loess && input$s11_sample) {
                    ids <- c(171, 176, 406, 158, 133, 118, 461, 343, 207, 81, 
                             556, 250, 421, 535, 206, 262)
                    print(xyplot(pro ~ time | factor(id),
                           panel = function (x, y, ...) {
                               panel.xyplot(x, y, type = "l", col = 1, ...)
                               if (length(unique(x)) > 5)
                                   panel.loess(x, y, col = 2, lwd = 2)
                           }, data = prothro, subset = id %in% ids, layout = c(4, 4), as.table = TRUE, 
                           xlab = "Time (years)", ylab = "Prothrobin"))
                } else {
                    print(xyplot(pro ~ time | treat, group = id, data = prothro,
                                 type = "l", col = 1, xlab = "Time (years)",
                                 ylab = "Prothrobin"))
                }
            }
        }
        
        if (input$chapter == "Chapter 2" && input$section == "Section 2.4" &&
            input$fit_effPlt == "Effect plot (PBC data)") {
            fm_s24_pbc <- gls(log(serBilir) ~ ns(year, 2) * sex + age + age:sex, data = pbc2,
                              correlation = corCAR1(form = ~ year | id))
            
            # the following function creates the predicted values
            # and the 95% CIs
            effectPlotData <- function (object, newdata, orig_data, ...) {
                if (inherits(object, "MixMod")) {
                    return(GLMMadaptive::effectPlotData(object, newdata, ...))
                }
                library("lattice")
                form <- formula(object)
                respVar <- all.vars(form)[1]
                newdata[[respVar]] <- 0.01 
                betas <- if (inherits(object, "gls")) coef(object) else fixef(object)
                V <- vcov(object)
                mfX <- model.frame(terms(form), data = orig_data)
                X <- model.matrix(attr(mfX, "terms"), newdata)
                pred <- c(X %*% betas)
                ses <- sqrt(diag(X %*% V %*% t(X)))
                newdata$pred <- pred
                newdata$low <- pred - 1.96 * ses
                newdata$upp <- pred + 1.96 * ses
                newdata
            }
            
            newDF <- with(pbc2, expand.grid(year = seq(0, 12, length.out = 25),
                                            sex = levels(sex),
                                            age = input$age_select_pbc))
            
            newDF_low <- with(pbc2, expand.grid(year = seq(0, 12, length.out = 25),
                                                sex = levels(sex),
                                                age = 30))
            
            newDF_high <- with(pbc2, expand.grid(year = seq(0, 12, length.out = 25),
                                                 sex = levels(sex),
                                                 age = 65))
            
            vals <- c(unlist(effectPlotData(fm_s24_pbc, newDF_low, pbc2)[c('low', 'upp')]),
                      unlist(effectPlotData(fm_s24_pbc, newDF_high, pbc2)[c('low', 'upp')]))
            
            print(xyplot(pred + low + upp ~ year | sex, data = effectPlotData(fm_s24_pbc, newDF, pbc2), 
                         lty = c(1, 2, 2), col = c(2, 1, 1), lwd = 2, type = "l",
                         xlab = "Follow-up time (years)",
                         ylab = "log Serum Billirubin", ylim = range(vals)))
        }
        
        if (input$chapter == "Chapter 2" && input$section == "Section 2.7" &&
            input$corr_plot) {
            Fun <- switch(input$corrStr,
                          "Compound Symmetry" = corCompSymm,
                          "AR1" = corAR1, 
                          "continuous AR1" = corCAR1,
                          "exponential" = corExp,
                          "linear" = corLin,
                          "Gaussian" = corGaus)
            col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                                       "cyan", "#007FFF", "blue","#00007F"))
            corrplot.mixed(testCS(Fun, input$corrStr_param), upper.col = rev(col4(200)),
                           lower.col = 1)
        }
        
        if (input$chapter == "Chapter 2" && input$section == "Section 2.12") {
            if (naf(input$s212_typePlot) && input$s212_typePlot == "QQnorm") {
                form <- if (naf(input$s212_type_res) && input$s212_type_res == "Pearson") {
                    '~ resid(., "p")'
                } else {
                    '~ resid(., "n")'
                }
                if (naf(input$s212_sex) && input$s212_sex) {
                    form <- if (naf(input$s212_datachoice) && input$s212_datachoice == "AIDS") {
                        paste(form, "| drug")
                    } else { 
                        paste(form, "| sex")
                    }
                }
                form <- as.formula(form)
                if (naf(input$s212_datachoice) && input$s212_datachoice == "AIDS") {
                    if (!exists("fm_s212_aids")) {
                        withProgress({
                            fm <- gls(CD4 ~ obstime + obstime:drug, data = aids,
                                  correlation = corSymm(form = ~ 1 | patient),
                                  weights = varIdent(form = ~ 1 | obstime))
                        }, message = "Fitting the model...")
                        fm_s29_aids2 <<- fm
                        fm_s212_aids <<- fm
                    }
                    print(qqnorm(fm_s212_aids, form))
                } else {
                    print(qqnorm(gls(log(serBilir) ~ year + year:drug + year*sex + age, 
                                     data = pbc2, correlation = corCAR1(form = ~ year | id)), 
                                 form))
                }
            } else {
                v <- if (naf(input$s212_var_res) && input$s212_var_res == "fitted") "fitted(.)" else input$s212_var_res
                form <- if (naf(input$s212_type_res) && input$s212_type_res == "Pearson") {
                    paste('resid(., "p") ~', v)
                } else {
                    paste('resid(., "n") ~', v)
                }
                if (naf(input$s212_sex) && input$s212_sex) {
                    form <- if (naf(input$s212_datachoice) && input$s212_datachoice == "AIDS") {
                        paste(form, "| drug")
                    } else { 
                        paste(form, "| sex")
                    }
                }
                form <- as.formula(form)
                if (naf(input$s212_datachoice) && input$s212_datachoice == "AIDS") {
                    if (!exists("fm_s212_aids")) {
                        withProgress({
                            fm <- gls(CD4 ~ obstime + obstime:drug, data = aids,
                                  correlation = corSymm(form = ~ 1 | patient),
                                  weights = varIdent(form = ~ 1 | obstime))
                        }, message = 'Fitting the model...')
                        fm_s29_aids2 <<- fm
                        fm_s212_aids <<- fm
                    }
                    print(plot(fm_s212_aids, form, type = c("p", "smooth"), lwd = 3))
                } else {
                    print(plot(gls(log(serBilir) ~ year + year:drug + year*sex + age, 
                                   data = pbc2, correlation = corCAR1(form = ~ year | id)), 
                               form, type = c("p", "smooth"), lwd = 3))
                }
            }
        }
        
        if (input$chapter == "Chapter 3" && input$section == "Section 3.2" &&
            input$fit_effPlt == "Effect plot (PBC data)") {
            pbc2$basePro <- with(pbc2, ave(prothrombin, id, FUN = function (x) x[1]))
            if (!exists("fm_s32_pbc")) {
                withProgress({
                    fm <- lme(log(serBilir) ~ ns(year, 2) * sex + (age + basePro) * sex, 
                                   data = pbc2, random = ~ ns(year, 2) | id)
                    fm_s32_pbc <<- fm
                    fm_s34_pbc <<- fm
                }, message = "Fitting the model...")
            }
            
            # the following function creates the predicted values
            # and the 95% CIs
            effectPlotData <- function (object, newdata, orig_data, ...) {
                if (inherits(object, "MixMod")) {
                    return(GLMMadaptive::effectPlotData(object, newdata, ...))
                }
                form <- formula(object)
                respVar <- all.vars(form)[1]
                newdata[[respVar]] <- 0.01 
                betas <- if (inherits(object, "gls")) coef(object) else fixef(object)
                V <- vcov(object)
                mfX <- model.frame(terms(form), data = orig_data)
                X <- model.matrix(attr(mfX, "terms"), newdata)
                pred <- c(X %*% betas)
                ses <- sqrt(diag(X %*% V %*% t(X)))
                newdata$pred <- pred
                newdata$low <- pred - 1.96 * ses
                newdata$upp <- pred + 1.96 * ses
                newdata
            }
            
            newDF <- with(pbc2, expand.grid(year = seq(0, 12, length.out = 25),
                                            sex = levels(sex), serBilir = 0,
                                            age = input$age_select_pbc,
                                            basePro = input$pro_select_pbc))
            
            newDF <- effectPlotData(fm_s32_pbc, newDF, pbc2)
            if (nrow(newDF)) {
                print(xyplot(pred + low + upp ~ year | sex, data = newDF, 
                             lty = c(1, 2, 2), col = c(2, 1, 1), lwd = 2, type = "l",
                             xlab = "Follow-up time (years)",
                             ylab = "log Serum Billirubin"))
            }
        }
        
        if (input$chapter == "Chapter 3" && input$section == "Section 3.3" &&
            input$corr_plot) {
            params <- if (naf(input$diag_covMat) && !input$diag_covMat) {
                switch(input$reStr,
                       "intercepts" = list(sigma2 = input$sigma2, D = input$sigma2_b0),
                       "intercepts & slopes" = {
                           cov01 <- sqrt(input$sigma2_b0) * sqrt(input$sigma2_b1) * input$rho_b0b1
                           DD <- matrix(c(input$sigma2_b0, cov01, cov01, input$sigma2_b1), 2, 2)
                           list(sigma2 = input$sigma2, D = DD)
                       },
                       "intercepts, slopes & slopes^2" = {
                           cov01 <- sqrt(input$sigma2_b0) * sqrt(input$sigma2_b1) * input$rho_b0b1
                           cov02 <- sqrt(input$sigma2_b0) * sqrt(input$sigma2_b2) * input$rho_b0b2
                           cov12 <- sqrt(input$sigma2_b1) * sqrt(input$sigma2_b2) * input$rho_b1b2
                           DD <- matrix(c(input$sigma2_b0, cov01, cov02, cov01, input$sigma2_b1,
                                          cov12, cov02, cov12, input$sigma2_b2), 3, 3)
                           list(sigma2 = input$sigma2, D = DD)
                       })
            } else { 
                switch(input$reStr,
                       "intercepts" = list(sigma2 = input$sigma2, D = input$sigma2_b0),
                       "intercepts & slopes" = {
                           DD <- matrix(c(input$sigma2_b0, 0, 0, input$sigma2_b1), 2, 2)
                           list(sigma2 = input$sigma2, D = DD)
                       },
                       "intercepts, slopes & slopes^2" = {
                           DD <- matrix(c(input$sigma2_b0, 0, 0, 0, input$sigma2_b1,
                                          0, 0, 0, input$sigma2_b2), 3, 3)
                           list(sigma2 = input$sigma2, D = DD)
                       })
            }
            col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F", 
                                       "cyan", "#007FFF", "blue","#00007F"))
            corrplot.mixed(cov2cor(testRES(input$reStr, params)), upper.col = rev(col4(200)),
                           lower.col = 1)
        }
        
        if (input$chapter == "Chapter 3" && input$section == "Section 3.4") {
            pbc2$basePro <- with(pbc2, ave(prothrombin, id, FUN = function (x) x[1]))
            if (!exists("fm_s34_pbc")) {
                withProgress({
                    fm <- lme(log(serBilir) ~ ns(year, 2) * sex + (age + basePro) * sex, 
                              data = pbc2, random = ~ ns(year, 2) | id)
                    fm_s32_pbc <<- fm
                    fm_s34_pbc <<- fm
                }, message = "Fitting the model...")
            }
            pbc2$fitted_marg <- fitted(fm_s34_pbc, level = 0)
            pbc2$fitted_subj <- fitted(fm_s34_pbc, level = 1)
            ids <- c(38, 39, 51, 68, 70, 82, 90, 93, 134, 148, 
                     173, 200, 216, 242, 269, 290)
            
            if (input$s34_data && !input$s34_marg && !input$s34_subj) {
                print(xyplot(log(serBilir) ~ year | id, data = pbc2, type = "p",
                             subset = id %in% ids, layout = c(4, 4), as.table = TRUE, 
                             xlab = "Time (years)", ylab = "log serum Bilirubin"))
            }
            if (input$s34_data && input$s34_marg && !input$s34_subj) {
                print(xyplot(log(serBilir) + fitted_marg + fitted_subj ~ year | id, data = pbc2,
                             panel = function (x, y, ...) {
                                 x.mat <- matrix(x, ncol = 3)
                                 y.mat <- matrix(y, ncol = 3)
                                 panel.xyplot(x.mat[, 1], y.mat[, 1], type = "p", col = "black")
                                 panel.xyplot(x.mat[, 2], y.mat[, 2], type = "l", lwd = 2, col = "red")
                             }, subset = id %in% ids, layout = c(4, 4), as.table = TRUE, 
                             xlab = "Time (years)", ylab = "log serum Bilirubin"))
            }
            if (input$s34_data && !input$s34_marg && input$s34_subj) {
                print(xyplot(log(serBilir) + fitted_marg + fitted_subj ~ year | id, data = pbc2,
                             panel = function (x, y, ...) {
                                 x.mat <- matrix(x, ncol = 3)
                                 y.mat <- matrix(y, ncol = 3)
                                 panel.xyplot(x.mat[, 1], y.mat[, 1], type = "p", col = "black")
                                 panel.xyplot(x.mat[, 3], y.mat[, 3], type = "l", lwd = 2, col = "blue")
                             }, subset = id %in% ids, layout = c(4, 4), as.table = TRUE, 
                             xlab = "Time (years)", ylab = "log serum Bilirubin"))
            }
            if (input$s34_data && input$s34_marg && input$s34_subj) {
                print(xyplot(log(serBilir) + fitted_marg + fitted_subj ~ year | id, data = pbc2,
                             panel = function (x, y, ...) {
                                 x.mat <- matrix(x, ncol = 3)
                                 y.mat <- matrix(y, ncol = 3)
                                 panel.xyplot(x.mat[, 1], y.mat[, 1], type = "p", col = "black")
                                 panel.xyplot(x.mat[, 2], y.mat[, 2], type = "l", lwd = 2, col = "red")
                                 panel.xyplot(x.mat[, 3], y.mat[, 3], type = "l", lwd = 2, col = "blue")
                             }, subset = id %in% ids, layout = c(4, 4), as.table = TRUE, 
                             xlab = "Time (years)", ylab = "log serum Bilirubin"))
            }
            if (!input$s34_data && input$s34_marg && input$s34_subj) {
                print(xyplot(log(serBilir) + fitted_marg + fitted_subj ~ year | id, data = pbc2,
                             panel = function (x, y, ...) {
                                 x.mat <- matrix(x, ncol = 3)
                                 y.mat <- matrix(y, ncol = 3)
                                 panel.xyplot(x.mat[, 2], y.mat[, 2], type = "l", lwd = 2, col = "red")
                                 panel.xyplot(x.mat[, 3], y.mat[, 3], type = "l", lwd = 2, col = "blue")
                             }, subset = id %in% ids, layout = c(4, 4), as.table = TRUE, 
                             xlab = "Time (years)", ylab = "log serum Bilirubin"))
            }
            if (!input$s34_data && !input$s34_marg && input$s34_subj) {
                print(xyplot(log(serBilir) + fitted_marg + fitted_subj ~ year | id, data = pbc2,
                             panel = function (x, y, ...) {
                                 x.mat <- matrix(x, ncol = 3)
                                 y.mat <- matrix(y, ncol = 3)
                                 panel.xyplot(x.mat[, 3], y.mat[, 3], type = "l", lwd = 2, col = "blue")
                             }, subset = id %in% ids, layout = c(4, 4), as.table = TRUE, 
                             xlab = "Time (years)", ylab = "log serum Bilirubin"))
            }
            if (!input$s34_data && input$s34_marg && !input$s34_subj) {
                print(xyplot(log(serBilir) + fitted_marg + fitted_subj ~ year | id, data = pbc2,
                             panel = function (x, y, ...) {
                                 x.mat <- matrix(x, ncol = 3)
                                 y.mat <- matrix(y, ncol = 3)
                                 panel.xyplot(x.mat[, 2], y.mat[, 2], type = "l", lwd = 2, col = "red")
                             }, subset = id %in% ids, layout = c(4, 4), as.table = TRUE, 
                             xlab = "Time (years)", ylab = "log serum Bilirubin"))
            }
            
        }
        
        if (input$chapter == "Chapter 3" && input$section == "Section 3.11") {
            if (!exists('fm_s311_pro')) {
                withProgress({
                    fm_s311_pro <<- lme(pro ~ ns(time, 3) * treat, data = prothro,
                                   random = list(id = pdDiag(form = ~ ns(time, 3))))
                }, message = "Fitting the model...")
            }
            form <- if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot") {
                if (naf(input$s311_var_res) && input$s311_var_res == "fitted") {
                    if (naf(input$s311_type_res) && input$s311_type_res == "Marginal") {
                        'resid(., type = "p", level = 0) ~ fitted(., level = 0)'
                    } else {
                        'resid(., type = "p") ~ fitted(.)'
                    }
                } else {
                    if (naf(input$s311_type_res) && input$s311_type_res == "Marginal") {
                        'resid(., type = "p", level = 0) ~ time'
                    } else {
                        'resid(., type = "p") ~ time'
                    }
                }
            } else if (naf(input$s311_typePlot) && input$s311_typePlot == "QQnorm") {
                if (naf(input$s311_type_res) && input$s311_type_res == "Marginal") {
                    '~ resid(., type = "p", level = 0)'
                } else {
                    '~ resid(., type = "p")'
                }
            }
            if (naf(input$s311_drug) && input$s311_drug) {
                form <- paste(form, "| treat")
            }
            form <- as.formula(form)
            if (naf(input$s311_typePlot) && input$s311_typePlot == "Scatterplot") {
                print(plot(fm_s311_pro, form, type = c("p", "smooth"), lwd = 3))
            } else {
                print(qqnorm(fm_s311_pro, form))
            }
        }
        
        if (input$chapter == "Chapter 4" && input$section == "Section 4.3" &&
            naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot") {
            fm_s43_splines <- geeglm(serCholD ~ ns(year, 3) + ns(age, 3) + drug + sex, 
                                     family = binomial, data = pbc2, id = id, 
                                     corstr = "exchangeable")
            
            # the following function creates the predicted values
            # and the 95% CIs
            effectPlotData <- function (object, newdata, orig_data, ...) {
                if (inherits(object, "MixMod")) {
                    return(GLMMadaptive::effectPlotData(object, newdata, ...))
                }
                form <- formula(object)
                namesVars <- all.vars(form)
                respVar <- namesVars[1]
                newdata[[respVar]] <- 0.01
                betas <- if (!inherits(object, "lme")) coef(object) else fixef(object)
                V <- if (inherits(object, "geeglm")) object$geese$vbeta else vcov(object)
                orig_data <- orig_data[complete.cases(orig_data[namesVars]), ]
                mfX <- model.frame(terms(form), data = orig_data)
                X <- model.matrix(attr(mfX, "terms"), newdata)
                pred <- c(X %*% betas)
                ses <- sqrt(diag(X %*% V %*% t(X)))
                newdata$pred <- pred
                newdata$low <- pred - 1.96 * ses
                newdata$upp <- pred + 1.96 * ses
                newdata
            }
            
            newDF <- with(pbc2, expand.grid(
                year = seq(0, 12, length.out = 25),
                age = input$age_select_pbc_gee,
                drug = levels(drug),
                sex = levels(sex)
            ))
            
            newDF_low <- with(pbc2, expand.grid(
                year = seq(0, 12, length.out = 25),
                age = 30,
                drug = levels(drug),
                sex = levels(sex)
            ))
            
            newDF_high <- with(pbc2, expand.grid(
                year = seq(0, 12, length.out = 25),
                age = 65,
                drug = levels(drug),
                sex = levels(sex)
            ))
            
            vals <- c(unlist(effectPlotData(fm_s43_splines, newDF_low, pbc2)[c('low', 'upp')]),
                      unlist(effectPlotData(fm_s43_splines, newDF_high, pbc2)[c('low', 'upp')]))
            
            if (input$scale_s43 == 'log Odds') {
                print(xyplot(pred + low + upp ~ year | sex * drug, 
                             data = effectPlotData(fm_s43_splines, newDF, pbc2), 
                             lty = c(1, 2, 2), col = c(2, 1, 1), lwd = 2, type = "l",
                             xlab = "Follow-up time (years)",
                             ylab = "log Odds", ylim = range(vals)))
            } else {
                expit <- function (x) exp(x) / (1 + exp(x))
                print(xyplot(expit(pred) + expit(low) + expit(upp) ~ year | sex * drug, 
                       data = effectPlotData(fm_s43_splines, newDF, pbc2), 
                       lty = c(1, 2, 2), col = c(2, 1, 1), lwd = 2, type = "l",
                       xlab = "Follow-up time (years)",
                       ylab = "Probabilities", ylim = c(0.1, 1)))
            }
        }
        
        if (input$chapter == "Chapter 4" && input$section == "Section 4.5"
            && naf(input$s45_CoefPlot) && input$s45_CoefPlot) {
            aids$lowCD4 <- aids$CD4 < sqrt(150)
            aids$obstimef <- factor(aids$obstime)
            if (!exists('fm_s45_ind')) {
                withProgress({
                    fm_s45_ind <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "independence")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_exc')) {
                withProgress({
                    fm_s45_exc <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "exchangeable")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_ar1')) {
                withProgress({
                    fm_s45_ar1 <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "ar1")
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s45_uns')) {
                withProgress({
                    fm_s45_uns <<- geeglm(lowCD4 ~ obstimef, family = binomial, data = aids, 
                                          id = patient, corstr = "unstructured")
                }, message = "Fitting the model...")
            }
            betas <- round(cbind("independence" = coef(fm_s45_ind), 
                                 "exchangeable" = coef(fm_s45_exc),
                                 "AR1" = coef(fm_s45_ar1),
                                 "unstructured" = coef(fm_s45_uns)), 3)
            # Sandwich standard errors
            extractSEs <- function (model) sqrt(diag(model$geese$vbeta))
            ses_sand <- cbind("independence" = extractSEs(fm_s45_ind), 
                              "exchangeable" = extractSEs(fm_s45_exc),
                              "AR1" = extractSEs(fm_s45_ar1),
                              "unstructured" = extractSEs(fm_s45_uns))
            # Naive standard errors
            extractSEs_naive <- function (model) sqrt(diag(model$geese$vbeta.naiv))
            ses_naiv <- cbind("independence" = extractSEs_naive(fm_s45_ind), 
                              "exchangeable" = extractSEs_naive(fm_s45_exc),
                              "AR1" = extractSEs_naive(fm_s45_ar1),
                              "unstructured" = extractSEs_naive(fm_s45_uns))
            dat <- data.frame(
                est = c(betas), 
                lower_sand = unname(c(betas) - 1.96 * c(ses_sand)),
                lower_naiv = unname(c(betas) - 1.96 * c(ses_naiv)),
                upper_sand = unname(c(betas) + 1.96 * c(ses_sand)),
                upper_naiv = unname(c(betas) + 1.96 * c(ses_naiv)),
                parm = gl(5, 1, 20, labels = rownames(betas)),
                work_corr = gl(4, 5, labels = colnames(betas))
            )
            prepanel.ci2 <- function (x, y, lx, ux, lx2, ux2, subscripts, ...) {
                x <- as.numeric(x)
                lx <- as.numeric(lx[subscripts])
                ux <- as.numeric(ux[subscripts])
                lx2 <- as.numeric(lx2[subscripts])
                ux2 <- as.numeric(ux2[subscripts])
                list(xlim = range(x, ux, lx, ux2, lx2, finite = TRUE))
            }
            panel.ci2 <- function (x, y, lx, ux, lx2, ux2, subscripts, pch = 16, ...) {
                x <- as.numeric(x)
                y <- as.numeric(y)
                lx <- as.numeric(lx[subscripts])
                ux <- as.numeric(ux[subscripts])
                lx2 <- as.numeric(lx2[subscripts])
                ux2 <- as.numeric(ux2[subscripts])
                panel.abline(h = c(unique(y)), 
                             col = "grey", lty = 2, lwd = 1.5)
                panel.arrows(lx, y, ux, y,
                             length = 0.1, unit = "native",
                             angle = 90, code = 3, lwd = 2, col = "blue")
                panel.arrows(lx2, y + 0.06, ux2, y + 0.06,
                             length = 0.1, unit = "native",
                             angle = 90, code = 3, lwd = 2, col = "magenta2")
                panel.xyplot(x, y, pch = pch, col = 2, cex = 1.5, ...)
                panel.xyplot(x, y + 0.06, pch = pch, col = 2, cex = 1.5, ...)
            }
            print(dotplot(work_corr ~  est | parm, lx = dat$lower_sand, lx2 = dat$lower_naiv, 
                    ux = dat$upper_sand, ux2 = dat$upper_naiv,
                    data = dat, xlab = "", prepanel = prepanel.ci2, panel = panel.ci2, 
                    as.table = TRUE, 
                    key = simpleKey(c("Robust Standard Errors", "Naive Standard Errors"), 
                                    points = FALSE, lines = TRUE, col = c("blue", "magenta2")),
                    scales = list(x = list(relation = "free"))))
        }
        
        if (input$chapter == "Chapter 5" && input$section == "Section 5.2" &&
            naf(input$fit_effPlt) && input$fit_effPlt == "Effect plot (PBC data)") {
            if (!exists('fm_s52_pbc')) {
                withProgress({
                    fm_s52_pbc <<- mixed_model(serCholD ~ year * drug + I(age - 50) * sex,
                                               random = ~ 1 | id, 
                                               family = binomial(), data = pbc2, nAGQ = 15)
                }, message = "Fitting the model...")
            }
            newDF <- with(pbc2, expand.grid(
                year = seq(0, 12, length.out = 25),
                age = input$age_select_pbc_glmm,
                drug = levels(drug),
                sex = levels(sex)
            ))
            if (input$scale_s52 == 'log Odds') {
                print(xyplot(pred + low + upp ~ year | sex * drug, 
                             data = GLMMadaptive::effectPlotData(fm_s52_pbc, newDF), 
                             lty = c(1, 2, 2), col = c(2, 1, 1), lwd = 2, type = "l",
                             xlab = "Follow-up time (years)",
                             ylab = "Conditional Log Odds"))
            } else {
                plot_data_marg <- GLMMadaptive::effectPlotData(fm_s52_pbc, newDF, marginal = TRUE, 
                                                 cores = 5)
                plot_data_marg$pred0 <- GLMMadaptive::effectPlotData(fm_s52_pbc, newDF)$pred
                key <- simpleKey(c("marginal probabilities", "probabilities average patient"), 
                                 points = FALSE, lines = TRUE)
                key$lines$col <- c("red", "blue")
                key$lines$lwd <- c(2, 2)
                key$lines$lty <- c(1, 1)
                expit <- function (x) exp(x) / (1 + exp(x))
                print(xyplot(expit(pred) + expit(pred0) + expit(low) + expit(upp) ~ year | sex * drug, 
                             data = plot_data_marg, key = key,
                             type = "l", lty = c(1, 1, 2, 2), lwd = 2, 
                             col = c("red", "blue", "black", "black"), 
                             xlab = "Follow-up time (years)", 
                             ylab = "Marginal Probabilities"))
            }
        }

        if (input$chapter == "Chapter 5" && input$section == "Section 5.3") {
            aids$lowCD4 <- aids$CD4 < sqrt(150)
            aids$obstimef <- factor(aids$obstime)
            if (!exists('fm_s53_PQL')) {
                withProgress({
                    fm_s53_PQL <<- glmmPQL(lowCD4 ~ obstimef, family = binomial, 
                                           data = aids, random = ~ 1 | patient)
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s53_q1')) {
                withProgress({
                    fm_s53_q1 <<- mixed_model(lowCD4 ~ obstimef, random = ~ 1 | patient, 
                                              family = binomial(), data = aids, nAGQ = 1)
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s53_q5')) {
                withProgress({
                    fm_s53_q7 <<- mixed_model(lowCD4 ~ obstimef, random = ~ 1 | patient, 
                                              family = binomial(), data = aids, nAGQ = 7)
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s53_q10')) {
                withProgress({
                    fm_s53_q10 <<- mixed_model(lowCD4 ~ obstimef, random = ~ 1 | patient, 
                                               family = binomial(), data = aids, nAGQ = 10)
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s53_q15')) {
                withProgress({
                    fm_s53_q15 <<- mixed_model(lowCD4 ~ obstimef, random = ~ 1 | patient, 
                                               family = binomial(), data = aids, nAGQ = 15)
                }, message = "Fitting the model...")
            }
            if (!exists('fm_s53_q21')) {
                withProgress({
                    fm_s53_q21 <<- mixed_model(lowCD4 ~ obstimef, random = ~ 1 | patient, 
                                               family = binomial(), data = aids, nAGQ = 21)
                }, message = "Fitting the model...")
            }
            extractCIS <- function (object) {
                if (inherits(object, 'MixMod')) {
                    confint(object)
                } else {
                    intervals(object)[[1]]
                }
            }
            models <- list(fm_s53_PQL, fm_s53_q1, fm_s53_q7, fm_s53_q10, fm_s53_q15, fm_s53_q21)
            mat <- do.call("rbind", lapply(models, extractCIS))
            coef.nam <- rownames(mat)
            rownames(mat) <- NULL
            dat <- as.data.frame(mat)
            dat$coef.nam <- factor(coef.nam, levels = unique(coef.nam))
            dat$model <- gl(6, nrow(mat)/6, labels = c('PQL', 'Laplace', 'AGQ-q7', 'AGQ-q10', 
                                                       'AGQ-q15', 'AGQ-q21'))
            prepanel.ci <- function (x, y, lx, ux, subscripts, ...) {
                x <- as.numeric(x)
                lx <- as.numeric(lx[subscripts])
                ux <- as.numeric(ux[subscripts])
                list(xlim = range(x, ux, lx, finite = TRUE))
            }
            panel.ci <- function (x, y, lx, ux, subscripts, pch = 16, ...) {
                x <- as.numeric(x)
                y <- as.numeric(y)
                lx <- as.numeric(lx[subscripts])
                ux <- as.numeric(ux[subscripts])
                panel.abline(h = c(unique(y)), 
                             col = "grey", lty = 2, lwd = 1.5)
                panel.arrows(lx, y, ux, y,
                             length = 0.1, unit = "native",
                             angle = 90, code = 3, lwd = 3, col = "blue")
                panel.xyplot(x, y, pch = pch, col = 2, cex = 1.5, ...)
            }
            print(dotplot(model ~  est. | coef.nam, lx = dat$lower, ux = dat$upper, 
                          data = dat, xlab = "", prepanel = prepanel.ci, panel = panel.ci, 
                          as.table = TRUE, scales = list(x = list(relation = "free"))))
        }
        
        if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
            && naf(input$data_plot) &&input$data_plot == "Boxplot CD4") {
            ##############
            aids_missings <- aids[c('patient', 'CD4', 'obstime', 'AZT', 'prevOI')]
            planned_visits <- c(0, 2, 6, 12, 18)
            data_patient <- split(aids_missings, aids_missings$patient)
            aids_missings <- do.call(rbind, lapply(data_patient, function (d) {
                out <- d[rep(1, length(planned_visits)), ]
                out$CD4 <- rep(NA, nrow(out))
                out$CD4[match(d$obstime, planned_visits)] <- d$CD4
                out$obstime <- planned_visits
                out
            }))
            row.names(aids_missings) <- seq_len(nrow(aids_missings))
            ##############
            length.noNA <- function (x) sum(!is.na(x))
            index <- with(aids_missings, ave(CD4, patient, FUN = length.noNA))
            # 5 measurements to NA in order to exclude them in the analysis
            aids_missings$CD4cc <- aids_missings$CD4
            aids_missings$CD4cc[index < 5] <- NA
            ##############
            locf <- function (x) {
                na.ind <- is.na(x)
                noNA_x <- x[!na.ind]
                idx <- cumsum(!na.ind)
                noNA_x[idx]
            }
            aids_missings$CD4locf <- with(aids_missings, ave(CD4, patient, FUN = locf))
            ##############
            means <- with(aids_missings, tapply(CD4, obstime, mean, na.rm = TRUE))
            mean_imp <- function (x) {
                na.ind <- is.na(x)
                x[na.ind] <- means[na.ind]
                x
            }
            aids_missings$CD4mean_imp <- with(aids_missings, ave(CD4, patient, FUN = mean_imp))
            ##############
            ll <- with(aids_missings, list(
                "Available Cases" = CD4,
                "Complete Cases" = CD4cc,
                "LOCF" = CD4locf,
                "Mean Imputation" = CD4mean_imp
            ))
            boxplot(ll, varwidth = TRUE, col = "lightgrey", 
                    ylab = "square root CD4 cell count")
        }

        if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
            && naf(input$data_plot) &&input$data_plot == "Dropout Patterns") {
            ##############
            aids_missings <- aids[c('patient', 'CD4', 'obstime', 'AZT', 'prevOI')]
            planned_visits <- c(0, 2, 6, 12, 18)
            data_patient <- split(aids_missings, aids_missings$patient)
            aids_missings <- do.call(rbind, lapply(data_patient, function (d) {
                out <- d[rep(1, length(planned_visits)), ]
                out$CD4 <- rep(NA, nrow(out))
                out$CD4[match(d$obstime, planned_visits)] <- d$CD4
                out$obstime <- planned_visits
                out
            }))
            row.names(aids_missings) <- seq_len(nrow(aids_missings))
            ##############
            length.noNA <- function (x) sum(!is.na(x))
            index <- with(aids_missings, ave(CD4, patient, FUN = length.noNA))
            # 5 measurements to NA in order to exclude them in the analysis
            aids_missings$CD4cc <- aids_missings$CD4
            aids_missings$CD4cc[index < 5] <- NA
            ##############
            locf <- function (x) {
                na.ind <- is.na(x)
                noNA_x <- x[!na.ind]
                idx <- cumsum(!na.ind)
                noNA_x[idx]
            }
            aids_missings$CD4locf <- with(aids_missings, ave(CD4, patient, FUN = locf))
            ##############
            means <- with(aids_missings, tapply(CD4, obstime, mean, na.rm = TRUE))
            mean_imp <- function (x) {
                na.ind <- is.na(x)
                x[na.ind] <- means[na.ind]
                x
            }
            aids_missings$CD4mean_imp <- with(aids_missings, ave(CD4, patient, FUN = mean_imp))
            ##############
            pattern <- function (x) max(which(!is.na(x)))
            
            aids_missings$dropout_pattern <- with(aids_missings, ave(CD4, patient, FUN = pattern))
            aids_missings$dropout_pattern <- factor(aids_missings$dropout_pattern, 
                                                    labels = c(paste("Dropout Time:", c(0, 2, 6, 12)), "Completers"))
            
            print(xyplot(CD4 ~ obstime | dropout_pattern, data = aids_missings, group = patient, 
                         panel = function (x, y, ...) {
                             if (panel.number() == 1) {
                                 panel.bwplot(x, y, horizontal = FALSE, col = "black", box.width = 2,
                                              pch = "|", fill = "grey")
                             } else {
                                 panel.xyplot(x, y, type = "l", col = "lightgrey", ...)
                                 panel.loess(x, y, type = "l", lwd = 2, col = "red", 
                                             span = if (panel.number() == 2) 2 else 2/3)
                             }
                         }, as.table = TRUE, xlab = "Time (months)",  ylab = "square root CD4 cell count"))
        }
        
        if (input$chapter == "Chapter 6" && input$section == "Section 6.3"
            && naf(input$data_plot) &&input$data_plot == "Coefficients' Plot") {
            ##############
            aids_missings <- aids[c('patient', 'CD4', 'obstime', 'AZT', 'prevOI')]
            planned_visits <- c(0, 2, 6, 12, 18)
            data_patient <- split(aids_missings, aids_missings$patient)
            aids_missings <- do.call(rbind, lapply(data_patient, function (d) {
                out <- d[rep(1, length(planned_visits)), ]
                out$CD4 <- rep(NA, nrow(out))
                out$CD4[match(d$obstime, planned_visits)] <- d$CD4
                out$obstime <- planned_visits
                out
            }))
            row.names(aids_missings) <- seq_len(nrow(aids_missings))
            ##############
            length.noNA <- function (x) sum(!is.na(x))
            index <- with(aids_missings, ave(CD4, patient, FUN = length.noNA))
            # 5 measurements to NA in order to exclude them in the analysis
            aids_missings$CD4cc <- aids_missings$CD4
            aids_missings$CD4cc[index < 5] <- NA
            ##############
            locf <- function (x) {
                na.ind <- is.na(x)
                noNA_x <- x[!na.ind]
                idx <- cumsum(!na.ind)
                noNA_x[idx]
            }
            aids_missings$CD4locf <- with(aids_missings, ave(CD4, patient, FUN = locf))
            ##############
            means <- with(aids_missings, tapply(CD4, obstime, mean, na.rm = TRUE))
            mean_imp <- function (x) {
                na.ind <- is.na(x)
                x[na.ind] <- means[na.ind]
                x
            }
            aids_missings$CD4mean_imp <- with(aids_missings, ave(CD4, patient, FUN = mean_imp))
            ##############
            if (!exists("fm_s63_aids1")) {
                withProgress({
                    fm_s63_aids1 <<- lme(CD4 ~ obstime * (AZT + prevOI), data = aids_missings,
                                         random = ~ obstime | patient, na.action = na.exclude)
                }, message = 'Fitting the model...')
            }
            if (!exists("fm_s63_aids2")) {
                withProgress({
                    fm_s63_aids2 <<- lme(CD4cc ~ obstime * (AZT + prevOI), data = aids_missings,
                                         random = ~ obstime | patient, na.action = na.exclude)
                }, message = 'Fitting the model...')
            }
            if (!exists("fm_s63_aids3")) {
                withProgress({
                    fm_s63_aids3 <<- lme(CD4locf ~ obstime * (AZT + prevOI), data = aids_missings,
                                         random = ~ obstime | patient)
                }, message = 'Fitting the model...')
            }
            if (!exists("fm_s63_aids4")) {
                withProgress({
                    fm_s63_aids4 <<- lme(CD4mean_imp ~ obstime * (AZT + prevOI), data = aids_missings,
                                         random = ~ obstime | patient, control = lmeControl(opt = "optim"))
                }, message = 'Fitting the model...')
            }
            ##############
            prepanel.ci <- function (x, y, lx, ux, subscripts, ...) {
                x <- as.numeric(x)
                lx <- as.numeric(lx[subscripts])
                ux <- as.numeric(ux[subscripts])
                list(xlim = range(x, ux, lx, finite = TRUE))
            }
            panel.ci <- function (x, y, lx, ux, subscripts, pch = 16, ...) {
                x <- as.numeric(x)
                y <- as.numeric(y)
                lx <- as.numeric(lx[subscripts])
                ux <- as.numeric(ux[subscripts])
                panel.abline(h = c(unique(y)), 
                             col = "grey", lty = 2, lwd = 1.5)
                panel.arrows(lx, y, ux, y,
                             length = 0.2, unit = "native",
                             angle = 90, code = 3, lwd = 2, col = "blue")
                panel.xyplot(x, y, pch = pch, col = 2, cex = 1.2, ...)
            }
            f <- function (model) {
                ints <- intervals(model)
                list(ints$fixed, ints$reStruct[[1]], rbind("sigma" = ints$sigma))
            }
            mat <- rbind(data.matrix(do.call(rbind, f(fm_s63_aids1))), 
                         data.matrix(do.call(rbind, f(fm_s63_aids2))),
                         data.matrix(do.call(rbind, f(fm_s63_aids3))),
                         data.matrix(do.call(rbind, f(fm_s63_aids4))))
            coef.nam <- rownames(mat)
            coef.nam[coef.nam == 'sd((Intercept))'] <- 'sd(b0)'
            coef.nam[coef.nam == 'sd(obstime)'] <- 'sd(b1)'
            coef.nam[coef.nam == 'cor((Intercept),obstime)'] <- 'cor(b0, b1)'
            rownames(mat) <- NULL
            dat <- as.data.frame(mat)
            dat$coef.nam <- factor(coef.nam, levels = unique(coef.nam))
            dat$model <- gl(4, nrow(mat)/4, labels = c('Available Cases', 'Complete Cases', 
                                                       'LOCF', 'Mean Imputation'))
            
            print(dotplot(model ~  est. | coef.nam, lx = dat$lower, ux = dat$upper, data = dat, xlab = "",
                    prepanel = prepanel.ci, panel = panel.ci, as.table = TRUE,
                    par.settings = list(fontsize = list(text = 14)),
                    scales = list(x = list(relation = "free"))))
        }
    })
})