library(shiny)
library(shinythemes)
library(dplyr)
library(greekLetters)

ui <- fluidPage(
  navbarPage("ChoCalc", theme = shinytheme("lumen"),
             tabPanel("Open data", fluid = FALSE, icon = icon("folder-open"),
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("inputFile", "Choose a CSV or Excel File",
                                    multiple = FALSE,
                                    accept = c(".csv", ".xls", ".xlsx")),
                          checkboxInput("header", "The first row is the header", TRUE),
                          uiOutput("smpsize_ui"),
                        ),
                        mainPanel(
                          tableOutput("data_show")
                        )
                      )
             ),
             tabPanel("Unidimensional reliability", icon = icon("university"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          uiOutput("uni_var_ui"),
                          
                          uiOutput("conord_ui"),
                          
                          checkboxGroupInput("point", "Point estimates to obtain",
                                             choiceNames = c(paste( greeks("alpha"), "(tau-equivalent reliability)"),
                                                             paste("standardized ", greeks("alpha")),
                                                             paste("ten Berge-Zegers", greeks("mu_4")),
                                                             paste("standardized ", greeks("mu_4")),
                                                             "congeneric reliability (CFA)",
                                                             "standardized congeneric reliability",
                                                             "principal component analysis reliability"),
                                             selected =  c("std_mu", "std_cr", "pca"),
                                             choiceValues = c("alpha", "std_alpha", "mu", "std_mu", "cr", "std_cr", "pca"),
                                             width = '100%'),
                          
                          uiOutput("uni_ci_ask"),
                          
                          radioButtons("ifdrop", "What-if-item-dropped analysis",
                                       choices = c("Yes, please"= "yes",
                                                   "No, thanks"= "no"),
                                       selected = "no"),
                          
                          uiOutput("ifdrop_detail_ui"),
                          
                          sliderInput("digit", "Display digits", min = 2, max = 6, value = 3),
                          
                          uiOutput("uni_run_ui") 
                        ),
                        mainPanel(
                          shinycssloaders::withSpinner(tableOutput("uni_table")),
                          uiOutput("unidown1_ui"),
                          textOutput("rec"),
                          tableOutput("drop_out"),
                          textOutput("ref"),
                        )
                      )
             ),
             tabPanel("Multidimensional reliability", icon = icon("medium-m"),
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("untilFile", "The column positions of the last items of all subscales",
                                    multiple = FALSE,
                                    accept = c(".csv", ".txt")), 
                          checkboxGroupInput("multi_choice", "Multidimensional test reliability, subtest reliability and omega hierarchical",
                                             choiceNames = c("Stratified alpha (multidimensional tau-equivalent reliability)",
                                                             "Multidimensional parallel reliability",
                                                             paste0("Bottom-up approach using ", greeks("mu_4")),
                                                             "Bottom-up approach using congeneric reliability",
                                                             "Bottom-up approach using PCA reliability",
                                                             "Bifactor reliability",
                                                             "Second-order factors reliability",
                                                             "Correlated factors reliability"),
                                             selected =  c("rmt", "rmp", "rbmu", "rbc",  "rbpca", "rbf", "rsof", "rcf"),
                                             choiceValues = c("rmt", "rmp", "rbmu", "rbc",  "rbpca", "rbf", "rsof", "rcf"),
                                             width = '100%'),
                          radioButtons("ifcfa", "CFA model details (model fit and parameter estimates)",
                                       choices = c("Yes, please"= "yes",
                                                   "No, thanks"= "no"),
                                       selected = "no"),
                          
                          uiOutput("cfa_choose_ui"),
                          uiOutput("multi_run_ui") 
                        ),
                        mainPanel(
                          textOutput("notice_q"),
                          tags$head(tags$style("#notice_q{font-weight: bold}")),
                          textOutput("notice_1"),
                          textOutput("notice_2"),
                          textOutput("notice_3"),
                          tags$head(tags$style("#notice_3{font-family: courier}")),
                          textOutput("notice_4"),
                          tableOutput("show_subtest"),
                          shinycssloaders::withSpinner(tableOutput("multi_table")),
                          uiOutput("multidown1_ui"),
                          tableOutput("cfa_fit"),
                          tableOutput("cfa_lambda"),
                          tableOutput("cfa_theta"),#qq
                          tableOutput("cfa_psi"), 
                          tableOutput("cfa_beta")
                        )  
                      )
             ),
             
             tabPanel("Frequently asked questions", icon = icon("question-circle"),
                      fluidRow(
                        column(6,
                               h5(strong("Can ChoCalc take covariance data as input?"),
                                  p("Currently, only unidimensional reliability is possible.. ChoCalc automatically recognizes symmetrical input data as covariances. If the first row of the data has the names of items (i.e., the header), then the first column of the data must also have the same name.")
                               ),
                               h5(strong("How does ChoCalc obtain confidence intervals for reliability?"),
                                  p("It obtains the confidence interval for categorical omega using the def option of the package misty to save time. It directly obtains confidence intervals for other reliability estimators using the package boot. Specifically, it uses the bias corrected and accelerated bootstrap interval and performs 1,000 bootstrap replications.")
                               ),
                               h5(strong("I'm not familiar with the names of reliability estimators. Why doesn't ChoCalc use more common names such as Cronbach's alpha or composite reliability?"),
                                  p("In ChoCalc, Cronbach's alpha is alpha (tau-equivalent reliability), and composite reliability is congeneric reliability. The reason ChoCalc does not use the conventional names is that they give inaccurate information. For example, Cronbach did not develop alpha. See Cho (2022) for a detailed description of the reliability estimators used in ChoCalc.")
                               ),
                               h5(strong("How can I contact the author?"),
                                  p("My name is Eunseong Cho from Kwangwoon University in the Republic of Korea. My email is bene[at]kw.ac.kr. Feel free to send bug reports, questions, suggestions, and more.")
                               ),
                        )
                      )
             )
  )
)

server <- function(input, output, session) {
  ##########################################################################
  # Functions
  ##########################################################################
  calc_drop <- function(dat, estimator_name, vars) {
    out <- vector("double", 0)
    if(isCov(dat)) {
      for (i in 1:length(vars)) {
        out <- c(out, uni_ci(ord_shorts[which(ord_names == estimator_name)], dat[-i, -i])$pe)
      }
    } else {
      for (i in 1:length(vars)) {
        out <- c(out, uni_ci(ord_shorts[which(ord_names == estimator_name)], dat[, -i])$pe)
      }
    }
    out
  }
  find_name <- function(choices, values, names) {
    out <- vector("character", 0)
    for (i in 1:length(choices)) {
      out <- c(out, names[which(values == choices[i])])
    }
    return(out)
  }
  isCov <- function(df) {
    rownames(df) <- colnames(df) <- NULL
    isSymmetric(as.matrix(df))
  }
  loc <- function(full, sub) {
    sub <- as.character(sub)
    out <- vector("integer", 0)
    for (i in 1: length(sub)) {
      out <- c(out, which(full == sub[i]))
    }
    out
  }
  myif <- function(var, value) {
    if (is.null(var)) {
      out <- TRUE
    } else {
      if (var == value) {
        out <- TRUE
      } else {
        out <- FALSE
      }
    }
    out
  }
  alpha <- function(x) {
    m <- get_cov(x)
    n <- nrow(m)/(nrow(m) - 1)
    off <- m
    diag(off) <- 0
    out <- n * sum(off)/sum(m)
    return(out)
  }
  get_cov <- function(x, cor = FALSE) {
    if (nrow(x) == ncol(x)) {
      if (cor) {
        out <- stats::cov2cor(as.matrix(x))
      } else {
        out <- as.matrix(x)
      }
    } else {
      if (cor) {
        out <- stats::cor(x, use = "pairwise.complete.obs")
      } else {
        out <- stats::cov(x, use = "pairwise.complete.obs")
      }
    }
    return(out)
  }
  mu4 <- function(x) {
    m <- get_cov(x)
    n <- nrow(m)/(nrow(m) - 1)
    off <- m
    diag(off) <- 0
    numerator <- sum(off) + sqrt(sum(off^2) +
                                   sqrt(sum(off^4) +
                                          sqrt(sum(off^8) +
                                                 sqrt(n * sum(off^16)))))
    out <- numerator/sum(m)
    return(out)
  }
  joreskog <- function(x, nonneg_loading = FALSE) {
    stopifnot(requireNamespace("matrixcalc"))
    cov <- get_cov(x)
    if (!matrixcalc::is.positive.definite(cov)) {
      out <- NA
    } else {
      est <- uni_cfa(cov, nonneg_loading = nonneg_loading)
      if (any(is.na(est))) {
        out <- NA
      } else {
        sum_lambda <- sum(est$lambda)
        sum_theta <- sum(est$theta)
        out <- sum_lambda^2/(sum_lambda^2 + sum_theta)
      }
    }
    return(out)
  }
  uni_cfa <- function(cov, what = "est", sample_size = 500, nonneg_loading = FALSE,
                      nonneg_error = TRUE, taueq = FALSE, parallel = FALSE) {
    stopifnot(requireNamespace("lavaan"))
    k <- nrow(cov)
    rownames(cov) <- character(length = k)
    for (i in 1:k) {
      rownames(cov)[i] <- paste0("V", i)
      if (i == 1) {
        model_str <- paste("F =~ NA*V1")
      } else if (taueq | parallel) { # tau-equivalent or parallel
        model_str <- paste0(model_str, " + equal('F=~V1')*V", i)
      } else {# congeneric
        model_str <- paste0(model_str, " + l", i, "*V", i)
      }
    }
    colnames(cov) <- rownames(cov)
    model_str <- paste0(model_str, " \n F ~~ 1*F", collapse = "\n")
    if (parallel) {
      for (i in 1:k) { # all errors are constained to be equal
        model_str <- paste0(model_str, "\n V", i, " ~~ e*V", i)
      }
    } else if (!taueq) { # congeneric
      for (i in 1:k) { # to prevent negative errors
        if (nonneg_error) {
          model_str <- paste0(model_str, "\n V", i, " ~~ e", i, "*V", i, "\n e", i,
                              "> 0")
        }
        if (i > 1 & nonneg_loading) { # prevent negative loadings
          model_str <- paste0(model_str, "\n l", i, "> .0")
        }
      }
    }
    fit <- lavaan::cfa(model_str, sample.cov = cov, sample.nobs = sample_size)
    if (lavaan::inspect(fit, what = "converged")) {
      out <- lavaan::inspect(fit, what = what)
    } else {
      out <- NA
    }
    return(out)
  }
  kaisercaffrey <- function(x) {
    matrix <- get_cov(x)
    k <- nrow(matrix)
    first_eigen <- eigen(stats::cov2cor(matrix))$values[1]
    out <- k / (k - 1) * (1 - 1 / first_eigen)
    return(out)
  }
  uni_ci <- function(choices, dat, ci = FALSE) {
    org <- dat
    dat <- as.matrix(dat)
    rownames(dat) <- colnames(dat) <- NULL
    if (isCov(dat)) {
      r <- cov2cor(dat)
    } else {
      r <- cor(dat)
    }
    lower <- upper <- pe <- vector("double", 0)
    if ("alpha" %in% choices) { 
      pe <- c(pe, alpha(dat))
      if (ci) {
        temp <- ci(dat, "alpha")
        lower <- c(lower, temp$lower)
        upper <- c(upper, temp$upper)
      }
    } 
    if ("std_alpha" %in% choices) {
      pe <- c(pe, alpha(r))
      if (ci) {
        temp <- ci(dat, "std_alpha")
        lower <- c(lower, temp$lower)
        upper <- c(upper, temp$upper)
      }
    }
    if ("mu" %in% choices) {
      pe <- c(pe, mu4(dat))
      if (ci) {
        temp <- ci(dat, "mu")
        lower <- c(lower, temp$lower)
        upper <- c(upper, temp$upper)
      }
    }
    if ("std_mu" %in% choices) {
      pe <- c(pe, mu4(r))
      if (ci) {
        temp <- ci(dat, "std_mu")
        lower <- c(lower, temp$lower)
        upper <- c(upper, temp$upper)
      }
    }
    if ("cr" %in% choices) {
      pe <- c(pe, joreskog(dat))
      if (ci) {
        temp <- ci(dat, "cr")
        lower <- c(lower, temp$lower)
        upper <- c(upper, temp$upper)
      }
    } 
    if ("std_cr" %in% choices) {
      est <- uni_cfa(r)
      sum_lambda <- sum(est$lambda)
      sum_theta <- sum(est$theta)
      pe <- c(pe, sum_lambda^2/(sum_lambda^2 + sum_theta))
      if (ci) {
        temp <- ci(dat, "std_cr")
        lower <- c(lower, temp$lower)
        upper <- c(upper, temp$upper)
      }
    } 
    if ("pca" %in% choices) {
      pe <- c(pe, kaisercaffrey(dat))
      if (ci) {
        temp <- ci(dat, "pca")
        lower <- c(lower, temp$lower)
        upper <- c(upper, temp$upper)
      }
    }
    if ("cat" %in% choices) {
      misty_out <- misty::item.omega(org, type = "categ")$result$omega
      pe <- c(pe, misty_out$omega)
      if (ci) {
        lower <- c(lower, misty_out$low)
        upper <- c(upper, misty_out$upp)
      }
    }
    out <- list(pe = pe, lower = lower, upper = upper)
    return(out)
  }
  ci <- function(x, what = "alpha", bca = TRUE) {
    library(boot)
    ls <- function(x, indices) {
      d <- x[indices,]
      #print(ncol(d))
      if (what == "alpha") {
        alpha(d)
      } else if (what == "std_alpha") {
        alpha(cor(d))
      } else if (what == "mu") {
        mu4(d)
      } else if (what == "std_mu") {
        mu4(cor(d))
      } else if (what == "cr") {
        joreskog(d)
      } else if (what == "std_cr") {
        joreskog(cor(d))
      } else if (what  == "pca") {
        kaisercaffrey(d)
      }
    }
    if (bca) {
      ci <- boot.ci(boot(x, ls, 1000))$bca
    } else {
      ci <- boot.ci(boot(x, ls, 1000))$basic
    }
    lower <- ci[4]
    upper <- ci[5]
    out <- list(lower = lower, upper = upper)
    out
  }
  get_multi <- function(choices, dat, until, ci = FALSE) {
    dat <- as.matrix(dat)
    dim <- length(until) + 1
    rel <- omegah <-  vector("double", length(choices))
    sub_rel <- matrix(0, nrow = length(choices), ncol = dim)
    count <- 1
    if ("rmt" %in% choices) { 
      temp <- stratified_alpha(dat, until = until, print = F)
      rel[count] <- temp$rel
      sub_rel[count, ] <- temp$sub_rel
      omegah[count] <- temp$omegah
      count <- count + 1
    } 
    if ("rmp" %in% choices) {
      temp <- stratified_alpha(cor(dat), until = until, print = F)
      rel[count] <- temp$rel
      sub_rel[count, ] <- temp$sub_rel
      omegah[count] <- temp$omegah
      count <- count + 1
    }
    if ("rbmu" %in% choices) {
      temp <- nunnally(dat, until = until, method = "mu", print = F)
      rel[count] <- temp$rel
      sub_rel[count, ] <- temp$sub_rel
      omegah[count] <- NA
      count <- count + 1
    }
    if ("rbc" %in% choices) {
      temp <- nunnally(dat, until = until, method = "joreskog", print = F)
      rel[count] <- temp$rel
      sub_rel[count, ] <- temp$sub_rel
      omegah[count] <- NA
      count <- count + 1
    }
    if ("rbpca" %in% choices) {
      temp <- nunnally(dat, until = until, method = "kaisercaffrey", print = F)
      rel[count] <- temp$rel
      sub_rel[count, ] <- temp$sub_rel
      omegah[count] <- NA
      count <- count + 1
    }
    if ("rbf" %in% choices) {
      temp <- bifactor(dat, until = until, print = F)
      rel[count] <- temp$rel
      sub_rel[count, ] <- temp$sub_rel
      omegah[count] <- temp$omegah
      count <- count + 1
    }
    if ("rsof" %in% choices) {
      temp <- second_order(dat, until = until, print = F)
      rel[count] <- temp$rel
      sub_rel[count, ] <- temp$sub_rel
      omegah[count] <- temp$omegah
      count <- count + 1
    }
    if ("rcf" %in% choices) {
      temp <- correlated_factors(dat, until = until, print = F)
      rel[count] <- temp$rel
      sub_rel[count, ] <- temp$sub_rel
      omegah[count] <- NA
      count <- count + 1
    }
    out <- list(rel = rel, sub_rel = sub_rel, omegah = omegah)
    return(out)
  }
  
  ##########################################################################
  # stratified alpha
  ##########################################################################
  stratified_alpha <- function(x, until, mp = FALSE, print = TRUE) {
    m <- get_cov(x)
    grp_start <- c(1, until + 1)
    grp_end <- c(until, nrow(m))
    item <- round(nrow(m)/length(grp_start))
    # to obtain hierarchical omega
    btw_cov <- m
    for (i in seq_along(grp_start)) { # to conveniently compute the average correlation using na.rm
      btw_cov[grp_start[i]:grp_end[i], grp_start[i]:grp_end[i]] <- NA
    }
    avg_btw_cov <- mean(btw_cov, na.rm = TRUE)
    omegah <- nrow(m)^2 * avg_btw_cov / sum(m)
    
    sub_rel <- sub_omegah <- sub_var <- vector("double", length(grp_start))
    for (i in seq_along(grp_start)) {
      sub_mat <- m[grp_start[i]:grp_end[i], grp_start[i]:grp_end[i]]
      sub_rel[i] <- reliacoef::alpha(sub_mat, print = FALSE)
      sub_var[i] <- sum(sub_mat)
      sub_omegah[i] <- item^2 * avg_btw_cov / sub_var[i]
    }
    sum <- vector("double", length = 1)
    for (i in seq_along(grp_start)) {
      sum <- sum + sub_var[i] * (1 - sub_rel[i])
    }
    rel <- 1 - sum / sum(m)
    # output
    out <- list(rel = rel,
                omegah = omegah,
                sub_rel = sub_rel,
                sub_omegah = sub_omegah)
    invisible(out)
  }
  ##########################################################################
  # bottom-up reliability
  ##########################################################################
  nunnally <- function(x, until, method = "joreskog", print = TRUE) {
    m <- get_cov(x)
    n <- ncol(x)
    dim <- length(until) + 1
    grp_start <- c(1, until + 1)
    grp_end <- c(until, n)
    sub_rel <- subprod <- vector("double", dim)
    for (i in 1:dim) {
      subvar <- m[grp_start[i]:grp_end[i], grp_start[i]:grp_end[i]]
      if(method == "joreskog") {
        sub_rel[i] <- joreskog(subvar)
      } else if (method == "mu") {
        sub_rel[i] <- mu4(subvar)
      } else if (method == "kaisercaffrey") {
        sub_rel[i] <- kaisercaffrey(subvar)
      }
      subprod[i] <- sum(subvar) * (1 - sub_rel[i])
    }
    rel <- 1 - sum(subprod) / sum(m)
    out <- list(rel = rel, sub_rel = sub_rel)
    invisible(out)
  }
  ##########################################################################
  # bifactor reliability
  ##########################################################################
  bifactor <- function(x, nobs = NULL, until, print = TRUE) {
    stopifnot(requireNamespace("lavaan"))
    if(nrow(x) == ncol(x)) {
      m <- x
      nobs <- ifelse(is.null(nobs), 200, nobs)
    } else {
      m <- cov(x)
      nobs <- nrow(x)
    }
    grp_start <- c(1, until + 1)
    grp_end <- c(until, nrow(m))
    colnames(m) <- rownames(m) <- character(length = nrow(m))
    for (i in 1:nrow(m)) {
      rownames(m)[i] <- paste0("V", i)
      # general factor
      if (i == 1) {
        model_str <- paste("F =~ NA*V1")
      } else {
        model_str <- paste0(model_str, "+ a", i, "*V", i)
      }
    }
    for (i in 1:nrow(m)) {
      if (any(i == grp_start)) {
        which <- which(i == grp_start)
        for (j in i:grp_end[which]) {
          if (j == i) {
            model_str <- paste0(model_str, "\n G", which, "=~ NA*V", j)
          } else {
            model_str <- paste0(model_str, " + b", j, "*V", j)
          }
        } # end of for (j in i:grp_end[which])
      } # end of if(any(i == grp_start))
    } # end of for (i in 1:n)
    model_str <- paste0(model_str, "\n F ~~ 1*F")
    for (i in seq_along(grp_start)) {
      model_str <- paste0(model_str, "\n G", i, " ~~ 1*G", i)
      model_str <- paste0(model_str, "\n F ~~ 0 * G", i)
      if (i < length(grp_start)) {
        for (j in (i + 1):length(grp_start)) {
          model_str <- paste0(model_str, "\n G", i, "~~ 0 * G", j)
        } # end of for (j in (i + 1):length(grp_start))
      } # end of if (i < length(grp_start))
    } # end of for (i in seq_along(grp_start))
    for (i in 1:nrow(m)) { # to prevent negative errors
      model_str <- paste0(model_str, "\n V", i, " ~~ e", i, "*V", i, "\n e", i, "> 0")
    }
    lav_out <- lavaan::cfa(model_str, sample.cov = m, sample.nobs = nobs)
    if (lavaan::inspect(lav_out, what  = "converged")) {
      fit <- lavaan::inspect(lav_out, what = "fit")
      est <- lavaan::inspect(lav_out, what = "est")
      implied <- lavaan::inspect(lav_out, what = "implied")[[1]]
      rel <- 1 - sum(est$theta)/sum(implied)
      omegah <- sum(est$lambda[, 1])^2 / sum(implied)
      # to obtain subdimensional reliability
      sub_rel <- sub_omegah <- vector("double", length = length(grp_start))
      for (i in seq_along(grp_start)) { # to conveniently compute the average correlation using na.rm
        sub_uniq <- sum(est$theta[grp_start[i]:grp_end[i], grp_start[i]:grp_end[i]])
        sub_implied <- sum(implied[grp_start[i]:grp_end[i], grp_start[i]:grp_end[i]])
        subgen <- sum(est$lambda[grp_start[i]:grp_end[i], 1])
        sub_rel[i] <- 1 - sub_uniq /sub_implied
        sub_omegah[i] <- subgen^2 /sub_implied
      }
      # output
      out <- list(rel = rel,
                  omegah = omegah,
                  sub_rel = sub_rel,
                  sub_omegah = sub_omegah,
                  fit = fit,
                  est = est)
    } else {# NA for non-convergent solutions
      out <- NA
    }
    
    invisible(out)
  }
  ##########################################################################
  # second order factor reliability
  ##########################################################################
  second_order <- function(x, nobs = NULL, until, print = TRUE) {
    stopifnot(requireNamespace("lavaan"))
    if(nrow(x) == ncol(x)) {
      m <- x
      nobs <- ifelse(is.null(nobs), 200, nobs)
    } else {
      m <- cov(x)
      nobs <- nrow(x)
    }
    grp_start <- c(1, until + 1)
    grp_end <- c(until, nrow(m))
    grp_n <- length(until) + 1
    colnames(m) <- rownames(m) <- character(length = nrow(m))
    model_str <- vector("character")
    
    for (i in 1:nrow(m)) {
      rownames(m)[i] <- paste0("V", i)
      if (any(i == grp_start)) {
        which <- which(i == grp_start)
        for (j in i:grp_end[which]) {
          if (j == i) {
            model_str <- paste0(model_str, "\n FoF", which, "=~ NA*V", j)
          } else {
            model_str <- paste0(model_str, " + b", j, "*V", j)
          }
        } # end of for (j in i:grp_end[which])
      } # end of if(any(i == grp_start))
    } #end of for (i in 1:nrow(m)row(m))
    colnames(m) <- rownames(m)
    
    for (i in seq_along(grp_start)) {
      if (i == 1) {
        model_str <- paste0(model_str, "\n SoF =~ NA * FoF1")
      } else {
        model_str <- paste0(model_str, " + a", i, " * FoF", i)
      }
    }
    
    for (i in 1:length(grp_start)) {
      model_str <- paste0(model_str, "\n FoF", i, " ~~ 1 * FoF", i)
    }
    
    model_str <- paste0(model_str, "\n SoF ~~ 1 * SoF")
    
    for (i in 1:nrow(m)) { # to prevent negative errors
      model_str <- paste0(model_str, "\n V", i, " ~~ e", i, "*V", i, "\n e", i, "> 0")
    }
    
    lav_out <- lavaan::cfa(model_str, sample.cov = m, sample.nobs = nobs)
    if (lavaan::inspect(lav_out, what = "converged")) {
      fit <- lavaan::inspect(lav_out, what = "fit")
      est <- lavaan::inspect(lav_out, what = "est")
      implied <- lavaan::inspect(lav_out, what = "implied")[[1]]
      rel <- 1 - sum(est$theta)/sum(implied)
      lambda <- est$lambda[, 1:grp_n]
      beta <- est$beta[1:grp_n, (grp_n + 1)]
      gen_loading <- lambda %*% beta
      omegah <- sum(gen_loading) ^ 2 / sum(implied)
      
      # to obtain subdimensional reliability
      sub_rel <- sub_omegah <- vector("double", length = length(grp_start))
      for (i in seq_along(grp_start)) { # to conveniently compute the average correlation using na.rm
        sub_uniq <- sum(est$theta[grp_start[i]:grp_end[i], grp_start[i]:grp_end[i]])
        sub_implied <- sum(implied[grp_start[i]:grp_end[i], grp_start[i]:grp_end[i]])
        sub_rel[i] <- 1 - sub_uniq / sub_implied
        sub_omegah[i] <- sum(gen_loading[grp_start[i]:grp_end[i]])^2 / sub_implied
      }
      
      # output
      out <- list(rel = rel,
                  omegah = omegah,
                  sub_rel = sub_rel,
                  sub_omegah = sub_omegah,
                  fit = fit,
                  est = est)
      
    } else { # NA for non-convergent solutions
      out <- NA
    }
    invisible(out)
  }
  ##########################################################################
  # correlated factors reliability
  ##########################################################################
  correlated_factors <- function(x, nobs = NULL, until, print = TRUE) {
    stopifnot(requireNamespace("lavaan"))
    if(nrow(x) == ncol(x)) {
      m <- x
      nobs <- ifelse(is.null(nobs), 200, nobs)
    } else {
      m <- cov(x)
      nobs <- nrow(x)
    }
    colnames(m) <- rownames(m) <- character(length = nrow(m))
    grp_start <- c(1, until + 1)
    grp_end <- c(until, nrow(m))
    model_str <- vector("character")
    
    for (i in 1:nrow(m)) {
      rownames(m)[i] <- paste0("V", i)
    }
    
    for (i in 1:nrow(m)) {
      if (any(i == grp_start)) {
        which <- which(i == grp_start)
        for (j in i:grp_end[which]) {
          if (j == i) {
            model_str <- paste0(model_str, "\n G", which, "=~ NA*V", j)
          } else {
            model_str <- paste0(model_str, " + b", j, "*V", j)
          }
        } # end of for (j in i:grp_end[which])
      } # end of if(any(i == grp_start))
    } # end of for (i in 1:nrow(m))
    
    for (i in seq_along(grp_start)) {
      model_str <- paste0(model_str, "\n G", i, " ~~ 1 * G", i)
      if (i > 1) { # to prevent correlation less than -1 or greater than 1
        for (j in 1:(i - 1)) {
          model_str <- paste0(model_str, "\n G", i, " ~~ a", i, j, " * G", j)
          model_str <- paste0(model_str, "\n a", i, j, " < 1")
          model_str <- paste0(model_str, "\n a", i, j, " > -1")
        }  
      }
    }
    
    for (i in 1:nrow(m)) { # to prevent negative errors
      model_str <- paste0(model_str, "\n V", i, " ~~ e", i, "*V", i, "\n e", i, "> 0")
    }
    
    lav_out <- lavaan::cfa(model_str, sample.cov = m, sample.nobs = 500)
    if (lavaan::inspect(lav_out, what = "converged")) {
      fit <- lavaan::inspect(lav_out, what = "fit")
      est <- lavaan::inspect(lav_out, what = "est")
      implied <- lavaan::inspect(lav_out, what = "implied")[[1]]
      rel <- 1 - sum(est$theta)/sum(implied)
      # to obtain subdimensional reliability
      sub_rel <- vector("double", length = length(grp_start))
      for (i in seq_along(grp_start)) { # to conveniently compute the average correlation using na.rm
        sub_uniq <- sum(est$theta[grp_start[i]:grp_end[i], grp_start[i]:grp_end[i]])
        sub_implied <- sum(implied[grp_start[i]:grp_end[i], grp_start[i]:grp_end[i]])
        sub_rel[i] <- 1 - sub_uniq / sub_implied
      }
      # output
      out <- list(rel = rel,
                  sub_rel = sub_rel,
                  fit = fit,
                  est = est)
    } else { # NA for non-convergent solutions
      out <- NA
    }
    invisible(out)
  }
  
  ##########################################################################
  # Names
  ##########################################################################
  con_names <- c(paste( greeks("alpha"), "(tau-equivalent reliability)"),
                 paste("standardized ", greeks("alpha")),
                 paste("ten Berge-Zegers", greeks("mu_4")),
                 paste("standardized ", greeks("mu_4")),
                 "congeneric reliability (CFA)",
                 "standardized congeneric reliability",
                 "principal component analysis reliability")
  ord_names <- c(con_names, "categorical omega")
  multi_names <- c("Stratified alpha (multidimensional tau-equivalent reliability)",
                   "Multidimensional parallel reliability",
                   paste0("Bottom-up approach using ", greeks("mu_4")),
                   "Bottom-up approach using congeneric reliability",
                   "Bottom-up approach using PCA reliability",
                   "Bifactor reliability",
                   "Second-order factors reliability",
                   "Correlated factors reliability")
  
  con_shorts <- c("alpha",
                  "std_alpha",
                  "mu",
                  "std_mu",
                  "cr",
                  "std_cr",
                  "pca")
  ord_shorts <- c(con_shorts, "cat")
  multi_shorts <- c("rmt", "rmp", "rbmu", "rbc", "rbpca", "rbf", "rsof", "rcf")
  
  con_def <-  c("std_mu",
                "std_cr",
                "pca")
  ord_def <- c(con_def, "cat")
  ##########################################################################
  # Open data
  ##########################################################################
  # Read data
  data <- reactive({
    inputFile <- input$inputFile
    #message(class(inputFile))
    if (is.null(inputFile)) { 
      return() 
    } 
    ext <- tools::file_ext(inputFile$name)
    if (ext == "csv") {
      data <- read.csv(file = inputFile$datapath, header = input$header)
    } else if (ext == "xlsx" | ext == "xls") {
      data <- readxl::read_excel(path = inputFile$datapath, col_names = input$header)
    } 
    if (input$header && isCov(data[, -1])) {
      data <- data[, -1]
    }
    return(data)
  })
  # show the ui for sample size if the data are covariances
  output$smpsize_ui <- renderUI({
    req(input$inputFile)
    if (isCov(data())) {
      numericInput("smpsize", "Sample Size", value = 200, min = 1)
    } 
  })
  # Show the data
  output$data_show <- renderTable({
    req(input$inputFile)
    head(data(), n = 20L)
  }, digits = reactive(input$digit))
  ##########################################################################
  # Unidimensional reliability
  ##########################################################################
  # Select items to be used for estimating unidimensional reliability
  output$uni_var_ui <- renderUI({
    varSelectInput("var", "Choose Items to be used for reliability estimation", data = data(), multiple = TRUE)
  })
  # Ask whether the data are continuous or ordinal
  output$conord_ui <- renderUI({
    req(input$inputFile)
    if (!isCov(data())) {
      radioButtons("conord", "Level of measurement",
                   choices = c("Continuous "= "continuous",
                               "Ordinal or dichotomous"= "ordinal"),
                   selected = "ordinal")
    } 
  })
  # Shows different reliability estimators depending on whether the data are continuous or ordinal
  observeEvent(input$conord, {
    cont <- ifelse(isCov(data()), TRUE, myif(input$conord, "continuous"))
    # ifelse(is.null(input$conord), TRUE, 
    #        ifelse(input$conord == "continuous", TRUE, FALSE)))
    if (cont) {
      updateCheckboxGroupInput(
        inputId = "point",
        choiceNames = con_names,
        choiceValues = con_shorts,
        selected = con_def)
    } else {
      updateCheckboxGroupInput(
        inputId = "point",
        choiceNames = ord_names,
        choiceValues = ord_shorts,
        selected = ord_def)
    }
  })
  # Asks if the user wants to obtain confidence interval
  output$uni_ci_ask <- renderUI({
    req(input$inputFile)
    if (!isCov(data())) {
      radioButtons("uni_ci", "95% bootstrap confidence interval (this may take a few minuites)",
                   choices = c("Yes, please"= "yes",
                               "No, thanks"= "no"),
                   selected = "no")
    }
  })
  # Ask which estimator to use for the what-if-item-dropped analysis
  output$ifdrop_detail_ui <- renderUI({
    req(input$ifdrop == "yes")
    selectInput("ifdrop_detail", 
                label = "Which estimator to use",
                choices = find_name(input$point, ord_shorts, ord_names),
                selected = find_name(input$point, ord_shorts, ord_names)[1])
  })
  # At least two variables must be selected for the run button to be active
  output$uni_run_ui <- renderUI({
    req(length(input$var) >= 2)
    actionButton("uni_run", "Run", icon = icon("running"))
  })
  # Calculate the point estimate of unidimensional reliability
  estimate <- eventReactive(input$uni_run, {
    if (isCov(data())) {
      dat <- data()[loc(colnames(data()), input$var), loc(colnames(data()), input$var)]
    } else {
      dat <- data()[, loc(colnames(data()), input$var)]
    }
    calc <- uni_ci(input$point, dat, !myif(input$uni_ci, "no"))
    est <- data.frame(Estimator = find_name(input$point, ord_shorts, ord_names),
                      "Point estimate" = calc$pe)
    if (!myif(input$uni_ci, "no")) {
      est$lower <- calc$lower
      est$upper <- calc$upper
      names(est)[3] <- "95% lower bound"
      names(est)[4] <- "95% upper bound"
    } 
    return(est)
  })
  # Show the  point estimates of unidimensional reliability
  output$uni_table <- renderTable({
    req(input$inputFile)
    estimate()
  }, digits = reactive(input$digit))
  # Download the main table
  output$unidown1_ui <- renderUI({
    req(length(estimate()) >= 1)
    downloadButton("unidown1")
  })
  # Download handle
  output$unidown1 <- downloadHandler(
    filename = function() {
      paste0("unirel_out.csv")
    },
    content = function(file) {
      write.csv(estimate(), file)
    }
  )
  # Shows recommendations from Cho (2022)
  output$rec <- renderText({
    req(input$uni_run)
    n <- ifelse(isCov(data()), input$smpsize, nrow(data()))
    k <- length(input$var)
    cont <- ifelse(isCov(data()), TRUE, myif(input$conord, "continuous"))
    if(cont) {
      if (n < 250 & k >= 5) {
        recom <- ord_names[7] # PCA reliability
      } else {
        recom <- paste(ord_names[4], "or", ord_names[6])
      }
    } else if (n >= 500) {
      recom <- ord_names[8]
    } else if (k >= 5) {
      recom <- paste(ord_names[4], "or", ord_names[7])
    } else {
      recom <- ord_names[6]
    }
    return(paste("Cho's (2022) Recommendation:", recom))
  })
  # Calculates confidence estimates when an item is dropped
  drop_estimate <- reactive({
    if (isCov(data())) {
      dat <- data()[loc(colnames(data()), input$var), loc(colnames(data()), input$var)]
    } else {
      dat <- data()[, loc(colnames(data()), input$var)]
    }
    drop_estimate <- calc_drop(dat, input$ifdrop_detail, input$var)
    point_estimate <- estimate()[which(estimate()[1] == input$ifdrop_detail),2]
    drop <- data.frame(Items = as.character(input$var),
                       estimates = drop_estimate,
                       diff =  drop_estimate - point_estimate)
    names(drop)[2] <- paste(input$ifdrop_detail, "if the item is dropped")
    names(drop)[3] <- "Difference from the original"
    return(drop)
  })
  # Shows the confidence estimates when an item is dropped
  output$drop_out <- renderTable({
    req(input$ifdrop == "yes")
    req(input$uni_run)
    drop_estimate()
  }, digits = reactive(input$digit))
  # show reference
  output$ref <- renderText({
    req(input$uni_run)
    "Cho (2022), Beyond alpha and omega: Choosing a reliability estimator in unidimensional data, submitted for publication"
  })
  ##########################################################################
  # Multidimensional reliability
  ##########################################################################
  output$notice_q <- renderText({
    req(is.null(input$untilFile))
    "Q. What should be done in advance to obtain multidimensional reliability?"
  })
  output$notice_1 <- renderText({
    req(is.null(input$untilFile))
    "A. First, you must upload the data in the Open data tab. The first column of the data must be the first item of the scale to be analyzed. In other words, non-scale data (e.g., identification numbers) must be pre-edited. Items belonging to the same subscale must be placed consecutively, and items of different subscales must not be included in the middle."
  })
  output$notice_2 <- renderText({
    req(is.null(input$untilFile))
    "Next, you must upload a file that stores the column positions of the last items of all subscales . This file should be in the csv format, which is simply a text(txt) format whose contents are separated by commas. For example, suppose that the scale you want to analyze consists of three subscales, each with three, five, and four items, respectively. In this case, the contents of the position you will enter are 3, 8, and 12. The file you will upload must have the following contents."
  })
  output$notice_3 <- renderText({
    req(is.null(input$untilFile))
    "3,8,12"
  })
  output$notice_4 <- renderText({
    req(is.null(input$untilFile))
    "Save the numbers for these positions in csv or txt format using Notepad, Microsoft Word or another editor. However, as a result of my testing, an error occurred when reading a file saved in the csv format using Microsoft Excel."
  })
  output$subtest_byitems <- renderTable({
    
  })
  until <- reactive({
    uf <- input$untilFile
    out <- as.integer(unlist(read.csv(file = uf$datapath, header = FALSE)))
    return(out)
  })
  make_show_subtest <- reactive({
    until <- until()
    num_subtest <- seq(1, length(until))
    itemname <- vector("character", length(until))
    for (i in 1:length(until)) {
      temp <- vector("character", 1)
      if (i == 1) {
        dimstart <- 1
      } else {
        dimstart <- until[i - 1] + 1
      }
      for (j in dimstart:until[i]) {
        temp <- paste(temp, colnames(data())[j])
      }
      itemname[i] <- temp
    }
    out <- data.frame(Subtest = num_subtest,
                      ItemNames = itemname)
  })
  output$show_subtest <- renderTable({
    req(input$untilFile)
    make_show_subtest()
  })
  output$multi_run_ui <- renderUI({
    req(input$untilFile)
    actionButton("multi_run", "Run", icon = icon("running"))
  })
  # Calculate the point estimate of unidimensional reliability
  multi_estimate <- eventReactive(input$multi_run, {
    ndim <- length(until())
    until <- until()[-ndim]
    grp_start <- c(1, until + 1)
    subname <- vector("character", ndim)
    calc <- get_multi(input$multi_choice, data(), until)
    multi <- data.frame("estimator" = find_name(input$multi_choice, multi_shorts, multi_names),
                        "Test reliability" = calc$rel,
                        "omega hierarchical" = calc$omegah)
    for (i in 1:ndim) {
      #subname[i] <- paste(colnames(data())[grp_start[i]], "etc")
      subname[i] <- paste("subtest", i)
      multi[, subname[i]] <- calc$sub_rel[, i]
    }
    # fit indices
    if (input$ifcfa == "yes") {
      if (input$cfa_choose == "bf") {
        cfaout <- bifactor(data(), until = until)
        facnames <- c("general factor", paste0("subtest", 1:ndim))
        beta <- NULL
      } else if (input$cfa_choose == "sof") {
        cfaout <- second_order(data(), until = until)
        facnames <- c(paste0("subtest", 1:ndim), "second-order factor")
        colnames(cfaout$est$beta) <- facnames
        beta <- data.frame("Factors" = facnames, cfaout$est$beta)
      } else {
        cfaout <- correlated_factors(data(), until = until)
        facnames <- paste0("subtest", 1:ndim)
        beta <- NULL
      }
      colnames(cfaout$est$lambda) <- colnames(cfaout$est$psi) <- facnames
      loc <- c(1, 2, 3, 4, 5, 9, 10, 23)
      fitname <- names(cfaout$fit)[loc]
      fitnumber <- cfaout$fit[loc]
      fit <- data.frame("Fit indices" = fitname, "output" = fitnumber)
      lambda <- data.frame("Items" = colnames(data()), cfaout$est$lambda)
      theta <- data.frame("Items" = colnames(data()),
                          "Error variance" = diag(cfaout$est$theta))
      psi <- data.frame("Factors" = facnames, cfaout$est$psi)
    } else {
      fit <- lambda <- theta <- psi <- beta <- NA
    }
    
    
    out <- list(multi = multi, fit = fit, lambda = lambda, theta = theta, psi = psi, beta = beta)
    return(out)
  })
  output$multi_table <- renderTable({
    req(input$multi_run)
    multi_estimate()$multi
  }, digits = reactive(input$digit))
  
  # Download the main table
  output$multidown1_ui <- renderUI({
    req(length(multi_estimate()) >= 1)
    downloadButton("multidown1")
  })
  output$multidown1 <- downloadHandler(
    filename = function() {
      paste0("multirel_out.csv")
    },
    content = function(file) {
      write.csv(multi_estimate()$multi, file)
    }
  )
  output$cfa_choose_ui <- renderUI({
    req(input$ifcfa == "yes")
    selectInput("cfa_choose", 
                label = "Which CFA model to show",
                choices = c("Bifactor model" = "bf",
                            "Second-order factor model" = "sof",
                            "Correlated factors model" = "cf"),
                selected = "bf")
  })
  output$cfa_fit <- renderTable({
    req(input$ifcfa == "yes")
    multi_estimate()$fit
  }, digits = reactive(input$digit))
  output$cfa_lambda <- renderTable({
    req(input$ifcfa == "yes")
    multi_estimate()$lambda
  }, digits = reactive(input$digit),
  caption=paste("Factor loadings (lambda, coefficients between factors and items)"),
  caption.placement = getOption("xtable.caption.placement", "top")
  )
  output$cfa_theta <- renderTable({
    req(input$ifcfa == "yes")
    multi_estimate()$theta
  }, digits = reactive(input$digit),
  caption=paste("Item error variance (theta)"),
  caption.placement = getOption("xtable.caption.placement", "top")
  )
  output$cfa_psi <- renderTable({
    req(input$ifcfa == "yes")
    multi_estimate()$psi
  }, digits = reactive(input$digit),
  caption=paste("Covariances of factors (psi)"),
  caption.placement = getOption("xtable.caption.placement", "top")
  )
  output$cfa_beta <- renderTable({
    req(input$ifcfa == "yes" & input$cfa_choose == "sof")
    multi_estimate()$beta
  }, digits = reactive(input$digit),
  caption=paste("Coefficients between factors (beta)"),
  caption.placement = getOption("xtable.caption.placement", "top")
  )
}

shinyApp(ui = ui, server = server)