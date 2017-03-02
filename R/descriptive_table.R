#' Generate html table of descritive statistics for data 
#'
#' @param dataframe Data to be used
#' @param num       A vector of numeric variable names
#' @param char      A vector of categorical variable names
#' @param grp       Group names, limited to one name
#' @param title     Title of the table
#' @param label.option Default is \code{'FALSE'}.\code{'TURE'} uses variable label rather than variable name
#' @param option    Default is \code{'FALSE'}. \code{'TRUE'} uses min and max for numeric variable and percentage for categorical variable
#' @param digit     Round the numeric statistics except for p-value. P-value always had 3 decimals.
#' @param num.test  A character string specifying the test method for numeric variable among groups, 
#' must be one of "None", "ANOVA", "Wilcoxon/Kruskal". "Wilcoxon/Kruskal" means if the group variable has two groups, 
#' then use Wilcoxon sum rank test, if has more than two groups, use Kruskal-Wallis test.
#'
#' @import plyr  htmlTable
#' @return Desciptive tables for selected variables, median (min-max) for numeric variable, n(\%) for characteristic
#'  variables. ANOVA for numeric variables with normality assumption; Wilcoxon test for non-parametric variable and 
#'  two groups; Kruiskal test for more than two groups (excluding missing group level) Chi-square test for 
#'  categorical variables (excluding missing levels, both variable and group)
#'  
#' @export
#'

descriptive_table <- function(dataframe, num, char, grp, title="Table", 
                              label.option=F, option=F, digit=1, 
                              num.test=c("None", "Wilcoxon/Kruskal", "ANOVA")){

  # inspect variables input
  if(!num.test %in% c("None", "ANOVA", "Wilcoxon/Kruskal")) {stop("num.test must be one of \"None\", \"ANOVA\", \"Wilcoxon/Kruskal\"")}
  
  #############
  # libraries #
  #############
  require(plyr)
  require(htmlTable)
  
  # Get dataset
  dt            <- get(dataframe)
  var.grp       <- addNA(dt[, grp], ifany = T) # add <NA> level if any
  var.grp.naomit<- factor(dt[, grp]) #default na.omit, no <NA> level for wilcoxon/kruskal analysis
  var           <- c(num, char)[!is.na(c(num, char))]
  rslt          <- NULL
  rslt.list.all <- as.list(1:length(var)) # exlude NA in num or char
  rslt.list.grp <- as.list(1:length(var))
  rslt.list.p   <- as.list(1:length(var)) 
  names(rslt.list.all) <- names(rslt.list.grp) <- names(rslt.list.p) <- var
  
  # numeric variables
  if(!anyNA(num)){#if missing then skip
    for(i in 1:length(num)){
      if(FALSE){
        var.num    <- dt[, num[i]]
        n.all      <- sprintf(paste0("%.", digit, "f"), mean(var.num, na.rm=T))                  #class(): character
        n.grp      <- sprintf(paste0("%.", digit, "f"), tapply(var.num, var.grp, mean, na.rm=T)) #class(): character
        
        if(option){# option=T then add SD
          sd.all   <- sprintf(paste0("%.", digit, "f"), sd(var.num, na.rm=T))
          n.all    <- paste0(n.all, " +/- ", sd.all) #class(): character
          sd.grp   <- sprintf(paste0("%.", digit, "f"), tapply(var.num, var.grp, sd, na.rm=T))
          n.grp    <- paste0(n.grp, " +/- ", sd.grp) #class(): character
        }
        rslt.list.p[[i]] <- rslt.list.all[[i]] <- data.frame(n.all, stringsAsFactors=F) #prevent changing from character to factor 
        rownames(rslt.list.all[[i]]) <- num[i]
        colnames(rslt.list.all[[i]]) <- "All"
        
        rslt.list.grp[[i]]           <- data.frame(t(n.grp), stringsAsFactors=F)
        rownames(rslt.list.grp[[i]]) <- num[i]
        colnames(rslt.list.grp[[i]]) <- ifelse(is.na(levels(var.grp)), "Missing", levels(var.grp))
        
        p.temp                       <- anova(lm(var.num~var.grp))$P[1]
        if(p.temp <0.001){ p.value   <- "<0.001"}else{ p.value <- sprintf("%.3f", p.temp)}
        rslt.list.p[[i]]             <- data.frame(p=c(p.value, rep(NA, nrow(rslt.list.p[[i]])-1)))
        rownames(rslt.list.p[[i]])   <- num[i]
        colnames(rslt.list.p[[i]])   <- "p-value"
      }# if(FALSE)
      
      var.num    <- dt[, num[i]]
      n.all      <- sprintf(paste0("%.", digit, "f"), median(var.num, na.rm=T))                  #class(): character
      n.grp      <- sprintf(paste0("%.", digit, "f"), tapply(var.num, var.grp, median, na.rm=T)) #class(): character
      if(option){# option=T then add min and max
        min.all  <- sprintf(paste0("%.", digit, "f"), min(var.num, na.rm=T))
        max.all  <- sprintf(paste0("%.", digit, "f"), max(var.num, na.rm=T))
        n.all    <- paste0(n.all, " (", min.all, "-", max.all, ")") #class(): character  
        min.grp  <- sprintf(paste0("%.", digit, "f"), tapply(var.num, var.grp, min, na.rm=T))
        max.grp  <- sprintf(paste0("%.", digit, "f"), tapply(var.num, var.grp, max, na.rm=T))
        n.grp    <- paste0(n.grp, " (", min.grp, "-", max.grp, ")") #class(): character
      }
      rslt.list.p[[i]] <- rslt.list.all[[i]] <- data.frame(n.all, stringsAsFactors=F) #prevent changing from character to factor 
      rownames(rslt.list.all[[i]]) <- num[i]
      colnames(rslt.list.all[[i]]) <- "All"
      
      rslt.list.grp[[i]]           <- data.frame(t(n.grp), stringsAsFactors=F)
      rownames(rslt.list.grp[[i]]) <- num[i]
      colnames(rslt.list.grp[[i]]) <- ifelse(is.na(levels(var.grp)), "Missing", levels(var.grp))
      
      if(num.test!="None"){
        if(num.test=="ANOVA"){
          p.temp                     <- anova(lm(var.num~var.grp))$P[1]
        }else if(num.test=="Wilcoxon/Kruskal"){
          if(length(levels(var.grp.naomit)) <= 2){ #exclude missing group level
            p.temp                   <- wilcox.test(var.num[!is.na(dt[, grp])] ~ var.grp.naomit[!is.na(dt[, grp])])$p.value #two groups (default na.omit)
          }else{
            p.temp                   <- kruskal.test(var.num[!is.na(dt[, grp])] ~ var.grp.naomit[!is.na(dt[, grp])])$p.value #more than two groups (default na.omit)
          } 
        }
        
        if(p.temp <0.001){ p.value   <- "<0.001"}else{ p.value <- sprintf("%.3f", p.temp)}
        rslt.list.p[[i]]             <- data.frame(p=c(p.value, rep(NA, nrow(rslt.list.p[[i]])-1)))
        rownames(rslt.list.p[[i]])   <- num[i]
        colnames(rslt.list.p[[i]])   <- "p-value"
      }
      
    }#for
  }#if anyNA(num)
  
  i       <- length(num[!is.na(num)]) #continue from numeric. if NA, i=0
  n.rgroup <- c(rep(1, length(num[!is.na(num)])))
  # categorical variables
  if(!anyNA(char)){#if any missing then skip
    for(j in 1:length(char)){
      # convert character variables to factor and add <NA> level if any
      var.char   <- addNA(as.factor(dt[, char[j]]), ifany=T)
      n.rgroup   <- c(n.rgroup, length(levels(var.char))) #for htmlTable
      c.all      <- table(var.char)          #class(c.all[1]): integer
      c.grp      <- table(var.char, var.grp) #class(c.grp[1,]): integer
      rownames(c.all) <- rownames(c.grp) <- ifelse(is.na(levels(var.char)), "Missing", levels(var.char))
      
      if(option){# option=T then add percentage
        pct.all <- sprintf(paste0("%.", digit, "f"), prop.table(c.all)*100)#no need margin
        pct.grp <- sprintf(paste0("%.", digit, "f"), prop.table(c.grp, margin = 2)*100)#column proportion
        c.all   <- data.frame(paste0(c.all, " (", pct.all, "%)"), stringsAsFactors=F)
        c.grp   <- matrix(paste0(c.grp, " (", pct.grp, "%)"), nrow=length(levels(var.char)))# return vector to dataframe
      }else{
        c.all   <- cbind(c.all)  #class(c.all[1]): integer
      }
      rslt.list.p[[i+j]] <- rslt.list.all[[i+j]]           <- data.frame(c.all)
      rownames(rslt.list.all[[i+j]]) <- ifelse(is.na(levels(var.char)), "Missing", levels(var.char))
      colnames(rslt.list.all[[i+j]]) <- "All"
      
      rslt.list.grp[[i+j]]           <- as.data.frame.matrix(c.grp)
      rownames(rslt.list.grp[[i+j]]) <- ifelse(is.na(levels(var.char)), "Missing", levels(var.char))
      colnames(rslt.list.grp[[i+j]]) <- ifelse(is.na(levels(var.grp)), "Missing", levels(var.grp))
      
      if(num.test!="None"){
        t <- table(var.char, var.grp)
        p.temp                         <- chisq.test(t[!is.na(rownames(t)), !is.na(colnames(t))])$p.value #Chi-square test excluding both missing levels
        if(p.temp < 0.001){ p.value     <- "<0.001"}else{ p.value <- sprintf("%.3f", p.temp)}
        rslt.list.p[[i+j]]             <- data.frame(p=c(p.value, rep(NA, nrow(rslt.list.p[[i+j]])-1)))
        rownames(rslt.list.p[[i+j]])   <- ifelse(is.na(levels(var.char)), "Missing", levels(var.char))
        colnames(rslt.list.p[[i+j]])   <- "p-value"
      }
    }#End For
  }#if anyNA(char)
  
  #could be improved here, like how to merge 
  rslt.all <- do.call(rbind, rslt.list.all)
  rowname  <- rownames(rslt.all)
  rslt.all <- cbind(rowname, rslt.all)
  rslt.grp <- do.call(rbind, rslt.list.grp)
  rowname  <- rownames(rslt.grp)
  rslt.grp <- cbind(rowname, rslt.grp)
  #rslt    <- merge(rslt.all, rslt.grp, by="row.names")
  rslt     <- join(rslt.all, rslt.grp, by="rowname")
  
  if(num.test!="None"){
    rslt.p  <- do.call(rbind, rslt.list.p)
    rowname <- rownames(rslt.p)
    rslt.p  <- cbind(rowname, rslt.p)
    rslt    <- join(rslt, rslt.p, by="rowname")
  }
  
  rownames(rslt) <- rslt$rowname 
  rslt     <- subset(rslt, select = -rowname)
  
  table_rownames <- gsub("^.*?\\.", "", rownames(rslt)) #only keep the string after period
  table_rownames[1:length(num)] <- "" #set the numeric variables' rownames to be space
  
  table <- htmlTable(rslt,
                     caption  = title,
                     align    = "r",
                     rowlabel = "Characteristics",
                     #rnames  = gsub("^.*?\\.", "", rownames(rslt)), #only keep the string after period
                     rnames   = table_rownames,
                     rgroup   = if(label.option) lapply(var, function(x){if (label(dt[, x])=="") var[var==x] else label(dt[,x])}) else var, # row group name
                     n.rgroup = n.rgroup,
                     cgroup   = if(num.test=="None") c("", if(label.option) label(dt[, grp]) else grp) else c("", if(label.option) label(dt[, grp]) else grp, ""),
                     n.cgroup = if(num.test=="None") c(1, length(levels(var.grp)))                     else c(1, length(levels(var.grp)), 1)
  )#EndHtml
  return(table)
}
