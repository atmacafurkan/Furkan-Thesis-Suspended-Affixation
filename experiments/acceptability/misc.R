# TODO: Figure out how to treat missing data in this function
# TODO: Make sure this is strictly a within-participants design. Strange things happen to the means if one tries to process two experiments at once, even if the experiment is used as a grouping factor in the group argument
se_cousineau <- function(df, n_conditions, subject, DV, group, is_proportion = NULL)
{
  stopifnot(!"avgDV" %in% colnames(df))
  subject_var <- substitute(subject) %>% deparse()
  DV <- substitute(DV) %>% deparse()
  
  subj_means <- df %>% group_by(.dots = subject_var) %>% dplyr::summarize(avgDV := mean(!!as.name(DV), na.rm = T))
  GM <- mean(subj_means$avgDV)
  df %<>% group_by(.dots = subject_var) %>% dplyr::mutate(nDV = !!as.name(DV) - mean(!!as.name(DV), na.rm = T) + GM )
  
  if (is.null(is_proportion)) {
    dv <- df[[DV]]
    dv_unique <- unique(dv)
    if ( is.logical(dv) || (length(dv_unique) == 2 && all(dv_unique %in% c(0,1))) ) {
      is_proportion <- TRUE
    } else {
      is_proportion <- FALSE
    }
  }
  
  var_correction_factor <- n_conditions/(n_conditions-1)
  df %>% group_by(.dots = group) %>% 
    dplyr::summarize(M = mean(nDV, na.rm = T),
                     Var = ifelse(is_proportion, M*(1-M), var(nDV, na.rm = T)) * var_correction_factor,
                     #Var = var(nDV, na.rm = T) * var_correction_factor,
                     N = sum(!is.na(nDV)),
                     SE = sqrt(Var/N) )
}


# 
# se_cousineau <- function(df, n_conditions, subject, DV, group, is_proportion = NULL)
# {
#   stopifnot(!"avgDV" %in% colnames(df))
#   subject_var <- substitute(subject) %>% deparse()
#   DV <- substitute(DV) %>% deparse()
#   
#   subj_means <- df %>% group_by(.dots = subject_var) %>% dplyr::summarize(avgDV := mean(!!as.name(DV), na.rm = T))
#   #subj_means$avgDV <- mean(subj_means$avgDV)
#   GM <- mean(subj_means$avgDV)
#   df %<>% left_join( subj_means, by = subject_var )
#   df %<>% group_by(.dots = subject_var) %>% dplyr::mutate(nDV := !!as.name(DV) - avgDV)
#   
#   #df
#   df %>% group_by(.dots = group) %>% dplyr::summarize( M = mean(nDV, na.rm = T),# + GM,
#                                                        N_yes = sum(ResponseYes == T, na.rm = T),
#                                                        N = sum(!is.na(nDV))
#                                                        )
# }
# 
# 


nunique <- function(x) length(unique(x))


read_file <- function(fname) { readChar(fname, file.info(fname)$size) }




lognormalParamMeanSigma <- custom_family(
  "lognormalParamMeanSigma", dpars = c("mu", "sigma"),
  links = c("identity", "log"), lb = c(0, 0),
  type = "real"
)

stan_funs_lognormalParamMeanSigma <- "
real lognormalmean2mu(real mean, real sigma) {
real mu;
if (mean < 25) {
mu = log( mean + (exp(mean)-mean)/(exp(2*mean) + 1) ) - sigma^2/2;
} else {
mu = log( mean ) - sigma^2/2;
}
return mu;
}
real lognormalParamMeanSigma_lpdf(real y, real mean, real sigma) {
return lognormal_lpdf(y | lognormalmean2mu(mean, sigma), sigma);
}
real lognormalParamMeanSigma_rng(real mean, real sigma) {
return lognormal_rng(lognormalmean2mu(mean, sigma), sigma);
}
"
stanvars_lognormalParamMeanSigma <- stanvar(scode = stan_funs_lognormalParamMeanSigma, 
                                            block = "functions")




prob2odds_str <- function(p, round_from = 5) {
  odds <- p/(1-p)
  odds_inv <- odds <= 1
  odds_round <- (odds >= round_from) | (odds <= 1/round_from)
  odds <- ifelse(odds_inv, 1/odds, odds)
  odds <- ifelse(odds_round, round(odds), odds)
  template <- ifelse(odds_inv, 
                     ifelse(odds_round, "1:%0.0f", "1:%0.1f"), 
                     ifelse(odds_round, "%0.0f:1", "%0.1f:1"))
  sapply(seq_along(template), function(i) { sprintf(template[i], odds[i]) })
}


prob_str <- function(p, gtst = 0.001) {
  if (p < .001) {
    str <- "< .001"
  } else if (p > .999) {
    str <- "> .999"
  } else if (p > .99 | p < .01 ) {
    str <- sprintf("  %.3f", p) %>% gsub("0\\.", ".", .)
  } else {
    str <- sprintf("   %.2f", p) %>% gsub("0\\.", ".", .)
  }
  str
}


model_summary <- function(m, include_pp_below_zero = T)
{
  tbl <- fixef(m)[-1,-2] %>% as.data.frame()
  tbl$coef <- rownames(tbl)
  
  if (include_pp_below_zero) {
    cnames <- paste("b", tbl$coef, sep = "_")
    samples <- brms::posterior_samples(m, pars = cnames)
    stopifnot(ncol(samples) == length(cnames))
    
    pref_coef_stats_df <- function(df, name) {
      df %>% as.data.frame(colnames = "x") %T>% 
      { colnames(.) <- name } %T>%
      { .$coef <- rownames(.) %>% gsub("^b_", "", .) }
    }
    
    p_below_zero <- samples %>% sapply(function(x) mean(x < 0)) %>% 
      pref_coef_stats_df("PBelowZero")
    tbl %<>% left_join(p_below_zero, by = "coef")
    
    p_below_zero_str <- samples %>% sapply(function(x) mean(x < 0) %>% prob_str()) %>% 
      pref_coef_stats_df("PBelowZeroStr")
    tbl %<>% left_join(p_below_zero_str, by = "coef")
    
    p_above_zero <- samples %>% sapply(function(x) mean(x > 0)) %>% 
      pref_coef_stats_df("PAboveZero")
    tbl %<>% left_join(p_above_zero, by = "coef")
    
    p_above_zero_str <- samples %>% sapply(function(x) mean(x > 0) %>% prob_str()) %>% 
      pref_coef_stats_df("PAboveZeroStr")
    tbl %<>% left_join(p_above_zero_str, by = "coef")
    
  }
  
  rownames(tbl) <- tbl$coef
  tbl
}


# TODO: In addition to label_max_width, add another argument, strip_label_max_terms,
#       which inserts a line break on a by-term basis
#       Alternatively, write a labeller, which finds the closest interaction symbol next to 
#       the character maximum, and breaks there.
create_model_coefs_plot <- function(m, 
                                    interaction_panels = c(), 
                                    strip_label_max_characters = NULL, 
                                    map_names = NULL,
                                    exclude_names = NULL,
                                    plot_stats = FALSE, 
                                    expand_right = 1, 
                                    expand_top = 1,
                                    x_stat_adjust = 0,
                                    x_breaks = ggplot2::waiver(),
                                    x_minor_breaks = ggplot2::waiver())
{
  interaction_symbol <- " * "
  use_interaction_panels <- length(interaction_panels) > 0
  
  if ( "brmsfit" %in% class(m) ) {
    tbl <- model_summary( m #, include_pp_below_zero = plot_stats 
    )
    
  } else if (is.list(m)) {
    stopifnot( length(names(m)) == length(unique(names(m))) )
    
    tbl <- plyr::ldply(seq_along(m), function(i) { 
      tbl <- model_summary( m[[i]] #, include_pp_below_zero = plot_stats 
      )
      tbl$model <- names(m)[i]
      tbl
    })
    tbl$model %<>% factor( levels = names(m) )
    tbl
    
  } else {
    stop("Unknown model format.")
  }
  tbl %<>% subset(!coef %in% exclude_names)
  
  # rename some rows 
  if (length(map_names) > 0) {
    for (i in seq_along(map_names)) {
      idx <- which(tbl$coef == names(map_names)[i])
      if (length(idx) > 0) {
        if (map_names[i] == "") {
          tbl <- tbl[-idx,]
        } else {
          tbl$coef[idx] <- map_names[i]
        }
      }
    }
  }
  
  if (use_interaction_panels) {
    tbl$interaction <- ""
  }
  for (cur_interaction in interaction_panels) {
    cur_interaction_term1 <- paste0(cur_interaction,":")
    cur_interaction_term2 <- paste0(":",cur_interaction)
    
    is_target_interaction <- grepl(cur_interaction_term1, tbl$coef) | grepl(cur_interaction_term2, tbl$coef)
    
    tbl$coef[is_target_interaction] %<>% gsub(cur_interaction_term1, "", .) %>% 
      gsub(cur_interaction_term2, "", .)
    
    tbl$interaction[is_target_interaction] <- paste0(cur_interaction, interaction_symbol, "...")
  }
  
  # replace interaction symbol if necessary
  if (interaction_symbol != ":") {
    tbl$coef %<>% gsub("([^ ]):([^ ])", paste0("\\1", interaction_symbol, "\\2"), .)
    
    if (use_interaction_panels)
      tbl$interaction %<>% gsub("([^ ]):([^ ])", paste0("\\1", interaction_symbol, "\\2"), .)
  }
  coefs_order <- c(rev(map_names), rev(tbl$coef)) %>% unique() # %>% rev()
  tbl$coef %<>% factor(levels = coefs_order)
  #tbl$coef %<>% factor(levels = tbl$coef %>% unique %>% rev())
  
  # plot
  p <- ggplot(tbl, aes(Estimate, coef)) + geom_point() + 
    geom_errorbarh(aes(xmin = Q2.5, xmax = Q97.5), height=0) + 
    geom_vline(xintercept = 0, color = "grey")
  
  if (plot_stats)
  {
    tbl$xmax <- with(tbl, max(c(Estimate, Q2.5, Q97.5))) + x_stat_adjust
    
    
    p <- p + scale_y_discrete(expand = expand_scale(mult = c(.05, .15*expand_top), 
                                                    add = c(0, 0))
    )
    p <- p + scale_x_continuous(expand = expand_scale(mult = c(.05, .15*expand_right), 
                                                      add = c(0, 0)),
                                breaks = x_breaks, 
                                minor_breaks = x_minor_breaks)
    
    p <- p + geom_text(aes(x = tbl$xmax, y = tbl$coef,#_idx, 
                           label = sprintf("[%s]", tbl$PBelowZeroStr)), 
                       family = "mono", hjust = "left")
    suppressWarnings({
      label <- parse(text = "underline(paste('P(', theta, ' < 0)'))")
      p <-  p + geom_text(x = tbl$xmax[1], y = length(unique(tbl$coef))+1, 
                          label = label,
                          family = "mono", hjust = "left")#, fontface = "underlined")
    })
  }
  
  if (use_interaction_panels) {
    p <- p + facet_wrap(~ interaction, strip.position = "left", ncol = 1, scales = "free_y")
    if (!is.null(strip_label_max_characters))
      p <- p + label_wrap_gen(width = strip_label_max_characters)
  }
  
  if ( !is.null(tbl$model) ) {
    p <- p + facet_wrap(~model)
  }
  
  p <- p + theme_bw() + 
    theme(panel.border = element_blank(), 
          axis.ticks.y = element_blank(),
          #strip.text.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.placement = "outside") +
    ylab("")
  
  return (p)
}


print_estimate_with_ci <- function(model, contr_name, fmt = "%0.2f") {
  full_fmt <- sprintf("$\\hat{\\beta}=%s;$ $CI=[%s; %s];$ $P(\\beta<0)%s$", fmt, fmt, fmt, "%s")
  est <- fixef(model, summary = T, robust = F) %>% .[contr_name,]
  post_prob <- model_summary(model) %>% .[contr_name,]
  post_prob %<>% mutate( is_extreme = post_prob[['PBelowZero']] < .001 | post_prob[['PBelowZero']] > .999 )
  post_prob %<>% mutate( PBelowZeroStr = ifelse(is_extreme, as.character(PBelowZeroStr), paste("=", PBelowZeroStr) ) )
  sprintf(full_fmt, est[['Estimate']], est[['Q2.5']], est[['Q97.5']], post_prob[['PBelowZeroStr']])
}