# ==================================== ITEM INFO FUNCTION for NGSS ================================================
## Description: 
##  At given theta(s), this function computes the item information for a standalone item or a cluster item
##  For standalone item, it currently supports 3PL items
##  For cluster item, it calculates the marginal item information, where "marginal" means 
##  the nuisance dimension being integrated out. Currently supports Rasch testlet model items 

## Inputs:
##   theta: a vector of thetas or a scalar value of theta
##   SA_parm: a dataframe or matrix of item parameters for standalone items. 
##            Columns are a, b, g, ItemID and AssertionID
##           *Columns must be in the above order
##   Cluster_parm: a dataframe or matrix of item parameters for cluster items. 
##                 Columnes are a, b, variance parameters for each assertion, cluster position, cluster ItemID, and AssertionID
##                *Columns must be in the above order
##   Dv: scaling factor for IRT models [1 or 1.7]
##   n.nodes: number of nodes used when integrating out the specific dimension 

## Return values:
##  When length(theta)==1: a vector containing the theta, all SA items info, and all cluster items info 
##  When length(theta)>1: a dataframe with a column for theta, columns for all SA items info, and colums and all cluster items info 

## Author: Zhongtian Lin
## Last Updated: 09/21/2020
# =================================================================================================================
item.info = function(theta, SA_parm=NULL, Cluster_parm=NULL, Dv=1, n.nodes = 50){
  if(is.null(SA_parm) & is.null(Cluster_parm)) {stop("No item found!!!")} 
  # -------------------------------------------------
  # ---------------- SA item chuck ------------------
  # -------------------------------------------------
  # +++ add flexibility for different models (SA_parm with only a and b or even a vector with only b) +++
  if(is.null(SA_parm)) {
    info_SA = NULL
  } else {
    SA_parm = as.data.frame(SA_parm)
    names(SA_parm) = c("a","b","g","ItemID","Assertion_ID")
    a = SA_parm[,1]
    b = SA_parm[,2]
    g = SA_parm[,3]
    a.parm = rep(1,length(theta)) %o% a
    b.parm = rep(1,length(theta)) %o% b
    g.parm = rep(1,length(theta)) %o% g
    theta.parm = theta %o% rep(1,length(b))
    lin_pred = Dv * a.parm *(theta.parm - b.parm)
    probs.SA = g.parm + (1 - g.parm) * plogis(lin_pred)
    info_SA = (Dv*a.parm)^2 * ((1-probs.SA)/probs.SA) * ((probs.SA - g.parm) / (1 - g.parm))^2
    info_SA = as.data.frame(info_SA)
    names(info_SA) = SA_parm$ItemID
    info_SA = split.default(info_SA, names(info_SA))
    info_SA = sapply(info_SA, rowSums)
  }
  
  # -------------------------------------------------
  # ------------- Cluster item chuck ----------------
  # -------------------------------------------------
  # +++ add flexibility for cluster variance in a different file and one value per item situation  +++
  if(is.null(Cluster_parm)) {
    info_Cluster = NULL
  } else {
    gq = statmod::gauss.quad.prob(n.nodes, dist = 'normal', sigma = 1)
    nodes = gq$nodes
    whts = gq$weights
    
    names(Cluster_parm) = c("a","b","cluster_var","position","ItemID","Assertion_ID")
    Cluster_parm$position = dense_rank(Cluster_parm$position) # reorder the position so it starts from 1
    info_Cluster = list()
    for (k in 1:length(unique(Cluster_parm$position))) {
      one_cluster = filter(Cluster_parm, position == k)
      mvars = one_cluster$cluster_var
      rescaled.nodes = nodes %o% sqrt(mvars) # rescaled nodes for these assertions
      ma = one_cluster$a # a parameters for these assertions
      mb = one_cluster$b # b parameters for these assertions
      mtheta = theta  # theta in a vector
      n.ass = length(mb)
      
      # Execute the Lord and Wingersky function
      LW_results = LW(cluster_var = mvars, a = ma, b = mb, theta = mtheta, n.nodes = n.nodes, return_additional = T, Dv=Dv)
      prk.marginal = LW_results$prk.marginal
      probs = LW_results$probs
      qrobs = LW_results$qrobs
      
      rawscores = 0:n.ass
      rawscoresXnodes = 0:n.ass %o% rescaled.nodes[,1]
      temp1 = apply(log(qrobs), c(1,2), sum)
      temp2 = lapply(1:nrow(rawscoresXnodes), function(x) sweep(temp1, 2, rawscoresXnodes[x,], "+"))
      temp3 = exp(array(as.numeric(unlist(temp2)), dim=c(dim(temp2[[1]]), length(temp2))))  # every person, every node, every possible raw score
      temp4 = temp3 * outer(-apply(probs, c(1,2), sum), rawscores, "+")
      
      numer = apply(temp4, 3, function(x) x %*% whts)
      denom = apply(temp3, 3, function(x) x %*% whts)
      
      info_Cluster[[k]] = -rowSums(t(prk.marginal) * -(numer/denom)^2)
    }
    names(info_Cluster) = unique(Cluster_parm$ItemID)
    info_Cluster = simplify2array(info_Cluster)
  }
  
  # -------------------------------------------------
  # -------------------- Output ---------------------
  # -------------------------------------------------
  if (length(theta)>1) {
    item_info = as.data.frame(cbind(theta, info_SA, info_Cluster))
  } else {
    item_info = c(theta=theta, info_SA, info_Cluster)
  }
  return(item_info)
}