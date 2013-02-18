


function xwhit = PCA_whit(signals)
    

    
    x=signals-repmat(mean(signals,2),1,size(signals,2)); %center data

    sigma = x * x' / size(x, 2);  % cov
  
    [U,S,V] = svd(sigma);  % singular value decomposition
    

     xwhit = diag(1./sqrt(diag(S))) * U' * x;