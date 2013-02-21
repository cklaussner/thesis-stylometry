
function [A,W] = fastICA(mixedsig, no_comp) % takes as input mixing matrix (document-term! matrix) and desired no- of components 



    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Centering
     [vectors]= rmvmean(mixedsig); %%% zero mean
      
      
     [E, D] = pcavec(vectors) %%%%%%%%%%%%%%%%%%%%%%%5 Eigenvalue + Eigenvector matrix
     
     %%%%%%%%%%%%%%%%%%%%%%% Calculate the whitening
     [vec, whiteningMatrix, dewhiteningMatrix] = whiten(mixedsig, E, D);
     
     
     [A,W] = fpICA(vec,whiteningMatrix, dewhiteningMatrix,no_comp)
     
     