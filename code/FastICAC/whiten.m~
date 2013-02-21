
function [newVectors, whiteningMatrix, dewhiteningMatrix] = whiten(vectors, E, D);

  whiteningMatrix = inv (sqrt (D)) * E';
  dewhiteningMatrix = E * sqrt (D);

  % Project to the eigenvectors of the covariance matrix.
  % Whiten the samples and reduce dimension simultaneously.
  %if b_verbose, fprintf ('Whitening...\n'); end
  newVectors =  whiteningMatrix * vectors;