


function xwhit = ICA_whit(signals)

% Centering
x=signals-repmat(mean(signals,2),1,size(signals,2));

% Linear whitening 
% Covariance matrix
covariance=cov(x');
% Eigenvectors and eigenvalues of the covariance matrix
[eigenvectors,eigenvalues] = eig(covariance);
% Linear whitening
xwhit = eigenvectors * (inv(sqrt(eigenvalues))) * eigenvectors' * x;