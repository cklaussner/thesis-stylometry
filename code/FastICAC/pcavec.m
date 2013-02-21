

%based on FastICA package of Matlab 7.x and 6.x
  % Version 2.5, October 19 2005 Copyright (c) Hugo Gävert, Jarmo Hurri, Jaakko Särelä, and Aapo Hyvärinen.
   %This program is free software: you can redistribute it and/or modify
   % it under the terms of the GNU General Public License as published by
   % the Free Software Foundation, either version 3 of the License, or
   % (at your option) any later version.

    %This program is distributed in the hope that it will be useful,
    %but WITHOUT ANY WARRANTY; without even the implied warranty of
    %MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    %GNU General Public License for more details.

function [E, D] = pcavec(vectors);


  covarianceMatrix = cov(vectors', 1);

  % Calculate the eigenvalues and eigenvectors of covariance matrix.
  [E, D] = eig (covarianceMatrix);