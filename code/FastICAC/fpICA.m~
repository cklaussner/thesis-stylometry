
function [A, W] = fpICA(X, whiteningMatrix, dewhiteningMatrix, numOfIC)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Parameters

maxNumIterations = 1000;
epsilon = 0.0001; 
%stabilization = 'on'; end
myy = 1;
a2 = 1;
a1 = 1; 
gOrig= 20;
failureLimit = 5;
usedNlinearity = gOrig;
[vectorSize, numSamples] = size(X);
myyOrig = myy; %% stupid
maxFinetune = 100;
finetune = 'off';
finetuningEnabled = 0;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% check for dimension size - can only retrieve components <= no. of observations
  
  if vectorSize < numOfIC
      error('Must have numOfIC <= Dimension!');
  end

  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Deflation Approach (estimating one component at a time until round = no_of_comp)
  
  B = zeros(vectorSize);
  
   % The search for a basis vector is repeated numOfIC times.
  round = 1;
  
  numFailures = 0;
  
  while round <= numOfIC,
    myy = myyOrig;
    usedNlinearity = gOrig;
    notFine = 1;
    endFinetuning = 0;
   
    
    % Take a random initial vector of lenght 1 and orthogonalize it
    % with respect to the other vectors.
    
      w = randn (vectorSize, 1);
    
      w = w - B * B' * w;
      w = w / norm(w);
    
    
    wOld = zeros(size(w));
    wOld2 = zeros(size(w));
    
    % This is the actual fixed-point iteration loop.
    %    for i = 1 : maxNumIterations + 1
    i = 1;
    gabba = 1;
    while i <= maxNumIterations + gabba
     
      
      % Project the vector into the space orthogonal to the space
      % spanned by the earlier found basis vectors. Note that we can do
      % the projection with matrix B, since the zero entries do not
      % contribute to the projection.
      w = w - B * B' * w;
      w = w / norm(w);
      
      if notFine
	if i == maxNumIterations + 1
	  
	  round = round - 1;
	  numFailures = numFailures + 1;
	  if numFailures > failureLimit
	    if round == 0
	      A=[];
	      W=[];
	    end
	    return;
	  end
	  % numFailures > failurelimit
	  break;
	end
	% i == maxNumIterations + 1
      else
	% if notFine
	if i >= endFinetuning
	  wOld = w; % So the algorithm will stop on the next test...
	end
      end
      % if notFine
      
       % Test for termination condition. Note that the algorithm has
      % converged if the direction of w and wOld is the same, this
      % is why we test the two cases.
      if norm(w - wOld) < epsilon | norm(w + wOld) < epsilon
        if finetuningEnabled & notFine
          if b_verbose, fprintf('Initial convergence, fine-tuning: '); end;
          notFine = 0;
	  gabba = maxFinetune;
          wOld = zeros(size(w));
          wOld2 = zeros(size(w));
          usedNlinearity = gFine;
          myy = myyK * myyOrig;
	  
	  endFinetuning = maxFinetune + i;
	  
        else
          numFailures = 0;
          % Save the vector
          B(:, round) = w;
	  
          % Calculate the de-whitened vector.
          A(:,round) = dewhiteningMatrix * w;
          % Calculate ICA filter.
          W(round,:) = w' * whiteningMatrix;
          break; % IC ready - next...
        end
        
      end
      
      wOld2 = wOld;
      wOld = w;
    
     switch usedNlinearity
	% pow3
       case 10
	w = (X * ((X' * w) .^ 3)) / numSamples - 3 * w;
       case 11
	EXGpow3 = (X * ((X' * w) .^ 3)) / numSamples;
	Beta = w' * EXGpow3;
	w = w - myy * (EXGpow3 - Beta * w) / (3 - Beta);
       case 12
	Xsub=X(:,getSamples(numSamples, sampleSize));
	w = (Xsub * ((Xsub' * w) .^ 3)) / size(Xsub, 2) - 3 * w;
       case 13
	Xsub=X(:,getSamples(numSamples, sampleSize));
	EXGpow3 = (Xsub * ((Xsub' * w) .^ 3)) / size(Xsub, 2);
	Beta = w' * EXGpow3;
	w = w - myy * (EXGpow3 - Beta * w) / (3 - Beta);
	% tanh
       case 20
	hypTan = tanh(a1 * X' * w);
	w = (X * hypTan - a1 * sum(1 - hypTan .^ 2)' * w) / numSamples;
       case 21
	hypTan = tanh(a1 * X' * w);
	Beta = w' * X * hypTan;
	w = w - myy * ((X * hypTan - Beta * w) / ...
		       (a1 * sum((1-hypTan .^2)') - Beta));
       case 22
	Xsub=X(:,getSamples(numSamples, sampleSize));
	hypTan = tanh(a1 * Xsub' * w);
	w = (Xsub * hypTan - a1 * sum(1 - hypTan .^ 2)' * w) / size(Xsub, 2);
       case 23
	Xsub=X(:,getSamples(numSamples, sampleSize));
	hypTan = tanh(a1 * Xsub' * w);
	Beta = w' * Xsub * hypTan;
	w = w - myy * ((Xsub * hypTan - Beta * w) / ...
		       (a1 * sum((1-hypTan .^2)') - Beta));
	% gauss
       case 30
	% This has been split for performance reasons.
	u = X' * w;
	u2=u.^2;
	ex=exp(-a2 * u2/2);
	gauss =  u.*ex;
	dGauss = (1 - a2 * u2) .*ex;
	w = (X * gauss - sum(dGauss)' * w) / numSamples;
       case 31
	u = X' * w;
	u2=u.^2;
	ex=exp(-a2 * u2/2);
	gauss =  u.*ex;
	dGauss = (1 - a2 * u2) .*ex;
	Beta = w' * X * gauss;
	w = w - myy * ((X * gauss - Beta * w) / ...
		       (sum(dGauss)' - Beta));
       case 32
	Xsub=X(:,getSamples(numSamples, sampleSize));
	u = Xsub' * w;
	u2=u.^2;
	ex=exp(-a2 * u2/2);
	gauss =  u.*ex;
	dGauss = (1 - a2 * u2) .*ex;
	w = (Xsub * gauss - sum(dGauss)' * w) / size(Xsub, 2);
       case 33
	Xsub=X(:,getSamples(numSamples, sampleSize));
	u = Xsub' * w;
	u2=u.^2;
	ex=exp(-a2 * u2/2);
	gauss =  u.*ex;
	dGauss = (1 - a2 * u2) .*ex;
	Beta = w' * Xsub * gauss;
	w = w - myy * ((Xsub * gauss - Beta * w) / ...
		       (sum(dGauss)' - Beta));
	% skew
       case 40
	w = (X * ((X' * w) .^ 2)) / numSamples;
       case 41
	EXGskew = (X * ((X' * w) .^ 2)) / numSamples;
	Beta = w' * EXGskew;
	w = w - myy * (EXGskew - Beta*w)/(-Beta);
       case 42
	Xsub=X(:,getSamples(numSamples, sampleSize));
	w = (Xsub * ((Xsub' * w) .^ 2)) / size(Xsub, 2);
       case 43
	Xsub=X(:,getSamples(numSamples, sampleSize));
	EXGskew = (Xsub * ((Xsub' * w) .^ 2)) / size(Xsub, 2);
	Beta = w' * EXGskew;
	w = w - myy * (EXGskew - Beta*w)/(-Beta);
	
       otherwise
	error('Code for desired nonlinearity not found!');
      end
      
      % Normalize the new w.
      w = w / norm(w);
      i = i + 1;
    end
    round = round + 1;
  end
  
  % Subfunction
% Calculates tanh simplier and faster than Matlab tanh.
function y=tanh(x)
y = 1 - 2 ./ (exp(2 * x) + 1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Samples = getSamples(max, percentage)
Samples = find(rand(1, max) < percentage);
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  
  
  