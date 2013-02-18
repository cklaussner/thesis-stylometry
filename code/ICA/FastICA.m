
%%FastICA

function comp= FastICA(signals, sources) % initialise with data matrix and no. of desired comp. 
%TODO check that no of comp at most equals no of observations

    xwhit = PCA_whit(signals);  % whiten data with pca
    
    wp = rand(size(xwhit,1),sources); % initialse random weights
    
   convergence=0;
 for i=1:size(wp,2)
    
    while (~convergence)
        
        % Calculate new weigth vector
        g = tanh(xwhit'*wp(:,i));
        gp = 1 - g.^2;
        wplus = xwhit*g./size(xwhit,2) - (sum(gp)/size(xwhit,2)).*wp(:,i);
        % Normalize
        wplus = wplus ./ norm(wplus);
        
     
        if i ~= 1  % no need to decorrelate first component
            wtmp=zeros(size(xwhit,1),1);
            for j=1:i-1 % compare with all up to current component
                wtmp=wtmp+wplus'*wp(:,j)*wp(:,j);
            end;
            wpp=wplus-wtmp; % deduct
        
            wplus=wpp/(sqrt(wpp'*wpp)); %normalise
        end  
       
        % Check convergence
       
         convergence=((wp(:,i)'*wplus)>0.9999999999)&&((wp(:,i)'*wplus)<1.000000000001); % convergence if little -no change in weights
       
        wp(:,i)=wplus;
        
    end
    convergence=0;
end

% Estimated independent components


comp = wp' * xwhit;  figure,

% figure
for j=1:sources
subplot(sources,1,j)
plot(1:size(xwhit,2),comp(j,:))
end


    
    