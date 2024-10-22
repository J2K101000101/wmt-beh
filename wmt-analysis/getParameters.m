function [Capacity,Precision] = getParameters(File,outputFolder,model,type)
%  type= 1 for prepost; type = 2 for trainings
    if type == 1
        setSize = 4; % a fixed setSize can be changed when necessary
    elseif type == 2
        setSize = str2double(extractBetween(File.name,'_ss','.csv'));
    else
        ' type= 1 for prepost (fixed setSize =4 ); type = 2 for trainings (setSize according to file names) '
    end
    
    fileName = extractBetween(File.name,'','.csv');
    capName = ['capacity_',fileName{1}];
    preName = ['precision_',fileName{1}];
    capDir = [outputFolder,'/',capName,'.csv'];
    preDir = [outputFolder,'/',preName,'.csv'];

datasets=datastructing(File,type);

%FitMultipleSubjects_MLE
parameters = FitMultipleSubjects_MLE(datasets, model);
g = parameters.paramsSubs(:,1);
sd = parameters.paramsSubs(:,2);
for i = 1: length(parameters.paramsSubs)
capacity(i) = (1-g(i))*setSize;
precision(i) = inv(sd(i));
rnames{i} = datasets{1, i}.subjectID;
end
Capacity = table(capacity','RowNames',rnames, 'VariableNames', {capName });
writetable(Capacity, capDir,'WriteRowNames', true, 'WriteVariableNames', true) ;
Precision =table(precision','RowNames',rnames, 'VariableNames', {preName });
writetable(Precision, preDir,'WriteRowNames', true, 'WriteVariableNames', true) ;
end

