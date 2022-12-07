function [datasets] = datastruct(File,type)
% data struct array for MemFit

 if type == 1 % prepost with fixed setsize
         results = readtable(File.name,'HeaderLines',1,'ReadRowNames',true);
         No = extractBetween(File.name,'rt_','.csv');
         setsize = 4;
         rnames = results.Properties.RowNames;
         results = table2array(results);
         for i=1:size(results,2)
           datasets{1, i}.errors = results(i,:);
           datasets{1, i}.n = repelem (setsize,size(results,1));
           datasets{1, i}.subjectID = rnames{i};
           datasets{1, i}.session = No{1};
            end
 elseif type == 2 % traingings with multiple setsizes
         results = readtable(File.name,'HeaderLines',1,'ReadRowNames',true);
         No = extractBetween(File.name,'ortT_','_');
         setsize = extractBetween(File.name,'_ss','.csv');
         rnames = results.Properties.RowNames;
         IDs = cellfun(@(rnames)rnames(1:end-14),cellstr(rnames),'uni',0);
         results = table2array(results);
        for i=1:size(results,2)
           datasets{1, i}.errors = results(i,:);
           datasets{1, i}.n = repelem (setsize,size(results,1));
           datasets{1, i}.subjectID = IDs{i};
           datasets{1, i}.session = No{1};
        end
     else
       'type =1 for prepost; type =2 for trianings'
     end 
end

