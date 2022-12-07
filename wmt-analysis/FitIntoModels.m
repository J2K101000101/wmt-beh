% % % % MemToolbox setu p%% Download the package and then set it up if you haven't
% % % Setup

%% pre-post 
clear all
cd /Users/JSK/Desktop/R/vwmt
default = pwd;% *you need to go back to default wd (e.g.,/Users/JSK/Desktop/R/vwmt/)!*my default working directory to run this section
outputFolder = [default, '/','output'];
cd /Users/JSK/Desktop/R/vwmt/cleanedPrePost % change to directory that contains your file
model = StandardMixtureModel();% we don't need Orientation(WithBias(StandardMixtureModel) here because the orientations in this current study is ranged from [-180,180] 
type = 1; 

% ort_post
File = dir('ort_post.csv');
getParameters (File,outputFolder,model,type);
% ort_pre
File = dir('ort_pre.csv');
getParameters (File,outputFolder,model,type);
% srt_post
File = dir('srt_post.csv');
getParameters (File,outputFolder,model,type);
% srt_pre
File = dir('srt_pre.csv');
getParameters (File,outputFolder,model,type);


%%
%%trainings for ort %%
cd /Users/JSK/Desktop/R/vwmt
clear all
default = pwd;% my default working directory (/Users/JSK/Desktop/R/vwmt/) to run this section
outputFolder = [default, '/','output'];
cd /Users/JSK/Desktop/R/vwmt/cleanedTraining % change to directory that contains your file
model = StandardMixtureModel();
type = 2; 

% t1_ss2
File = dir('ortT_t1_ss2.csv');
getParameters (File,outputFolder,model,type);

% t1_ss4
File = dir('ortT_t1_ss4.csv');
getParameters (File,outputFolder,model,type);

% t1_ss6
File = dir('ortT_t1_ss6.csv');
getParameters (File,outputFolder,model,type);

% t2_ss2
File = dir('ortT_t2_ss2.csv');
getParameters (File,outputFolder,model,type);

% t2_ss4
File = dir('ortT_t2_ss4.csv');
getParameters (File,outputFolder,model,type);

% t2_ss6
File = dir('ortT_t2_ss6.csv');
getParameters (File,outputFolder,model,type);

% t3_ss2
File = dir('ortT_t3_ss2.csv');
getParameters (File,outputFolder,model,type);

% t3_ss4
File = dir('ortT_t3_ss4.csv');
getParameters (File,outputFolder,model,type);

% t3_ss6
File = dir('ortT_t3_ss6.csv');
getParameters (File,outputFolder,model,type);

% t4_ss2
File = dir('ortT_t4_ss2.csv');
getParameters (File,outputFolder,model,type);

% t4_ss4
File = dir('ortT_t4_ss4.csv');
getParameters (File,outputFolder,model,type);

% t4_ss6
File = dir('ortT_t4_ss6.csv');
getParameters (File,outputFolder,model,type);

