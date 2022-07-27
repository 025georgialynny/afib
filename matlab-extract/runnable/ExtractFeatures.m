function ExtractFeatures(dbpath,varargin)
% This function extracts features for each record present  in a folder
%
%  Input:
%       - dbpath:         directory where database is
%       (optional inputs)
%           - useSegments:       segment signals into windows (bool)?
%           - windowSize:        size of window used in segmenting record
%           - percentageOverlap: overlap between windows
%
% --
% ECG classification from single-lead segments using Deep Convolutional Neural 
% Networks and Feature-Based Approaches - December 2017
% 
% Released under the GNU General Public License
%
% Copyright (C) 2017  Fernando Andreotti, Oliver Carr
% University of Oxford, Insitute of Biomedical Engineering, CIBIM Lab - Oxford 2017
% fernando.andreotti@eng.ox.ac.uk
%
% 
% For more information visit: https://github.com/fernandoandreotti/cinc-challenge2017
% 
% Referencing this work
%
% Andreotti, F., Carr, O., Pimentel, M.A.F., Mahdi, A., & De Vos, M. (2017). 
% Comparing Feature Based Classifiers and Convolutional Neural Networks to Detect 
% Arrhythmia from Short Segments of ECG. In Computing in Cardiology. Rennes (France).
%
% Last updated : December 2017
% 
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.


% Default arguments
optargs = {1 10 0.8};  % default values for input arguments
newVals = cellfun(@(x) ~isempty(x), varargin);
optargs(newVals) = varargin(newVals);
[useSegments, windowSize, percentageOverlap] = optargs{:};
clear optargs newVals
slashchar = char('/'*isunix + '\'*(~isunix));


fs = 300;       % sampling frequency [Hz]

% Add subfunctions to matlab path
mainpath = (strrep(which(mfilename),['preparation' slashchar mfilename '.m'],''));
addpath(genpath([mainpath(1:end-length(mfilename)-2) 'subfunctions' slashchar])) % add subfunctions folder to path
addpath('training2017')
addpath('subsets')
addpath('rpeaks')
addpath('mcode')
% Find recordings
ref_filename = ['subset_list.csv'];
reference_tab = readtable(ref_filename,'ReadVariableNames',false);
fls = reference_tab{:,2};
clear dataArray delimiter ref_filename formatSpec fileID

%% Initialize loop
% Wide BP
Fhigh = 5;  % highpass frequency [Hz]
Flow = 45;   % low pass frequency [Hz]
Nbut = 10;     % order of Butterworth filter
d_bp= design(fdesign.bandpass('N,F3dB1,F3dB2',Nbut,Fhigh,Flow,fs),'butter');
[b_bp,a_bp] = tf(d_bp);

% Narrow BP
Fhigh = 1;  % highpass frequency [Hz]
Flow = 100;   % low pass frequency [Hz]
Nbut = 10;     % order of Butterworth filter
d_bp= design(fdesign.bandpass('N,F3dB1,F3dB2',Nbut,Fhigh,Flow,fs),'butter');
[b_bp2,a_bp2] = tf(d_bp);
clear Fhigh Flow Nbut d_bp
wb = waitbar(0, 'Starting');
%% Run through files
dcp = inline ( 'round(input.*10.^number)./10.^number' );

for f = 2:length(fls)
    %% Loading data
     waitbar(f/length(fls), wb, sprintf('Progress: %d out of %d \n %g %%', f, length(fls), dcp(f/length(fls)*100, 2)));

    samp_filename = ['subsets/' fls{f} '.csv'];
    data = readtable(samp_filename,'ReadVariableNames',false);
    fname = fls{f};
    signal = data.Var2;
    if size(signal,1)<size(signal,2), signal = signal'; end % make sure it's column vector
    signalraw =  signal;
    
    %% Preprocessing
    signal = filtfilt(b_bp,a_bp,signal);             % filtering narrow
    signal = detrend(signal);                        % detrending (optional)
    signal = signal - mean(signal);
    signal = signal/std(signal);                     % standardizing
    signalraw = filtfilt(b_bp2,a_bp2,signalraw);     % filtering wide
    signalraw = detrend(signalraw);                  % detrending (optional)
    signalraw = signalraw - mean(signalraw);
    signalraw = signalraw/std(signalraw);        % standardizing

    
    WINSIZE = length(signal);
    startp = 1;
    endp = WINSIZE;
    nseg = 1;
    startp(nseg) = 0 + round((1)*WINSIZE);
    endp(nseg) = length(signal);
    
    %% Extract RR ints
    n=1;
    if true
        % Get signal of interest
        sig_seg = signal;
        sig_segraw = signalraw;
        % QRS detect
        [qrsseg,featqrs] = multi_qrsdetect(sig_seg,fs,[fname '_s' num2str(n)]);
        rpeaks = qrsseg{end};
    end       
    save(['rpeaks/' fname '.mat'],'rpeaks');
end
delete('gqrsdet*.*')


