addpath("/home/georgia/afib/sample2017/validation");
addpath("/home/georgia/afib/mcode")
fid = fopen(['RECORDS'],'r');


RECLIST = textscan(fid,'%s');


fclose(fid);
RECORDS = RECLIST{1};


for i = 1:length(RECORDS)
    fname = RECORDS{i};
    [tm,ecg,fs,siginfo]=rdmat(fname);
    mat2wfdb(ecg, fname, fs,16, 'mV',siginfo);
end
