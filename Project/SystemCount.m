for fig=1:2
    clf
end
clear;
d =5:0.1:3000;
f      = 868e6;
l      = 3e8/f;
Gt     = 2; % dBi
Gr     = 13;  % dBi
PrvsPt = db2pow(Gt+Gr).*l.^2./(4.*pi.*d).^2;
Lrt = -pow2db(PrvsPt);
hb     = 40; %m
f=f/1e6;
highlevel=repmat(142,1,length(d));
highlevel1=repmat(154,1,length(d));
%closed=Lsu+Lrt;
%open=Lo+Lrt;

hcl=1:1:15;
hop=1:1:15;
figure(1);
hold on;grid on;
for i=1:1:length(hcl)
    hm=hcl(i);
    CH=0.8+(1.1.*log10(f)-0.7).*hm-1.56.*log10(f);
    Lu=69.55 + 26.16.*log10(f) - 13.82.*log10(hb) - CH + (44.9-6.55.*log10(hb)).*log10(d./1000);
    Lsu=Lu-2.*(log10(f./28)).^2-5.4;
    Lo = Lu-4.78.*(log10(f)).^2+18.33.*log10(f)-40.94;
    closed=Lsu+Lrt;
    plot(d,closed,'LineWidth',1)
    cllab(i)={strcat('Hm = ',num2str(hm))};
end
plot(d,highlevel,'b-')
plot(d,highlevel1,'r-')
cllab(i+1)={'limit min'};
cllab(i+2)={'limit max'};
lgc=legend(cllab);
lgc.NumColumns=3;

title('плотная городская застройка')
yl = ylabel('L, дБ');
yl.Rotation=0;
yl.Position=[70 180];

xl=xlabel('d, м');
xl.Position = [1000 20];
axis([0 1000 30 180]);

clear cllab; clear hm;
figure(2)
hold on;grid on;
for i=1:1:length(hop)
    hm=hop(i);
    CH=0.8+(1.1.*log10(f)-0.7).*hm-1.56.*log10(f);
    Lu=69.55 + 26.16.*log10(f) - 13.82.*log10(hb) - CH + (44.9-6.55.*log10(hb)).*log10(d./1000);
    Lsu=Lu-2.*(log10(f./28)).^2-5.4;
    Lo = Lu-4.78.*(log10(f)).^2+18.33.*log10(f)-40.94;
    open=Lo+Lrt;
    plot(d,open,'LineWidth',1)
    cllab(i)={strcat('Hm = ',num2str(hm))};
end
plot(d,highlevel,'b-')
plot(d,highlevel1,'r-')
axis([0 inf 10 inf]);
cllab(i+1)={'limit min'};
cllab(i+2)={'limit max'};
lgo=legend(cllab);
lgo.NumColumns=3;


title('пригород, сельская местность')
yl = ylabel('L, дБ');
yl.Rotation=0;
yl.Position=[70 180];

xl=xlabel('d, м');
xl.Position = [2500 20];
axis([0 2500 30 180]);