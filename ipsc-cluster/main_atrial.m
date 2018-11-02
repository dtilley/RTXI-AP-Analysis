options = odeset('MaxStep',1e-3,'InitialStep',2e-5);
Y=[
    -0.070
    0.3
    0.0002
    1
    0
    1
    1
    1
    0
    1
    0
    0.75
    0.75
    0
    0.1
    1
    0
    14.1
    ];

%[T,Y] = ode15s(@rigid,[0 12],[0, 1, 1]);
[T,Yc] = ode15s(@Atrial_SI_working,[0 20],Y', options);
t = T;
Vm=Yc(:,1);
dVm = [0; diff(Vm)./diff(t)];
caSR = Yc(:,2);
Cai = Yc(:,3);
Nai = Yc(:,18);

% This iterates over all inputs and returns additional information
% for i= 1:size(Yc,1)
%     [temp dati] =  Atrial_SI_working(t(i), Yc(i,:));
%     Ik1(i) = dati(1);
%     Ito(i) = dati(2);
%     Ikr(i) = dati(3);
%     Iks(i) = dati(4);
%     ica(i) = dati(5);
%     INaK(i) = dati(6);
%     INa(i) = dati(7);
%     inaca(i) = dati(8);
%     IpCa(i) = dati(9);
%     If(i) = dati(10);
%     IbNa(i) = dati(11);
%     IbCa(i) = dati(12);
%     Irel(i) = dati(13);
%     Iup(i) = dati(14);
%     Ileak(i) = dati(15); 
%     Istim(i) = dati(16);
%     E_K(i) = dati(17);
%     E_Na(i) = dati(18);
% end






