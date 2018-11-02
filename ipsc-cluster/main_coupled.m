options = odeset('MaxStep',1e-3,'InitialStep',2e-5);
Y=[-0.070 0.3 0.0002 1 0 1 1 1 0 1 0 0.75 0.75 0 0.1 1 0 14.1 -0.070 0.3 0.0002 1 0 1 1 1 0 1 0 0.75 0.75 0 0.1 1 0 14.1];

[T,Yc] = ode15s(@Compute_Coupled_Voltage,[0 80],Y, options);
t = T;
Vm_1=Yc(:,1);
Vm_2=Yc(:,19);
% dVm = [0; diff(Vm)./diff(t)];
% caSR = Yc(:,2);
% Cai = Yc(:,3);
% Nai = Yc(:,18);
