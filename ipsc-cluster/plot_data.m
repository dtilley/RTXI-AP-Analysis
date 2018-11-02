




% For finding maxima / minima over time interval

t_range = 20000;
% [maxima, maxIdx] = findpeaks(Vm(1:t_range));
% length(maxima)

% plot(t(1:t_range), Vm(1:t_range));

%  Plot g_k1 against peak frequency
g_k1 = 19.1925;
conductance = [0, .5, 1, 2, 3, 3.5 ,4];
time_range = 20;
peaks = [35, 22 ,17, 15, 12, 7, 1];
frequency = peaks / time_range;

plot(conductance, frequency, "LineWidth", 4);
hold on;
scatter(conductance, frequency, 100, "filled", "MarkerFaceAlpha", .5);
title("AP Frequency Vs K1 Conductance");
xlabel("Conductance (times g_K1)");
ylabel("Frequency (1/s)");

ax = gca;
ax.FontSize = 20; 
ax.TitleFontSizeMultiplier = 2;
ax.LineWidth = 3;
