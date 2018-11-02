
t_range = 6000;

% plotTitle = "Nine Identical Cells";
% Yc = Yc_no_alter;

% plotTitle = "Random Sodium Conductance, No Gap";
% Yc = Yc_no_gap;

% plotTitle = "Random Sodium Conductance, With Gap";
% Yc = Yc_gap;

plotTitle = "One Spontaneous Cell";
Yc = Yc_k1_changed;


plot(t(1:t_range), Yc(1:t_range,1),... 
     t(1:t_range), Yc(1:t_range,19),...
     t(1:t_range), Yc(1:t_range,37),...
     t(1:t_range), Yc(1:t_range,55),...
     t(1:t_range), Yc(1:t_range,73),...
     t(1:t_range), Yc(1:t_range,91),...
     t(1:t_range), Yc(1:t_range,109),...
     t(1:t_range), Yc(1:t_range,127),...
     t(1:t_range), Yc(1:t_range,145));

% Get number of maxima

 
 
title(plotTitle);
xlabel("Time (s)");
ylabel("Voltage (V)");

ax = gca;
ax.FontSize = 20; 
ax.TitleFontSizeMultiplier = 2;

legend({"1", "2", "3", "4", "5", "6", "7", "8", "9"},"FontSize", 18)
 

 