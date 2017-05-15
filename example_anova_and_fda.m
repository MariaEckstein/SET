%%
% let's look at clean data
load('data clean.mat'); % load data in workspace
data(8) = [];  % this participant is a dud, unfortunately, and needs to be removed
num_parts = max(size(data)); % how many participants?


% plot data for a particular trial, for illustration
% let us plot the raw data for the left pupil on the last (hard) problem
figure; % open a figure
foo = max(size(data(1).trial_data(6).CleanPupil)); % how many samples?
scatter(1:600, data(1).trial_data(6).CleanPupil(foo-599:foo), 4, 'k', 'filled') % plot the last 600 (or 10s given 60Hz sampling)
title('Hard problem 3', 'FontSize', 16); % title
set(gca, 'XTick', 0:120:600, 'XTickLabel', 0:2000:10000); % ticks along x axis
hold on; % we will add to plot, rather than replace with new data
for j = 2:num_parts % repeat for remaining participants
    foo = max(size(data(j).trial_data(6).CleanPupil)); % how many samples
    scatter(1:600, data(j).trial_data(6).CleanPupil(foo-599:foo), 4, 'k', 'filled') % plot last 600
end
hold off; % done adding data
xlim([1 600]); % set axis limits
ylim([0 5]); % axis limits
xlabel('Time after problem onset (ms)', 'FontSize', 16); % axis label
ylabel('Pupil diameter (mm)', 'FontSize', 16); % axis label
set(gca, 'FontSize', 14); % axis label

%% Write data as csv so I can read it in with R
all_data = [];
for i = 1:num_parts
    foo = max(size(data(i).trial_data(6).CleanPupil)); % how many samples
    all_data(i,:) = data(i).trial_data(6).CleanPupil(foo-599:foo);
end
csvwrite('fda_example_data.csv', all_data);


%%
% let's remove a baseline

baseline_samples = 3; % 50 millisecond baseline

for i = 1:6 % go through all trials
    for j = 1:num_parts % go through participants
        foo = max(size(data(j).trial_data(i).CleanPupil)); % how many samples, including fixation?
        problem_data(j,i,:) = data(j).trial_data(i).CleanPupil(foo-599:foo) - mean(data(j).trial_data(i).CleanPupil(foo-599 - baseline_samples:foo-600)); % keep last 10secs and remove baseline
    end
end

%%
% show means by problem type
figure;
plot(1:600, mean([squeeze(problem_data(:,1,:)); squeeze(problem_data(:,3,:)); squeeze(problem_data(:,5,:))]), 'k', 'LineWidth', 4);    
hold on;
plot(1:600, mean([squeeze(problem_data(:,2,:)); squeeze(problem_data(:,4,:)); squeeze(problem_data(:,6,:))]), 'b', 'LineWidth', 4);    
hold off;
ylim([-0.4 0.2]);
xlim([1 600]);
legend('Easy', 'Hard', 'FontSize', 18);
set(gca,'Box', 'off', 'FontSize', 16);
xlabel('Time after problem onset (ms)','FontSize', 18);
ylabel('Average change in pupil diameter (mm)', 'FontSize', 18);
set(gca, 'XTick', 0:60:600, 'XTickLabel', 0:1000:10000);

%%
% extract data for ANOVA
slots = 1:120:600;
for i = 1:num_parts
    mean_easy = mean([squeeze(problem_data(i,1,:))'; squeeze(problem_data(i,3,:))'; squeeze(problem_data(i,5,:))']);
    mean_hard = mean([squeeze(problem_data(i,2,:))'; squeeze(problem_data(i,4,:))'; squeeze(problem_data(i,6,:))']);
    for j = 2:2:10
        ANOVA_data(i,j-1) = median(mean_easy(slots(j/2):slots(j/2)+6));
        ANOVA_data(i,j)   = median(mean_hard(slots(j/2):slots(j/2)+6));
    end
end

figure;
bar_data = reshape(mean(ANOVA_data), 2, 5)';
h = bar(0:2000:8000, bar_data);
box off;
set(gca,'FontSize', 14);
xlabel('Time from stimulus onset (ms)', 'FontSize', 18);
ylabel('Average pupil diameter change from baseline', 'FontSize', 18);
legend('Easy', 'Hard', 'FontSize',18);

%%
%%%%%%%% fda now
addpath(genpath('C:\Users\maria\MEGAsync\Berkeley\R scripts\sequentialset'));

for i = 1:num_parts % for each participant, create mean data for easy and hard, separately
    fda_mean_easy(i,:) = mean([squeeze(problem_data(i,1,:))'; squeeze(problem_data(i,3,:))'; squeeze(problem_data(i,5,:))']);
    fda_mean_hard(i,:) = mean([squeeze(problem_data(i,2,:))'; squeeze(problem_data(i,4,:))'; squeeze(problem_data(i,6,:))']); 
end

differences = fda_mean_hard - fda_mean_easy; % we'll do a t test comparing easy and hard... so the focus is on the difference between each, for each participant

basis = create_bspline_basis([1 600], 10, 4); % b-spline basis object

data_mat = [];
mean_raw = [];
for i = 1:num_parts
    fd_data_1_1   = data2fd(differences(i,:), 1:600, basis); % transform data into b-splines
    data_mat(i,:) = eval_fd(fd_data_1_1, 1:600)'; % data in raw form
%     mean_fd       = mean(fd_data_1_1); % mean difference as b-spline
end

figure; % let's plot!
plot(mean(data_mat));
h = findobj(gca, 'Type', 'line');
set(h(1), 'Color', 'k');
set(gca, 'XTick', 0:60:600);
set(gca, 'XTickLabel', 0:1000:(600/60)*1000);
xlabel('Time (ms)', 'FontSize', 18);
ylabel('Mean pupil diameter difference (hard - easy) in mm', 'FontSize', 18);
set(gca, 'Box', 'off', 'FontSize', 14);


%%%%%%%%%%%%%%%%%%%%% test t

data_mat_squared = data_mat .^ 2; % squared data

S_D = sqrt((sum(data_mat_squared) - ((sum(data_mat) .^ 2) ./ num_parts)) ./ (num_parts - 1)); % standard deviation of the difference
SE = S_D ./ sqrt(num_parts); % standard error of the difference

mean_raw = mean(data_mat); % mean difference in raw form

t_fd = (mean_raw ./ SE); % t ratio

crit_t_val = 2.45; %(n = 7) % critical value

figure; % plot!
plot(1:600, t_fd, 'Color', 'k', 'LineWidth', 4);
line([1 600], [ crit_t_val,  crit_t_val], 'Color', 'k');
line([1 600], [-crit_t_val, -crit_t_val], 'Color', 'k');
set(gca, 'XTick', 0:60:600);
set(gca, 'XTickLabel', 0:1000:(600/60)*1000);
xlabel('Time (ms)', 'FontSize', 18);
ylabel('t-test value', 'FontSize', 18);
xlim([1, 600])
ylim([-2, 5])
set(gca, 'Box', 'off', 'FontSize', 14);

