%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROPER REPEATED MEASURES ANOVA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dataset = [pos_fam_pup; pos_nov_pup; impos_fam_pup; impos_nov_pup];

fd_dataset = data2fd(dataset', 1:600, basis);

subject_var = [(1:num_participants)'; (1:num_participants)'; (1:num_participants)'; (1:num_participants)'];
perception_FACTOR = [zeros(num_participants,1); ones(num_participants,1); zeros(num_participants,1); ones(num_participants,1)] + 1;
conceptual_FACTOR = [zeros(num_participants*2,1); ones(num_participants*2,1)] + 1;

df_num = 1; % design is 2 x 2
df_den = num_participants - 1; % again, design is 2 x 2
crit_F_val = finv(.95,df_num,df_den);

% create index data array

for i = 1:num_participants*4
    index_array(subject_var(i),perception_FACTOR(i),conceptual_FACTOR(i)) = i;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% INTERACTION %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% now compute SS_AxB

SS_AxB = zeros(1,600);

for j = 1:2
    for k = 1:2
        fd_foo = mean(fd_dataset(index_array(:,j,k)))-...
            mean(fd_dataset(reshape(index_array(:,j,:),num_participants*2,1)))-...
            mean(fd_dataset(reshape(index_array(:,:,k),num_participants*2,1)))+...
            mean(fd_dataset);
        foo = eval_fd(fd_foo,1:600);
        SS_AxB = SS_AxB + (foo').^2;
    end
end

SS_AxB = SS_AxB * num_participants;

MS_AxB = SS_AxB./df_num;

% now compute SS_AxBxS, the error term

SS_AxBxS = zeros(1,600);

for j = 1:2
    for k = 1:2
        for i = 1:num_participants
            fd_foo = fd_dataset(index_array(i,j,k))-...
                mean(fd_dataset(reshape(index_array(i,j,:),2,1)))-...
                mean(fd_dataset(reshape(index_array(i,:,k),2,1)))-...
                mean(fd_dataset(index_array(:,j,k)))+...
                mean(fd_dataset(reshape(index_array(i,:,:),4,1)))+...
                mean(fd_dataset(reshape(index_array(:,j,:),num_participants*2,1)))+...
                mean(fd_dataset(reshape(index_array(:,:,k),num_participants*2,1)))-...
                mean(fd_dataset);
            foo = eval_fd(fd_foo,1:600);
            SS_AxBxS = SS_AxBxS + (foo').^2;
        end
    end
end

MS_AxBxS = SS_AxBxS./df_den;

F_ratio_AxB = MS_AxB./MS_AxBxS;

% nice plot

figure;
plot(F_ratio_AxB,'Color', 'k', 'LineWidth',3);
line([1 600],[crit_F_val,crit_F_val],'Color','k','Linestyle','--');
axis([0,600,0,15]);
line([50 50],[6,-0.5],'Color', 'k','Linestyle',':'); % screen starts rotating
line([200 200],[6,-0.5],'Color', 'k','Linestyle',':'); % screen stops rotating away
line([250 250],[6,-0.5],'Color', 'k','Linestyle',':'); % screen starts rotating back
line([400 400],[6,-0.5],'Color', 'k','Linestyle',':'); % exits left tunnel... in gap (or not) for 20 frames (40 samples)
set(gca,'XTick',0:200:600);
set(gca,'XTickLabel',0:2000:12000);
xlabel('Time (ms)','FontSize',14);
set(gca,'Box','off', 'FontSize',12);
ylabel('F test value for interaction','FontSize',14);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% PERCEPTION %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% now compute SS_A

SS_A = zeros(1,600);

for j = 1:2
    fd_foo = mean(fd_dataset(reshape(index_array(:,j,:),num_participants*2,1)))-...
        mean(fd_dataset);
    foo = eval_fd(fd_foo,1:600);
    SS_A = SS_A + (foo').^2;
end

SS_A = SS_A * num_participants;

MS_A = SS_A./df_num;

% now compute SS_AxS, the error term

SS_AxS = zeros(1,600);

for j = 1:2
    for i = 1:num_participants
        fd_foo = mean(fd_dataset(reshape(index_array(i,j,:),2,1)))-...
            mean(fd_dataset(reshape(index_array(i,:,:),4,1)))-...
            mean(fd_dataset(reshape(index_array(:,j,:),num_participants*2,1)))+...
            mean(fd_dataset);
        foo = eval_fd(fd_foo,1:600);
        SS_AxS = SS_AxS + (foo').^2;
    end
end

MS_AxS = SS_AxS./df_den;

F_ratio_A = MS_A./MS_AxS;

% nice plot

figure;
plot(F_ratio_A,'Color', 'k', 'LineWidth',3);
line([1 600],[crit_F_val,crit_F_val],'Color','k','Linestyle','--');
axis([0,600,0,15]);
line([50 50],[6,-0.5],'Color', 'k','Linestyle',':'); % screen starts rotating
line([200 200],[6,-0.5],'Color', 'k','Linestyle',':'); % screen stops rotating away
line([250 250],[6,-0.5],'Color', 'k','Linestyle',':'); % screen starts rotating back
line([400 400],[6,-0.5],'Color', 'k','Linestyle',':'); % exits left tunnel... in gap (or not) for 20 frames (40 samples)
set(gca,'XTick',0:200:600);
set(gca,'XTickLabel',0:2000:12000);
xlabel('Time (ms)','FontSize',14);
set(gca,'Box','off', 'FontSize',12);
ylabel('F test value for angle','FontSize',14);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% COGNITION %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% now compute SS_B

SS_B = zeros(1,600);

for k = 1:2
    fd_foo = mean(fd_dataset(reshape(index_array(:,:,k),num_participants*2,1)))-...
        mean(fd_dataset);
    foo = eval_fd(fd_foo,1:600);
    SS_B = SS_B + (foo').^2;
end

SS_B = SS_B * num_participants;

MS_B = SS_B./df_num;

% now compute SS_BxS, the error term

SS_BxS = zeros(1,600);

for k = 1:2
    for i = 1:num_participants
        fd_foo = mean(fd_dataset(reshape(index_array(i,:,k),2,1)))-...
            mean(fd_dataset(reshape(index_array(i,:,:),4,1)))-...
            mean(fd_dataset(reshape(index_array(:,:,k),num_participants*2,1)))+...
            mean(fd_dataset);
        foo = eval_fd(fd_foo,1:600);
        SS_BxS = SS_BxS + (foo').^2;
    end
end

MS_BxS = SS_BxS./df_den;

F_ratio_B = MS_B./MS_BxS;

% nice plot

figure;
plot(F_ratio_B,'Color', 'k', 'LineWidth',3);
line([1 600],[crit_F_val,crit_F_val],'Color','k','Linestyle','--');
axis([0,600,0,15]);
line([50 50],[6,-0.5],'Color', 'k','Linestyle',':'); % screen starts rotating
line([200 200],[6,-0.5],'Color', 'k','Linestyle',':'); % screen stops rotating away
line([250 250],[6,-0.5],'Color', 'k','Linestyle',':'); % screen starts rotating back
line([400 400],[6,-0.5],'Color', 'k','Linestyle',':'); % exits left tunnel... in gap (or not) for 20 frames (40 samples)
set(gca,'XTick',0:200:600);
set(gca,'XTickLabel',0:2000:12000);
xlabel('Time (ms)','FontSize',14);
set(gca,'Box','off', 'FontSize',12);
ylabel('F test value for box','FontSize',14);
