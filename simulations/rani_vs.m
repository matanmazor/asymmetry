mu=[1 0]; % target / distractor
sz_grid=1:7; %actually num distractors
sd_Q= 1;
sd_O= .5;
nsims=1e6;
beta=1;
threshold= 1.25;

for sz=sz_grid
    for cc=1:2 % find Q or O
       
        std_vec= [sd_Q, sd_O];
        if cc==2
            std_vec= fliplr(std_vec);
        end
        for nn=1:nsims
            x= [mu(1)+std_vec(1)*randn mu(2)+std_vec(2)*randn(1,sz)]; % target then distractors: "strength"
            % but the only thing that matters is whether strength exceeds a
            % threshold
            % we first seardh above threhsold items in a random order. If target not
            % found we continue to search below threshold items in a random order
            Items_above_Thresh= find(x> threshold);
            if ismember(1, Items_above_Thresh)
                pos(sz,cc,nn)= randperm(length(Items_above_Thresh),1);
            else
                pos(sz,cc,nn)= length(Items_above_Thresh)+ randperm(sz+1-length(Items_above_Thresh),1);
            end
        end
    end
end
% pos= mean(pos,3);
% pos_diff= pos(:,2)-pos(:,1); % search O - search Q
% plot(sz_grid, pos_diff);