N_sim = 1000;
sim_results = nan(N_sim,1);

for i=1:N_sim
    N=32000;

    lsr = normrnd(0,1,N,1);
    noise1 = normrnd(0,1,N,1);
    noise2 = normrnd(0,1,N,1);

    ma = noise1+lsr/(tan(acos(0.2)));
    sa = noise2+lsr/(tan(acos(0.2)));
    foo = fit(lsr,ma,'poly1');
    ma_lsr = ma-foo(lsr);

    T = compareCorrelationsT(corr(ma,sa),corr(ma_lsr,sa),corr(ma,ma_lsr),N);
    sim_results(i)=abs(T)>abs(tinv(0.025,N-3));
end
