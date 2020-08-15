function [ T ] = compareCorrelationsT(jk, jh, kh, n)

R = (1 - (jk^2) - (jh^2) - (kh^2)) + (2*jk*jh*kh);
r = 0.5*(jk+jh);
T = (jk - jh) * sqrt(((n-1)*(1+kh))/(2*((n-1)/(n-3))*R + (r^2)*((1-kh)^3)));

end