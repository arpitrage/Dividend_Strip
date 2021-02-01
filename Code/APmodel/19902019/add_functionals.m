function M = add_functionals(M1,M2)

M = M1;
M.name = strcat(M1.name,' ',M2.name);
M.GAMMA0 = M1.GAMMA0 + M2.GAMMA0;
M.GAMMA1 = M1.GAMMA1 + M2.GAMMA1;
M.GAMMA3 = M1.GAMMA3 + M2.GAMMA3;
M.PSI0 = M1.PSI0 + M2.PSI0;
M.PSI1 = M1.PSI1 + M2.PSI1;

end