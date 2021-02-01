function p = shock_elasticity_affine(p)
% input: X_t+1 = THETA11 * X_t + LAMBDA10 * W_t+1
%        Y_t+1 - Y_t = GAMMA0 + GAMMA1*X_t + X_t'*GAMMA3*X_t + ...
%                      PSI0*W_t+1 + X_t'*PSI1*W_t+1
%
% output: shock_elasticity in the form p.elast_0 + p.elast_x * X

n = size(p.THETA11,1);
k = size(p.LAMBDA10,2);

GAMMA0HAT = 0;
GAMMA1HAT = zeros(1,n);
GAMMA3HAT = zeros(n,n);

p.elast_0 = zeros(k,1,p.T);
p.elast_x = zeros(k,n,p.T);

for t = 1 : p.T
    GAMMA0TILDE = GAMMA0HAT + p.GAMMA0;
    GAMMA1TILDE = GAMMA1HAT*p.THETA11 + p.GAMMA1;
    GAMMA3TILDE = p.THETA11'*GAMMA3HAT*p.THETA11 + p.GAMMA3;
    PSI0TILDE = GAMMA1HAT*p.LAMBDA10 + p.PSI0;
    PSI1TILDE = 2*p.THETA11'*GAMMA3HAT*p.LAMBDA10 + p.PSI1;
    PSI2TILDE = p.LAMBDA10'*GAMMA3HAT*p.LAMBDA10;
    
    p.elast_0(:,:,t) = (eye(k)-2*PSI2TILDE)\PSI0TILDE';
    p.elast_x(:,:,t) = (eye(k)-2*PSI2TILDE)\PSI1TILDE';
    
    GAMMA0HAT = GAMMA0TILDE - 1/2*log(det(eye(k)-2*PSI2TILDE)) ...
        + 1/2*PSI0TILDE/(eye(k)-2*PSI2TILDE)*PSI0TILDE';
    GAMMA1HAT = GAMMA1TILDE + PSI0TILDE/(eye(k)-2*PSI2TILDE)*PSI1TILDE';
    GAMMA3HAT = GAMMA3TILDE + 1/2*PSI1TILDE/(eye(k)-2*PSI2TILDE)*PSI1TILDE';
end



end