function R = nancorr(data1,data2)
    in = ~isnan(data1+data2);
    X = data1(in);
    Y = data2(in);
    R = corr(X,Y);
