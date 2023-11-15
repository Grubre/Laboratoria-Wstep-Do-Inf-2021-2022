# Jakub Ogrodowczyk
function logistic_growth(p0, r, n)
    pn = p0
    for i in 1:n
        new = r*pn*(1 - pn)
        pn = pn + new
    end
    return pn
end

p0 = 0.01
r = 3
logistic_growth(p0, r, 40)