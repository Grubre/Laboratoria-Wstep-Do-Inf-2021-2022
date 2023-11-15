# Jakub Ogrodowczyk
using Plots

coeffs = [1, -210.0, 20615.0, -1256850.0,
          53327946.0, -1672280820.0, 40171771630.0, -756111184500.0,
          11310276995381.0, -135585182899530.0,
          1307535010540395.0, -10142299865511450.0,
          63030812099294896.0, -311333643161390640.0,
          1206647803780373360.0, -3599979517947607200.0,
          8037811822645051776.0, -12870931245150988800.0,
          13803759753640704000.0, -8752948036761600000.0,
          2432902008176640000.0]

P = Polynomial(reverse(coeffs))
P_roots = roots(P)

p(x) = (x - 1) * (x - 2) * (x - 3) * (x - 4) * (x - 5) * (x - 6) * (x - 7) * (x - 8) * (x - 9) * (x - 10) * (x - 11) * (x - 12) * (x - 13) * (x - 14) * (x - 15) * (x - 16) * (x - 17) * (x - 18) * (x - 19) * (x - 20)

# Create two figures
x_axis = 1:length(P_roots)
plot1 = plot(x_axis,
[[abs(P(P_roots[i])) for i in 1:length(P_roots)],
[abs(p(P_roots[i])) for i in 1:length(P_roots)]],
label=["P(x)" "p(x)"],
xlabel="x",
ylabel="y")

plot!(plot1)
savefig("images/abs_P_vs_p.png")
plot!()
plot2 = plot(1:length(P_roots), [abs(P_roots[i] - i) for i in 1:length(P_roots)], label="|zk - k|", xlabel="x", ylabel="y")

plot!(plot2)
savefig("images/abs_P_roots_vs_i.png")
