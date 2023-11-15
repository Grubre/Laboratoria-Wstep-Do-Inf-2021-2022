# Jakub Ogrodowczyk
using Plots

function f(x0, c, n)
    x = x0
    results = Float64[x0]  # Store the results in an array
    for i in 1:n
        x = x * x + c
        push!(results, x)
    end
    return results
end

n = 40
c_vals = [-2, -2, -2, -1, -1, -1, -1]
x_vals = [1, 2, 1.99999999999999, 1, -1, 0.75, 0.25]

# Iterate through each pair and create a separate plot
for i in 1:length(c_vals)
    c = c_vals[i]
    x0 = x_vals[i]

    # Create a plot for the graphical iteration
    plot(legend=false)

    # Plot y = x^2 + c
    x = -2:0.01:2
    y = x .^ 2 .+ c
    plot!(x, y, label="y = x^2 + c")

    # Plot y = x
    plot!(x, x, label="y = x")

    # Iterate and draw arrows
    x_n = x0
    for j in 1:n
        x_n1 = x_n * x_n + c
        plot!([x_n, x_n], [x_n, x_n1], arrow=true, linecolor="black")
        plot!([x_n, x_n1], [x_n1, x_n1], arrow=true, linecolor="black")
        x_n = x_n1
    end

    # Customize the plot
    xlabel!("x")
    ylabel!("y")
    title!("Iteracja graficzna x0 = $x0, c = $c")

    # Save the plot to a file with a specific filename
    savefig("./images/ex6_$i.png")

    # Clear the current plot for the next iteration
    plot!()
end
