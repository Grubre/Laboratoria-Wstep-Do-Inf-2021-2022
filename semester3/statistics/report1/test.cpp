#include <iostream>
#include <random>
#include <cstdint>
#include <functional>
#include <limits>
#include <fstream>

struct Point{
    Point() : x(0.0), y(0.0) {}
    Point(double x, double y) : x(x), y(y) {}
    double x;
    double y;
    friend std::ostream& operator<<(std::ostream& os, const Point& pt);
};


std::ostream& operator<<(std::ostream& os, const Point& pt)
{
    os << pt.x << ", " << pt.y;
    return os;
}


// double find_max_in_an_interval(std::function<double(double)> f, double a, double b)
// {
//     double ans = -INFINITY;
//     const double epsilon = (b-a) / 1000;
// }


void generate_random_points(std::vector<Point>& points, uint32_t n, double a, double b, double M)
{
    std::random_device rd; 
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dis_x(a, b);
    std::uniform_real_distribution<> dis_y(0.0, M);
    for(int i = 0; i < n; i++)
    {
        points[i] = {dis_x(gen), dis_y(gen)};
    }
}


uint32_t count_points_under_graph(std::vector<Point>& points, uint32_t n, std::function<double(double)> f, double a, double b)
{
    uint32_t C = 0;
    for(auto& i : points){
        if(i.y <= f(i.x))
            C++;
    }
    return C;
}


//TODO:
void approximate_integral_to_stream(const std::string& file_name, std::function<double(double)> f, double a, double b, double M)
{

}


int main()
{
    std::vector<Point> points(5000, {0,0});
    generate_random_points(points, 50, 0, 1, 5);
    return 0;
}
