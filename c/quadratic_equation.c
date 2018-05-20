#include <stdio.h>
#include <math.h>
#include <float.h>

// 检查一个浮点数是否为0
int is_zero(double x) {
  // 由于机器浮点数的表达会存在误差，
  // 判断其是否在一个极小区间之内。
  return x <= DBL_EPSILON && x >= -DBL_EPSILON;
}

void print_two_different_solutions(double a, double b) {
  printf("该方程有两个不同的实数解，分别是：%.2lf和%.2lf。\n", a, b);
}

void print_two_equal_solutions(double a) {
  printf("该方程有两个相同的实数解，为：%.2lf。\n", a);
}

void print_no_real_solutions() {
  printf("该方程没有实数解。\n");
}

void print_one_solution(double a) {
  printf("该方程有一个实数解，为：%.2lf。\n", a);
}

void print_no_solutions() {
  printf("该方程无解。\n");
}

void print_infinite_solutions() {
  printf("该方程有无穷多个解。\n");
}

int main(int argc, char* argv[])
{
  double a, b, c;
  printf("请输入a, b, c，用空格隔开：\n");
  scanf("%lf %lf %lf", &a, &b, &c);
  if (is_zero(a))  // 当a == 0时
    {
      if (is_zero(b))  // 当b == 0时
        {
          if (is_zero(c))
            {
              // 当c == 0时，有无穷多解
              print_infinite_solutions();
            }
          else
            {
              // 当c != 0时，无解
              print_no_solutions();
            }
        }
      else
        {
          // 当b != 0时，有一个实数解
          double solution = -c / b;
          print_one_solution(solution);
        }
    }
  else
    {
      // 当a != 0时，需要求判别式
      double delta = b * b - 4 * a * c;
      if (delta > 0) {
        // 当判别式>0时，有两个不同实数解
        double solution1 = (-b + sqrt(delta)) / (2*a), solution2 = (-b - sqrt(delta)) / (2*a);
        print_two_different_solutions(solution1, solution2);
      } else if (is_zero(delta)) {
        // 当判别式==0时，有两个相等的实数解
        double solution = (-b) / (2*a);
        print_two_equal_solutions(solution);
      } else {
        // 当判别式<0时，无实数解
        print_no_real_solutions();
      }
    }
}
