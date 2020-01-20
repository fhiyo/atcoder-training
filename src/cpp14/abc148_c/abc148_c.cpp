#include <iostream>

using namespace std;

int gcd(int a, int b)
{
  if (a < b) {
    // a = a ^ b;
    // b = a ^ b;
    // a = a ^ b;
    swap(a, b);
  }

  int tmp;
  while (b != 0) {
    tmp = a % b;
    a = b;
    b = tmp;
  }

  // cout << "in gcd: a: " << a << ", b: " << b << endl;

  return a;
}

long long lcm(int a, int b)
{
  return (long long) a * b / gcd(a, b);
}

int main(int argc, char const* argv[])
{
  int a, b;
  cin >> a >> b;

  // cout << a << ", " << b << endl;
  cout << lcm(a, b) << endl;

  return 0;
}

