#include <iostream>

using namespace std;

long long floor(long long a, long long b)
{
  return a / b;
}


long long solve(long long n)
{
  if (n % 2 == 1) {
    return 0;
  }

  long long result = 0;
  long long tmp;

  long long divisor = 10;
  while (divisor <= n) {
    tmp = floor(n, divisor);
    // cout << "tmp: " << tmp << endl;
    result += tmp;
    divisor *= 5;
  }

  return result;
}


int main(int argc, char const* argv[])
{
  long long n;

  cin >> n;

  cout << solve(n) << endl;

  return 0;
}

