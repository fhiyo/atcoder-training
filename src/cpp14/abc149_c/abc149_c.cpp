#include <iostream>
#include <bits/stdc++.h>
#include <cmath>

using namespace std;


bool is_prime(int x)
{
  // if (x <= 1) return false;
  for (int i = 2; i * i <= x; i++) {
    if (!(x % i)) return false;
  }
  return true;
}


int main(int argc, char const* argv[])
{
  int x;

  scanf("%i", &x);

  int ans;
  ans = x;

  while (!is_prime(ans)) ans++;

  printf("%i\n", ans);

  return 0;
}

