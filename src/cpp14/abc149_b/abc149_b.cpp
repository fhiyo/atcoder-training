ginclude <iostream>
#include <bits/stdc++.h>
#include <cstdlib>

using namespace std;

int main(int argc, char const* argv[])
{
  long long a, b, k;

  scanf("%lld  %lld  %lld", &a, &b, &k);

  if (a > k) {
    a -= k;
    printf("%lld %lld\n", a, b);
  } else {
    k -= a;
    printf("%lld %lld\n", 0, b - k > 0 ? b - k : 0);
  }

  return 0;
}
