#include <iostream>
#include <bits/stdc++.h>

using namespace std;


int main(int argc, char const* argv[])
{
  int n, k, r, s, p;
  string t;
  string hands;
  int ans = 0;

  scanf("%i %i", &n, &k);
  scanf("%i %i %i", &r, &s, &p);
  cin >> t;

  int iter = 0;
  for (; iter < k; iter++) {
    switch(t[iter]) {
      case 'r':
        hands += "p";
        ans += p;
        break;
      case 'p':
        hands += "s";
        ans += s;
        break;
      case 's':
        hands += "r";
        ans += r;
        break;
    }
  }

  for (; iter < n; iter++) {
    switch(t[iter]) {
      case 'r':
        if (hands[iter - k] != 'p') {
          hands += 'p';
          ans += p;
        } else {
          hands += 'x';
        }
        break;
      case 'p':
        if (hands[iter - k] != 's') {
          hands += 's';
          ans += s;
        } else {
          hands += 'x';
        }
        break;
      case 's':
        if (hands[iter - k] != 'r') {
          hands += 'r';
          ans += r;
        } else {
          hands += 'x';
        }
        break;
    }
  }

  printf("%i\n", ans);

  return 0;
}
