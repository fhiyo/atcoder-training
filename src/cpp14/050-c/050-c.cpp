#include <cstdio>

using namespace std;

typedef long long ll;

inline ll read()
{
  char ch=getchar();
  ll f = 1, x = 0;
  while (ch < '0' || '9' < ch) {
    if (ch == '-') {f = -1;}
    else {ch = getchar();}
  }
  while ('0' <= ch && ch <= '9') {
    x = 10*x + (ch-'0');
    ch = getchar();
  }
  return f * x;
}

ll n;
ll as[100000];
ll mod = 1000000007;
ll ans = 1;

int main(int argc, char const* argv[])
{
  scanf("%lld", &n);
  for (ll i = 0; i < n; i++) {
    ++as[read()];
  }

  for (ll i = n-1; i > 0; i -= 2) {
    if (as[i] != 2) {
      printf("%d\n", 0);
      return 0;
    }
    (ans*=2)%=mod;
  }

  printf("%lld\n",ans);

  return 0;
}
