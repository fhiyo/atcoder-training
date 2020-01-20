#include <iostream>
#include <bits/stdc++.h>

using namespace std;

#define REP(i,n) for(int i=0;i<(n);i++)
#define ALL(a)  (a).begin(),(a).end()
using ll=long long;
const ll mod = 1e9 + 7;


template<class T>
T gcd(T a, T b)
{
  if(a && b) {
    return gcd(min(a, b), max(a, b) % min(a, b));
  } else {
    return a;
  }
}

template<class T>
T lcm(T a, T b)
{
  return a / gcd(a, b) * b;
}

ll power(ll base, ll exponent, ll module)
{
  if(exponent % 2) {
    return power(base, exponent - 1, module) * base % module;
  } else if(exponent) {
    ll root_ans = power(base, exponent / 2, module);
    return root_ans * root_ans % module;
  } else {
    return 1;
  }
}

struct combination
{
  vector<ll> fact, inv;
  combination(int sz) : fact(sz + 1), inv(sz + 1)
  {
    fact[0] = 1;
    for(int i = 1; i <= sz; i++) {
      fact[i] = fact[i - 1] * i % mod;
    }
    inv[sz] = power(fact[sz], mod - 2, mod);
    for(int i = sz - 1; i >= 0; i--) {
      inv[i] = inv[i + 1] * (i + 1) % mod;
    }
  }
  ll P(int n, int r)
  {
    if(r < 0 || n < r) return 0;
    return (fact[n] * inv[n - r] % mod);
  }
  ll C(int p, int q)
  {
    if(q < 0 || p < q) return 0;
    return (fact[p] * inv[q] % mod * inv[p - q] % mod);
  }
};

template<typename T>
void print_vec(vector<T>& v)
{
  for(const auto& e: v) {
    cout << e << " ";
  }
  cout << endl;
}
template<typename T>
void print_2dvec(vector<vector<T>>& m)
{
  for(const auto& v: m) {
    for(const auto& e: v) {
      cout << e << " ";
    }
    cout << endl;
  }
}


int main(int argc, char const* argv[])
{
  int n, m;

  cin >> n >> m;

  if (n == m) {
    cout << "Yes" << endl;
  } else {
    cout << "No" << endl;
  }

  return 0;
}
