#include <iostream>
#include <bits/stdc++.h>
#include <string>

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
  int n;
  cin >> n;

  map<string, int> m = {
    {"11", 0}, {"12", 0}, {"13", 0}, {"14", 0}, {"15", 0}, {"16", 0}, {"17", 0}, {"18", 0}, {"19", 0},
    {"21", 0}, {"22", 0}, {"23", 0}, {"24", 0}, {"25", 0}, {"26", 0}, {"27", 0}, {"28", 0}, {"29", 0},
    {"31", 0}, {"32", 0}, {"33", 0}, {"34", 0}, {"35", 0}, {"36", 0}, {"37", 0}, {"38", 0}, {"39", 0},
    {"41", 0}, {"42", 0}, {"43", 0}, {"44", 0}, {"45", 0}, {"46", 0}, {"47", 0}, {"48", 0}, {"49", 0},
    {"51", 0}, {"52", 0}, {"53", 0}, {"54", 0}, {"55", 0}, {"56", 0}, {"57", 0}, {"58", 0}, {"59", 0},
    {"61", 0}, {"62", 0}, {"63", 0}, {"64", 0}, {"65", 0}, {"66", 0}, {"67", 0}, {"68", 0}, {"69", 0},
    {"71", 0}, {"72", 0}, {"73", 0}, {"74", 0}, {"75", 0}, {"76", 0}, {"77", 0}, {"78", 0}, {"79", 0},
    {"81", 0}, {"82", 0}, {"83", 0}, {"84", 0}, {"85", 0}, {"86", 0}, {"87", 0}, {"88", 0}, {"89", 0},
    {"91", 0}, {"92", 0}, {"93", 0}, {"94", 0}, {"95", 0}, {"96", 0}, {"97", 0}, {"98", 0}, {"99", 0},
  };

  string s, s2;
  for (int i = 1; i <= n; i++) {
    s = to_string(i);
    s2 += s[0];
    s2 += s[s.length() - 1];
    // cout << m[s2] << endl;
    if (s[s.length() - 1] != '0') {
      m[s2]++;
    }
    s2 = "";
  }

  ll ans = 0;
  for (auto iter = m.begin(); iter != m.end(); iter++) {
    // cout << "key: " << iter->first << ", value: " << iter->second << endl;
    s = iter->first;
    reverse(ALL(s));
    ans += iter->second * m[s];
  }

  cout << ans << endl;

  return 0;
}
