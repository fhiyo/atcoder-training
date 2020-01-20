// ref: https://atcoder.jp/contests/abc149/submissions/9557212
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

// XXX: なぜ1e6?
ll pow2_list[int(1e6)];

// treeなのでvisitedのフラグ管理がいらない
void dfs(int now, int back, vector<vector<int>>& g, vector<vector<int>>& c, vector<int>& s)
{
  s[now] = 1;
  for (int v: g[now]) {
    if (v == back) {
      continue;
    }
    dfs(v, now, g, c, s);
    s[now] += s[v];
    c[now].push_back(v);
  }
}


int main(int argc, char const* argv[])
{
  pow2_list[0] = 1;
  for (int i = 1; i < int(1e6); i++) {
    pow2_list[i] = pow2_list[i - 1] * 2 % mod;
  }

  int n;
  cin >> n;
  vector<vector<int>> graph(n);
  vector<vector<int>> child(n);  // 各ノードの子を格納する
  vector<int> size(n);  // 各ノードの子孫の数を表す

  REP(i, n-1) {
    int a, b;
    cin >> a >> b;
    a--, b--;
    graph[a].push_back(b);
    graph[b].push_back(a);
  }
  dfs(0, -1, graph, child, size);

  // print_vec(size);
  // print_2dvec(child);

  ll num_white_in_s = 0;
  // 各ノードについて見ていく
  // 各ノードを取り除いたときの話をしている？
  for (int i = 0; i < n; i++) {
    // 各ノードをtreeから取り除いたときにできる各部分木のサイズ
    vector<int> neighbor;
    int rest = n - 1;
    for (int v: child[i]) {
      neighbor.push_back(size[v]);
      rest -= size[v];
    }
    if (rest) neighbor.push_back(rest);

    // cout << "=== neighbor ===" << endl;
    // print_vec(neighbor);
    // cout << "=================" << endl;

    // 注目しているノードがsに含まれるパターンを余事象から求める
    // 初期値に注目しているノード以外が
    // 全部白のパターンの組み合わせ総数 (つまり1通り) を入れておく
    ll complement = 1;
    for (int v: neighbor) {
      // 注目してるsubtree (neighbor) だけに黒があり、
      // 他のsubtreeが全て白のパターンを足し合わせる
      complement = (complement + pow2_list[v] - 1) % mod;
    }
    // これは負の数になりうる？かもしれないので余りを取る前の数にmodを足す
    num_white_in_s = (num_white_in_s + pow2_list[n - 1] - complement + mod) % mod;
  }

  // treeの各nodeが取りうる状態の総数数が分母 (num_state)、
  // sに含まれるwhiteの数が分子 (num_white_in_s)。
  // 分母は2**nになる。なぜなら各ノードに対して白か黒かを選ぶから。
  ll num_state = pow2_list[n];
  cout << num_white_in_s * power(num_state, mod - 2, mod) % mod << endl;

  return 0;
}

