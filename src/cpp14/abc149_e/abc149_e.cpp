#include <iostream>
#include <bits/stdc++.h>
#include <algorithm>

using namespace std;

#define rep(i,n) for(int i=0;i<(n);i++)
#define ALL(a)  (a).begin(),(a).end()
using ll=long long;

template<typename T>
void print_vec(vector<T>& v)
{
  for(const auto& e: v) {
    cout << e << " ";
  }
  cout << endl;
}

template<typename T>
vector<T> cumsum(vector<T> v)
{
  vector<T> a(v.size() + 1, 0);
  for (int i = 0; i < v.size(); i++) {
    a[i + 1] = a[i] + v[i];
  }
  return a;
}


int main(int argc, char const* argv[])
{
  ll n, m, a;
  cin >> n >> m;

  vector<ll> as;
  for (int i = 0; i < n; i++) {
    cin >> a;
    as.push_back(a);
  }
  sort(ALL(as));

  // print_vec(as);

  vector<ll> num_greater_or_equal_power(2e5 + 1, 0);
  {
    int idx = 0;
    for (int i = 0; i < as.size(); i++) {
      while (idx <= as[i]) {
        num_greater_or_equal_power[idx] = as.size() - i;
        idx++;
      }
    }
  }

  // print_vec(num_greater_or_equal_power);

  // leftにはm通り以上の組み合わせをとれる最大の幸福度が入る
  // rightにはm通りよりも少ない組み合わせをとる最小の幸福度が入る
  ll left = 0, right = 2e5 + 1, mid, num_a, idx;
  while (right - left > 1) {
    num_a = 0;
    mid = left + (right - left) / 2;
    for (int i = 0; i < as.size(); i++) {
      idx = max((int)(mid - as[i]), 0);
      num_a += num_greater_or_equal_power[idx];
    }
    // 分かりづらいが、num_aが従う関数は単調減少
    if (num_a > m) left = mid;
    else right = mid;
  }

  // asの累積和を計算
  vector<ll> cum_sum_as = cumsum(as);

  ll ans = 0, sum_num_a = 0, num_greater_or_equal_power_idx;
  for (int i = 0; i < as.size(); i++) {
    num_greater_or_equal_power_idx = max((int)(right - as[i]), 0);
    num_a = num_greater_or_equal_power[num_greater_or_equal_power_idx];
    sum_num_a += num_a;
    ans += as[i] * num_a + *(cum_sum_as.end() - 1) - cum_sum_as[cum_sum_as.size() - 1 - num_a];
  }
  ans += left * (m - sum_num_a);

  cout << ans << endl;

  return 0;
}

// #include <iostream>
// #include <bits/stdc++.h>
// #include <algorithm>
//
// using namespace std;
//
// #define REP(i,n) for(int i=0;i<(n);i++)
// #define ALL(a)  (a).begin(),(a).end()
// using ll=long long;
//
//
// vector<ll> cumsum(vector<ll> v)
// {
//   int v_len = v.size();
//   vector<ll> s(v.size() + 1, 0);
//   for (int i = 0; i  < v_len; ++i) s[i + 1] = s[i] + v[i];
//
//   return s;
// }
//
//
// int main(int argc, char const* argv[])
// {
//   ll n, m;
//
//   cin >> n >> m;
//
//   vector<ll> a(n);
//
//   REP(i, n) {
//     cin >> a.at(i);
//   }
//
//   sort(ALL(a));
//   vector<ll> cum_sum = cumsum(a);
//
//   vector<int> num_greater_or_equal_x(2e5 + 1, 0);
//   {
//     int idx = 0;
//     for (int i = 0; i < a.size(); ++i) {
//       while (idx <= a[i]) {
//         num_greater_or_equal_x[idx] = a.size() - i;
//         idx++;
//       }
//     }
//   }
//
//   ll l = 0, r = 2e5 + 1, mid;
//
//   while(r - l > 1) {
//     mid = (r + l) / 2;
//     ll count = 0;
//     REP(i, n) {
//       // cout << "i: " << i << ", count: " << count << ", score: " << max((int)(mid - a[i]), 0) << endl;
//       count += num_greater_or_equal_x[max((int)(mid - a[i]), 0)];
//     }
//     if (count > m) {
//       l = mid;
//     } else {
//       r = mid;
//     }
//     // cout << "l: " << l << ", r: " << r << ", mid: " << mid << endl;
//   }
//   // cout << "l: " << l << ", r: " << r << ", mid: " << mid << endl;
//
//   ll num, sum_num = 0, ans = 0;
//   REP(i, n) {
//     num = num_greater_or_equal_x[max((int)(r - a[i]), 0)];
//     sum_num += num;
//     ans += a[i] * num + *(cum_sum.end() - 1) - cum_sum[cum_sum.size() - 1 - num];
//   }
//   ans += l * (m - sum_num);
//
//   cout << ans << endl;
//
//   return 0;
// }
