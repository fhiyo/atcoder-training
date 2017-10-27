#include <iostream>
#include <cmath>
#include <vector>

using namespace std;

int main(int argc, char const* argv[])
{
  int N;
  cin >> N;
  vector<int> a_list(N);

  int sum = 0;
  for (size_t i = 0; i < N; i++) {
    cin >> a_list[i];
    sum += a_list[i];
  }

  int x = sum / N;

  int res1 = 0;
  int res2 = 0;

  for (size_t i = 0; i < N; i++) {
    res1 += (x - a_list[i]) * (x - a_list[i]);
    res2 += (x + 1 - a_list[i]) * (x + 1 - a_list[i]);
  }

  if (res1 <= res2) {
    cout << res1;
  } else {
    cout << res2;
  }
  cout << endl;

  return 0;
}
