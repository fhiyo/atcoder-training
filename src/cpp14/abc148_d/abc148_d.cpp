#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  int n;

  cin >> n;

  int o = 1;
  int a;

  for (int i = 0; i < n; i++) {
    cin >> a;
    if (o == a) o++;
  }

  cout << (o == 1 ? -1 : n - o + 1) << endl;

  return 0;
}
