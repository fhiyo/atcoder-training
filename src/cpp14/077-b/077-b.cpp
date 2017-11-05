#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  int N;
  cin >> N;

  for (int i = 0;; i++) {
    if (i * i > N) {
      cout << (i - 1) * (i - 1) << endl;
      break;
    }
  }

  return 0;
}
