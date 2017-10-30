#include <iostream>
#include <string>

using namespace std;

int main(int argc, char const* argv[])
{
  string s;
  cin >> s;

  bool found = false;
  if (s.length() < 2) {
    cout << "-1 -1";
  } else if (s.length() == 2) {
    if (s[0] == s[1]) {
      cout << "1 2";
    } else {
      cout << "-1 -1";
    }
  } else {
    for (size_t i = 0; i < s.length() - 2; i++) {
      if (s[i] == s[i+1]) {
        cout << i + 1 << " " << i + 2;
        found = true;
        break;
      } else if (s[i] == s[i+2]) {
        cout << i + 1 << " " << i + 3;
        found = true;
        break;
      }
    }
    if (!found) {
      cout << "-1 -1";
    }
  }

  cout << endl;
  return 0;
}
