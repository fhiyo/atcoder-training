#include <iostream>
#include <string>

using namespace std;

int main(int argc, char const* argv[])
{
  string W;
  cin >> W;
  string vowel("aiueo");

  for (char &c : W) {
    if (vowel.find(c) == string::npos) {
      cout << c;
    }
  }
  cout << endl;

  return 0;
}
