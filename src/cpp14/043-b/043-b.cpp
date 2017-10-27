#include <iostream>
#include <cstdio>
#include <string>

using namespace std;

int main(int argc, char const* argv[])
{
  string s;
  cin >> s;

  string res = "";

  for(char& c : s) {
    if (c == 'B') {
      res.pop_back();
    } else {
      res.push_back(c);
    }
  }

  printf("%s\n", res.c_str());
  return 0;
}
